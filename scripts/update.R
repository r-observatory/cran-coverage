# Sharded, resumable runner. Mirrors cran-code-metrics/scripts/update.R.

#' Stable partition bucket (0 .. n-1) for a package, for matrix parallelism.
#'
#' A deterministic polynomial hash of the package name so that N runners
#' each take a disjoint, roughly-equal slice of the universe. Deterministic
#' across processes and R versions (base arithmetic only), so a package
#' always lands on the same runner and resume stays consistent.
#'
#' @param package Character vector of package names.
#' @param n       Number of partitions.
#' @return Integer vector of bucket indices in 0 .. n-1.
package_partition <- function(package, n) {
  vapply(package, function(p) {
    h <- 0L
    for (x in utf8ToInt(p)) h <- (h * 31L + x) %% 1000003L
    as.integer(h %% n)
  }, integer(1), USE.NAMES = FALSE)
}

#' Seed one runner's shard database from the canonical database's slice.
#'
#' Used at matrix cutover (no per-runner shard published yet) and to
#' self-heal a lost shard: copies just the rows whose package falls in this
#' runner's partition out of the canonical cran-coverage.db, so the runner
#' resumes instead of recomputing coverage the canonical already holds.
#'
#' @param src_path Canonical database to read from.
#' @param dst_path Shard database to create.
#' @param index    This runner's partition index.
#' @param count    Total number of partitions.
#' @return Invisibly, the number of coverage_summary rows written.
subset_partition <- function(src_path, dst_path, index, count) {
  if (summary_row_count(src_path) == 0L) return(invisible(0L))
  src <- DBI::dbConnect(RSQLite::SQLite(), src_path)
  on.exit(DBI::dbDisconnect(src), add = TRUE)
  tbls <- DBI::dbListTables(src)
  summ <- DBI::dbGetQuery(src, "SELECT * FROM coverage_summary")
  keep <- package_partition(summ$package, count) == index
  summ <- summ[keep, , drop = FALSE]
  pkgs <- summ$package
  grab <- function(tbl) {
    if (!tbl %in% tbls) return(NULL)
    d <- DBI::dbGetQuery(src, sprintf("SELECT * FROM %s", tbl))
    d[d$package %in% pkgs, , drop = FALSE]
  }
  filed <- grab("coverage_file")
  funcd <- grab("coverage_function")
  dst <- open_db(dst_path)
  on.exit(DBI::dbDisconnect(dst), add = TRUE)
  if (nrow(summ) > 0L) upsert_coverage(dst, summ, filed, funcd)
  invisible(nrow(summ))
}

#' Whether a coverage status is a transient failure worth re-attempting.
is_retryable <- function(status) status %in% RETRYABLE_STATUS

#' A build failure whose cause is transient dependency-resolution trouble (the
#' package manager reported a dependency "not available") rather than a genuine
#' build problem. Rolling-repo index outages burned a wave of these into
#' build_fail at the attempt cap; the dependencies are normally installable, so
#' such a failure stays retryable past the cap instead of perma-skipping a
#' package that would build fine once the index recovers (or under a stable
#' snapshot source).
is_transient_fail <- function(reason) {
  !is.na(reason) & grepl("(is|are) not available", reason)
}

#' Attempt count after recording an outcome: bumped on a retryable failure,
#' left unchanged on a terminal outcome. NA prior counts as 0.
next_attempts <- function(prior, status) {
  prior <- ifelse(is.na(prior), 0L, as.integer(prior))
  ifelse(is_retryable(status), prior + 1L, prior)
}

#' Read the checked-in popularity ranking (one package per line, most
#' downloaded first). Comment (#) and blank lines are ignored. Missing file
#' yields an empty ranking (pure alphabetical order).
popularity_rank <- function(path) {
  if (!file.exists(path)) return(character(0))
  lines <- trimws(readLines(path, warn = FALSE))
  lines[nzchar(lines) & !startsWith(lines, "#")]
}

#' Read the committed list of package versions to measure again.
#'
#' @param path TSV with columns package, version and reason.
#' @return data.frame(package, version, reason); zero rows when the file is absent.
read_requeue <- function(path) {
  if (!file.exists(path)) {
    return(data.frame(package = character(0), version = character(0),
                      reason = character(0), stringsAsFactors = FALSE))
  }
  q <- utils::read.delim(path, colClasses = "character", quote = "",
                         na.strings = character(0), comment.char = "")
  miss <- setdiff(c("package", "version", "reason"), names(q))
  if (length(miss)) stop("requeue list lacks column(s): ", paste(miss, collapse = " "))
  q <- q[c("package", "version", "reason")]
  blank <- !nzchar(trimws(q$package)) | !nzchar(trimws(q$version)) | !nzchar(trimws(q$reason))
  if (any(blank)) stop(sprintf("requeue list has %d row(s) with a blank field", sum(blank)))
  dup <- duplicated(paste(q$package, q$version, sep = "\x1f"))
  if (any(dup)) {
    stop(sprintf("requeue list names a package version more than once: %s",
                 paste(utils::head(paste(q$package[dup], q$version[dup]), 5), collapse = ", ")))
  }
  q
}

#' Where each queued row in this runner's partition stands.
#'
#' A row is waiting while its version is still the current CRAN version and
#' its raw covr object is not in the local store, which holds the shard's
#' bundles as fetched at the start of the run plus the objects written since.
#' A waiting row is due for a re-measure when it has a stored ok or test_error
#' row, its bundle was fetched whole or is not on the release yet, it has not
#' been tried this run and it has failed fewer than REQUEUE_MAX_ATTEMPTS times.
#' Without a record of the bundle fetch nothing is due, since nothing would be
#' uploaded.
#'
#' @param requeue  read_requeue() frame.
#' @param universe data.frame(package, latest_version).
#' @param state    analyzed_state() frame.
#' @param raw_dir  Local raw object store.
#' @param prior    fetch_prior_bundles() record, or NULL.
#' @param log      requeue_log() frame.
#' @param run_id   This run.
#' @param slice    NULL, or list(index, count).
#' @return The requeue rows in this partition with logical columns waiting,
#'   due and capped, and the integer column fails.
requeue_state <- function(requeue, universe, state, raw_dir, prior, log, run_id,
                          slice = NULL) {
  q <- requeue
  if (is.null(q)) q <- read_requeue(tempfile())
  if (!is.null(slice)) {
    q <- q[package_partition(q$package, slice$count) == slice$index, , drop = FALSE]
  }
  sep <- "\x1f"
  key <- paste(q$package, q$version, sep = sep)
  latest <- universe$latest_version[match(q$package, universe$package)]
  current <- !is.na(latest) & latest == q$version
  has_obj <- file.exists(raw_object_path(raw_dir, q$package, q$version))
  st <- state$covr_status[match(key, paste(state$package, state$version, sep = sep))]
  bundle <- raw_bundle_for(q$package, slice)
  bundle_ok <- if (is.null(prior)) rep(FALSE, nrow(q)) else
    !bundle %in% prior$unavailable &
      (!bundle %in% names(prior$expected) | bundle %in% names(prior$members))
  lkey <- paste(log$package, log$version, sep = sep)
  fails <- as.integer(table(factor(lkey[log$measured == 0L], levels = unique(key)))[key])
  fails[is.na(fails)] <- 0L
  tried <- key %in% lkey[log$run_id == run_id]
  q$fails   <- fails
  q$waiting <- current & !has_obj
  q$capped  <- q$waiting & fails >= REQUEUE_MAX_ATTEMPTS
  q$due     <- q$waiting & st %in% c("ok", "test_error") & bundle_ok & !tried & !q$capped
  rownames(q) <- NULL
  q
}

#' Choose the next batch of packages to process.
#'
#' A package is due when it has no row yet, its latest version differs from
#' the recorded one, or it failed transiently and is still under the retry
#' cap (see MAX_ATTEMPTS / RETRYABLE_STATUS). Never-analyzed and new-version
#' work is preferred over re-attempts, then higher download rank, then
#' alphabetical. With `slice`, only packages in this runner's partition are
#' considered, so matrix runners take disjoint work. Queued packages (see
#' requeue_state) come first, up to `requeue_budget` of them.
#'
#' @param universe data.frame(package, latest_version) of the whole CRAN.
#' @param state    analyzed_state() frame (package, version, covr_status,
#'   attempts); zero rows when nothing is analyzed yet.
#' @param size     Maximum number of packages to return.
#' @param rank     Character vector of package names, most popular first.
#' @param slice    NULL, or list(index, count) selecting one partition.
#' @param requeue  Names of packages whose current version is due for a
#'   queued re-measure.
#' @param requeue_budget How many queued packages may be returned.
#' @return Character vector of package names, in processing order.
select_shard <- function(universe, state, size, rank = character(0),
                         slice = NULL, requeue = character(0),
                         requeue_budget = 0L) {
  if (!is.null(slice)) {
    universe <- universe[
      package_partition(universe$package, slice$count) == slice$index, ,
      drop = FALSE]
  }
  if (nrow(universe) == 0L) return(character(0))

  # Match each package on its LATEST version's row specifically. A package
  # accumulates one row per version it is measured at; keying on the package
  # name alone picks whichever row comes first, which is a stale OLD-version row
  # once CRAN publishes a new version -- and since that old version never equals
  # the current latest, the package looks perpetually "new-version" and gets
  # re-selected every iteration even when its latest version is already done,
  # cycling a handful of popular packages and starving the never-analyzed
  # backlog. Looking up the (package, latest) row fixes that.
  sep  <- "\x1f"
  sidx <- stats::setNames(seq_len(nrow(state)),
                          paste(state$package, state$version, sep = sep))
  row  <- sidx[paste(universe$package, universe$latest_version, sep = sep)]

  a_sta <- state$covr_status[row]                     # NA where latest not recorded
  a_att <- state$attempts[row]; a_att[is.na(a_att)] <- 0L
  a_rsn <- if ("fail_reason" %in% names(state)) state$fail_reason[row] else
           rep(NA_character_, length(row))

  due_new <- is.na(row)                               # latest version not measured yet
  # A retryable failure is due while under the attempt cap; a transient
  # dependency-resolution failure stays due past the cap (see is_transient_fail)
  # so an index-outage wave does not permanently strand recoverable packages.
  retry   <- !due_new & a_sta %in% RETRYABLE_STATUS &
             (a_att < MAX_ATTEMPTS |
              (is_transient_fail(a_rsn) & a_att < TRANSIENT_MAX_ATTEMPTS))
  todo    <- due_new | retry

  pkg  <- universe$package[todo]
  akey <- ifelse(due_new[todo], 0L, a_att[todo])
  rk   <- match(pkg, rank); rk[is.na(rk)] <- .Machine$integer.max
  # Order by popularity rank first, so a popular retryable failure is tried at
  # its rank alongside new work (a fix such as sysreqs reaches it soon) rather
  # than waiting behind the whole never-attempted backlog. attempts breaks ties
  # within a rank (unranked packages: never-attempted before re-attempts).
  normal <- pkg[order(rk, akey, pkg)]

  queued <- setdiff(intersect(requeue, universe$package), normal)
  qrk <- match(queued, rank); qrk[is.na(qrk)] <- .Machine$integer.max
  queued <- utils::head(queued[order(qrk, queued)], max(0L, requeue_budget))
  utils::head(c(queued, normal), size)
}

#' Process one shard: measure each selected package version and store it.
#'
#' Queued rows (see requeue_state) are measured first, up to `requeue_budget`
#' per run, counted over every shard that shares `run_id`. A queued re-measure
#' that yields a raw covr object with status ok, or test_error over a stored
#' test_error row, replaces the stored rows and writes the object. Any other
#' outcome leaves the stored rows, their attempts included, as they were and
#' is logged as a failure instead.
#'
#' @param requeue read_requeue() frame, or NULL.
#' @param requeue_budget Queued rows to measure per run.
#' @param run_id  Identifies the run the shard belongs to.
#' @return The manifest list, also written to out_dir/manifest.json.
run_shard <- function(io, out_dir, shard_size = SHARD_SIZE,
                      rank = character(0), slice = NULL, requeue = NULL,
                      requeue_budget = REQUEUE_PER_RUN, run_id = "local") {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  db_path <- file.path(out_dir, DB_FILENAME)
  con <- open_db(db_path); on.exit(DBI::dbDisconnect(con), add = TRUE)
  raw_dir <- file.path(out_dir, "raw"); dir.create(raw_dir, showWarnings = FALSE)
  prior_path <- file.path(out_dir, "rawbundles", "prior-state.rds")
  prior <- if (file.exists(prior_path)) readRDS(prior_path) else NULL

  universe  <- io$package_list()
  state     <- analyzed_state(con)
  queue_at  <- function(state) {
    requeue_state(requeue, universe, state, raw_dir, prior, requeue_log(con),
                  run_id, slice)
  }
  budget_left <- function() {
    max(0L, as.integer(requeue_budget) - sum(requeue_log(con)$run_id == run_id))
  }
  queue     <- queue_at(state)
  due       <- queue$package[queue$due]
  # Key prior attempts on (package, version), not the package name alone. A
  # package accumulates one row per measured version, and the new-version row
  # is appended after the old one; looking up by name returns the FIRST (old)
  # row, so a still-failing new version reads the old row's attempts every pass
  # and its counter freezes -- it never reaches the cap and the loop re-runs it
  # forever. Match the same (package, version) key select_shard uses.
  prior_att <- stats::setNames(state$attempts,
                               paste(state$package, state$version, sep = "\x1f"))
  shard     <- select_shard(universe, state, shard_size, rank = rank, slice = slice,
                            requeue = due, requeue_budget = budget_left())

  n <- length(shard)
  lbl <- if (is.null(slice)) "" else sprintf("partition %d/%d ", slice$index, slice$count)
  message(sprintf("%s%d package(s) this shard: %s%s", lbl, n,
                  paste(utils::head(shard, 6), collapse = ", "),
                  if (n > 6L) ", ..." else ""))

  processed <- 0L
  for (pkg in shard) {
    v <- universe$latest_version[universe$package == pkg][1]
    queued <- pkg %in% due
    message(sprintf("[%d/%d] %s %s%s ...", processed + 1L, n, pkg, v,
                    if (queued) " (queued re-measure)" else ""))
    t0 <- proc.time()[["elapsed"]]
    wd <- tempfile(paste0("cov_", pkg, "_")); dir.create(wd)
    res <- tryCatch(io$run(pkg, v, wd),
      error = function(e) list(summary = data.frame(package = pkg, version = v,
        covr_status = "covr_error", line_pct = NA_real_,
        fail_reason = conditionMessage(e), stringsAsFactors = FALSE),
        file = NULL, func = NULL, raw = NULL))
    pa <- unname(prior_att[paste(pkg, v, sep = "\x1f")])
    if (length(pa) == 0L || is.na(pa)) pa <- 0L
    res$summary$attempts <- next_attempts(pa, res$summary$covr_status[1])
    st  <- res$summary$covr_status[1]
    note <- ""
    if (queued) {
      # Failing tests may not replace a stored row whose tests passed.
      was <- state$covr_status[state$package == pkg & state$version == v][1]
      measured <- !is.null(res$raw) && (identical(st, "ok") || identical(was, "test_error"))
      if (measured) {
        replace_coverage(con, res$summary, res$file, res$func)
        write_raw_object(raw_dir, pkg, v, res$raw)
      }
      log_requeue(con, run_id, pkg, v, st, measured,
                  if ("fail_reason" %in% names(res$summary)) res$summary$fail_reason[1] else NA)
      note <- if (measured) ", stored rows replaced" else ", stored rows kept"
    } else {
      upsert_coverage(con, res$summary, res$file, res$func)
      if (!is.null(res$raw)) write_raw_object(raw_dir, pkg, v, res$raw)
    }
    unlink(wd, recursive = TRUE, force = TRUE)
    lp  <- suppressWarnings(as.numeric(res$summary[["line_pct"]][1]))
    pct <- if (length(lp) == 1L && !is.na(lp)) sprintf(" %.1f%%", lp) else ""
    message(sprintf("    -> %s%s (%.0fs)%s", st, pct, proc.time()[["elapsed"]] - t0, note))
    processed <- processed + 1L
  }
  state_end <- analyzed_state(con)
  queue_end <- queue_at(state_end)
  normal_left <- length(select_shard(universe, state_end, .Machine$integer.max,
                                     rank = rank, slice = slice))
  manifest <- list(processed = processed, shard_size = n,
                   remaining = max(0L, normal_left +
                                   min(sum(queue_end$due), budget_left())))
  if (nrow(queue) > 0L) {
    log <- requeue_log(con)
    runs <- requeue_runs(con)
    this <- log$run_id == run_id
    counts <- list(
      queued = if (!is.null(runs) && run_id %in% runs$run_id)
        as.integer(runs$queued[runs$run_id == run_id][1]) else sum(queue$waiting),
      measured = sum(this & log$measured == 1L),
      failed = sum(this & log$measured == 0L),
      left = sum(queue_end$waiting),
      capped = sum(queue_end$capped))
    record_requeue_run(con, run_id, counts)
    manifest$requeue <- counts
    message(sprintf(paste("queued re-measures this run: %d queued at the start,",
                          "%d measured, %d failed, %d left (%d at the failure cap)"),
                    counts$queued, counts$measured, counts$failed, counts$left,
                    counts$capped))
  }
  message(sprintf("shard complete: %d processed, %d remaining%s", processed,
                  manifest$remaining,
                  if (is.null(slice)) "" else sprintf(" in partition %d", slice$index)))
  writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE),
             file.path(out_dir, "manifest.json"))
  manifest
}

default_io <- function() {
  list(
    package_list = function() {
      db <- available.packages(repos = "https://cloud.r-project.org")
      data.frame(package = rownames(db),
                 latest_version = as.character(db[, "Version"]),
                 stringsAsFactors = FALSE, row.names = NULL)
    },
    run = function(package, version, workdir) run_unit(package, version, workdir)
  )
}

# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------
if (identical(sys.nframe(), 0L)) {
  # Running standalone via Rscript. Source the sibling modules that define the
  # config constants and pipeline functions. The test harness sources these
  # itself, so this block only runs when update.R is invoked as a script.
  script_dir <- tryCatch(
    dirname(sys.frame(1)$ofile),
    error = function(e) {
      a <- commandArgs(FALSE)
      f <- sub("--file=", "", grep("--file=", a, value = TRUE))
      if (length(f) == 1L && nzchar(f)) dirname(f) else "scripts"
    }
  )
  for (s in c("config.R", "sources.R", "coverage.R", "export.R")) {
    source(file.path(script_dir, s))
  }
  args <- commandArgs(trailingOnly = TRUE)
  out_dir <- if (length(args) >= 1L) args[[1]] else "out"

  # Collect high-traffic packages first, and (under a matrix) take only this
  # runner's partition so N runners do disjoint work.
  repo_root <- dirname(normalizePath(script_dir))
  rank <- popularity_rank(file.path(repo_root, "data", "popularity.txt"))
  idx  <- suppressWarnings(as.integer(Sys.getenv("SHARD_INDEX", "")))
  cnt  <- suppressWarnings(as.integer(Sys.getenv("SHARD_COUNT", "")))
  slice <- if (!is.na(idx) && !is.na(cnt) && cnt > 1L)
    list(index = idx, count = cnt) else NULL

  # Package versions to measure again; each leaves the list once its raw
  # object is back in the shard's bundle.
  requeue <- read_requeue(file.path(repo_root, "data", "requeue", "requeue.tsv"))
  run_id <- Sys.getenv("GITHUB_RUN_ID", "local")
  if (!nzchar(run_id)) run_id <- "local"

  run_shard(default_io(), out_dir, rank = rank, slice = slice,
            requeue = requeue, run_id = run_id)
  message("Done.")
}
