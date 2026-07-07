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

#' Choose the next batch of packages to process.
#'
#' A package is due when it has no row yet, its latest version differs from
#' the recorded one, or it failed transiently and is still under the retry
#' cap (see MAX_ATTEMPTS / RETRYABLE_STATUS). Never-analyzed and new-version
#' work is preferred over re-attempts, then higher download rank, then
#' alphabetical. With `slice`, only packages in this runner's partition are
#' considered, so matrix runners take disjoint work.
#'
#' @param universe data.frame(package, latest_version) of the whole CRAN.
#' @param state    analyzed_state() frame (package, version, covr_status,
#'   attempts); zero rows when nothing is analyzed yet.
#' @param size     Maximum number of packages to return.
#' @param rank     Character vector of package names, most popular first.
#' @param slice    NULL, or list(index, count) selecting one partition.
#' @return Character vector of package names, in processing order.
select_shard <- function(universe, state, size, rank = character(0),
                         slice = NULL) {
  if (!is.null(slice)) {
    universe <- universe[
      package_partition(universe$package, slice$count) == slice$index, ,
      drop = FALSE]
  }
  if (nrow(universe) == 0L) return(character(0))

  ver <- stats::setNames(state$version, state$package)
  sta <- stats::setNames(state$covr_status, state$package)
  att <- stats::setNames(state$attempts, state$package)

  pkg    <- universe$package
  latest <- universe$latest_version
  a_ver  <- ver[pkg]
  a_sta  <- sta[pkg]
  a_att  <- att[pkg]; a_att[is.na(a_att)] <- 0L

  never  <- is.na(a_ver)
  newver <- !never & a_ver != latest
  retry  <- !never & a_ver == latest &
            a_sta %in% RETRYABLE_STATUS & a_att < MAX_ATTEMPTS
  todo   <- never | newver | retry
  if (!any(todo)) return(character(0))

  pkg  <- pkg[todo]
  akey <- ifelse(never[todo] | newver[todo], 0L, a_att[todo])
  rk   <- match(pkg, rank); rk[is.na(rk)] <- .Machine$integer.max
  # Order by popularity rank first, so a popular retryable failure is tried at
  # its rank alongside new work (a fix such as sysreqs reaches it soon) rather
  # than waiting behind the whole never-attempted backlog. attempts breaks ties
  # within a rank (unranked packages: never-attempted before re-attempts).
  utils::head(pkg[order(rk, akey, pkg)], size)
}

run_shard <- function(io, out_dir, shard_size = SHARD_SIZE,
                      rank = character(0), slice = NULL) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  db_path <- file.path(out_dir, DB_FILENAME)
  con <- open_db(db_path); on.exit(DBI::dbDisconnect(con), add = TRUE)
  raw_dir <- file.path(out_dir, "raw"); dir.create(raw_dir, showWarnings = FALSE)

  universe  <- io$package_list()
  state     <- analyzed_state(con)
  prior_att <- stats::setNames(state$attempts, state$package)
  shard     <- select_shard(universe, state, shard_size, rank = rank, slice = slice)

  n <- length(shard)
  lbl <- if (is.null(slice)) "" else sprintf("partition %d/%d ", slice$index, slice$count)
  message(sprintf("%s%d package(s) this shard: %s%s", lbl, n,
                  paste(utils::head(shard, 6), collapse = ", "),
                  if (n > 6L) ", ..." else ""))

  processed <- 0L
  for (pkg in shard) {
    v <- universe$latest_version[universe$package == pkg][1]
    message(sprintf("[%d/%d] %s %s ...", processed + 1L, n, pkg, v))
    t0 <- proc.time()[["elapsed"]]
    wd <- tempfile(paste0("cov_", pkg, "_")); dir.create(wd)
    res <- tryCatch(io$run(pkg, v, wd),
      error = function(e) list(summary = data.frame(package = pkg, version = v,
        covr_status = "covr_error", line_pct = NA_real_,
        fail_reason = conditionMessage(e), stringsAsFactors = FALSE),
        file = NULL, func = NULL, raw = NULL))
    pa <- unname(prior_att[pkg]); if (length(pa) == 0L || is.na(pa)) pa <- 0L
    res$summary$attempts <- next_attempts(pa, res$summary$covr_status[1])
    upsert_coverage(con, res$summary, res$file, res$func)
    if (!is.null(res$raw)) write_raw_object(raw_dir, pkg, v, res$raw)
    unlink(wd, recursive = TRUE, force = TRUE)
    st  <- res$summary$covr_status[1]
    lp  <- suppressWarnings(as.numeric(res$summary[["line_pct"]][1]))
    pct <- if (length(lp) == 1L && !is.na(lp)) sprintf(" %.1f%%", lp) else ""
    message(sprintf("    -> %s%s (%.0fs)", st, pct, proc.time()[["elapsed"]] - t0))
    processed <- processed + 1L
  }
  manifest <- list(processed = processed, shard_size = n,
                   remaining = max(0L, length(select_shard(universe,
                     analyzed_state(con), .Machine$integer.max,
                     rank = rank, slice = slice))))
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

  run_shard(default_io(), out_dir, rank = rank, slice = slice)
  message("Done.")
}
