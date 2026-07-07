# Run covr under a fixed deterministic environment and extract metrics.

COVR_STATUS <- c("ok", "no_tests", "build_fail", "sysreq_fail",
                 "test_error", "timeout", "covr_error")

# Repository root (absolute), captured the moment this file is sourced. By
# convention every entry point in this pipeline (tests/testthat.R, scripts/
# update.R) sources scripts/*.R with relative paths from the repo root, so
# the working directory is the repo root at that moment. run_unit_dir uses
# this to tell its callr subprocess (see .covr_subprocess) where to find
# config.R/sources.R/coverage.R, since the working directory when
# run_unit_dir actually runs is not reliable: testthat::test_dir() changes
# it to each test file's own directory while that file's tests execute.
.PIPELINE_ROOT <- normalizePath(getwd())

#' Apply the deterministic environment. Idempotent. Returns fingerprint fields.
apply_determinism <- function(seed = COVERAGE_SEED) {
  Sys.setenv(
    LC_ALL = "en_US.UTF-8", LANG = "en_US.UTF-8", TZ = "UTC",
    NOT_CRAN = "true", OMP_NUM_THREADS = "1", R_COVR = "true",
    TESTTHAT_CPUS = "1", `_R_CHECK_LIMIT_CORES_` = "true"
  )
  options(mc.cores = 1L, Ncpus = 1L, covr.gcov = Sys.which("gcov"))
  suppressWarnings(try(Sys.setlocale("LC_ALL", "en_US.UTF-8"), silent = TRUE))
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  coverage_fingerprint(seed)
}

coverage_fingerprint <- function(seed = COVERAGE_SEED) {
  list(
    r_version   = as.character(getRversion()),
    covr_version = tryCatch(as.character(utils::packageVersion("covr")),
                            error = function(e) NA_character_),
    gcov_version = tryCatch(system2(Sys.which("gcov"), "--version",
                            stdout = TRUE)[1], error = function(e) NA_character_),
    locale = Sys.getlocale("LC_ALL"),
    seed   = seed
  )
}

.is_compiled_file <- function(filename) {
  grepl("^src/", filename) |
    grepl("\\.(c|cc|cpp|cxx|h|hpp|f|f90|f95)$", filename, ignore.case = TRUE)
}

.cov_df <- function(cov) {
  df <- as.data.frame(cov)
  df$covered <- df$value > 0
  df$is_compiled <- .is_compiled_file(df$filename)
  df
}

#' Per-physical-line tally for the R portion of a coverage object: one row
#' per line (not per traced expression), via covr::tally_coverage(by =
#' "line"), with the compiled (src/*.c etc.) files filtered out.
.r_line_tally <- function(cov) {
  tl <- covr::tally_coverage(cov, by = "line")
  tl[!.is_compiled_file(tl$filename), , drop = FALSE]
}

#' One-row summary. True line counts (from a per-line tally), true expression
#' counts (from the traced-expression data frame), line/expression
#' percentages, and the compiled split.
summarise_coverage <- function(cov) {
  df <- .cov_df(cov)
  r  <- df[!df$is_compiled, , drop = FALSE]
  cc <- df[df$is_compiled, , drop = FALSE]
  pct <- function(x) if (nrow(x) == 0L) NA_real_ else round(100 * mean(x$covered), 4)

  tl <- .r_line_tally(cov)
  lines_covered <- sum(tl$value > 0)

  data.frame(
    line_pct               = tryCatch(round(covr::percent_coverage(cov, by = "line"), 4),
                                      error = function(e) pct(df)),
    expr_pct               = tryCatch(round(covr::percent_coverage(cov, by = "expression"), 4),
                                      error = function(e) pct(df)),
    lines_total            = nrow(tl),
    lines_covered          = lines_covered,
    lines_missed           = nrow(tl) - lines_covered,
    expr_total             = nrow(r),
    expr_covered           = sum(r$covered),
    expr_missed            = nrow(r) - sum(r$covered),
    compiled_line_pct      = pct(cc),
    compiled_lines_total   = nrow(cc),
    compiled_lines_covered = sum(cc$covered),
    stringsAsFactors = FALSE
  )
}

#' Per-file coverage. True physical line counts (see .r_line_tally): a line
#' with any covered expression on it counts as covered once, not once per
#' traced expression that falls on that line.
file_coverage <- function(cov) {
  tl <- .r_line_tally(cov)
  if (nrow(tl) == 0L) {
    return(data.frame(file = character(), lines_covered = integer(),
                      lines_total = integer(), coverage_pct = numeric(),
                      stringsAsFactors = FALSE))
  }
  agg <- stats::aggregate(value ~ filename, tl, function(x) c(sum(x > 0), length(x)))
  data.frame(
    file          = agg$filename,
    lines_covered = agg$value[, 1],
    lines_total   = agg$value[, 2],
    coverage_pct  = round(100 * agg$value[, 1] / agg$value[, 2], 4),
    stringsAsFactors = FALSE
  )
}

#' Per-function coverage. Joined on (file, label) using the `functions`
#' column from the same per-line tally (see .r_line_tally), so line counts
#' here agree with file_coverage and summarise_coverage. covr labels S4
#' methods by signature and R6/RC methods by bare name; we keep the label
#' verbatim and also carry the file so downstream reconciliation with the
#' analyzer is possible, and we never assume a 1:1 name match.
function_coverage <- function(cov) {
  tl <- .r_line_tally(cov)
  tl <- tl[!is.na(tl$functions) & nzchar(tl$functions), , drop = FALSE]
  if (nrow(tl) == 0L) {
    return(data.frame(file = character(), label = character(),
                      lines_total = integer(), lines_covered = integer(),
                      coverage_pct = numeric(), stringsAsFactors = FALSE))
  }
  key <- paste(tl$filename, tl$functions, sep = "\x1f")
  agg <- stats::aggregate(value ~ key, tl, function(x) c(sum(x > 0), length(x)))
  parts <- do.call(rbind, strsplit(agg$key, "\x1f", fixed = TRUE))
  data.frame(
    file          = parts[, 1],
    label         = parts[, 2],
    lines_covered = agg$value[, 1],
    lines_total   = agg$value[, 2],
    coverage_pct  = round(100 * agg$value[, 1] / agg$value[, 2], 4),
    stringsAsFactors = FALSE
  )
}

#' Prepend package/version identifier columns to a per-file or per-function
#' coverage table, staying correct when the table has zero rows.
#'
#' `cbind(package = , version = , df)` dispatches to `cbind.data.frame`, whose
#' body is `data.frame(..., check.names = FALSE)`; mixing a length-1 scalar
#' with a 0-row frame raises "arguments imply differing number of rows: 1, 0".
#' An empty coverage object (a package whose tests all skip, so covr traced no
#' lines -- e.g. httr when httpbin is unreachable) makes file_coverage() and
#' function_coverage() return 0 rows, which crashed the old cbind.
.tag_pv <- function(df, package, version) {
  data.frame(package = rep(package, nrow(df)),
             version = rep(version, nrow(df)),
             df, stringsAsFactors = FALSE, check.names = FALSE)
}

# Per-package unit runner: install, run covr, classify the outcome.

.has_tests <- function(pkgdir) {
  tt <- file.path(pkgdir, "tests")
  dir.exists(tt) && length(list.files(tt, recursive = TRUE, pattern = "\\.[Rr]$")) > 0L
}

#' Install a package's dependencies best-effort (r2u binaries in production).
#' Returns the character vector of Suggests that failed to install.
.install_deps <- function(pkgdir) {
  desc <- read.dcf(file.path(pkgdir, "DESCRIPTION"))
  fld  <- function(f) if (f %in% colnames(desc)) desc[1, f] else ""
  parse_deps <- function(s) {
    x <- trimws(unlist(strsplit(s, ",")))
    x <- sub("\\s*\\(.*", "", x)
    setdiff(x[nzchar(x)], c("R", rownames(installed.packages())))
  }
  hard <- unique(c(parse_deps(fld("Depends")), parse_deps(fld("Imports")),
                   parse_deps(fld("LinkingTo"))))
  soft <- parse_deps(fld("Suggests"))
  if (length(hard)) suppressWarnings(install.packages(hard, quiet = TRUE))
  failed_soft <- character(0)
  for (s in soft) {
    suppressWarnings(install.packages(s, quiet = TRUE))
    if (!s %in% rownames(installed.packages())) failed_soft <- c(failed_soft, s)
  }
  failed_soft
}

#' Whether a covr error message is a build/install failure (persistent, the
#' target would not compile/install) rather than a covr-internal error. covr's
#' install failure raises the literal "Package installation did not succeed.",
#' which is bucketed as build_fail so the honest breakdown is accurate.
.is_build_failure <- function(msg) {
  grepl("compilation failed|non-zero exit|installation did not succeed",
        msg, ignore.case = TRUE)
}

#' On a build failure, run a plain R CMD INSTALL of the target to capture the
#' actual compiler/parser/dependency error, so the recorded reason is
#' actionable instead of covr's generic "Package installation did not
#' succeed." The target's dependencies were already installed by covr before
#' it failed. Bounded by `timeout` where available; best-effort, returning ""
#' on any problem so it can never turn a clean failure into an error.
.capture_install_error <- function(pkgdir, timeout_s = 300L) {
  lib <- tempfile("diaglib_"); dir.create(lib, showWarnings = FALSE)
  on.exit(unlink(lib, recursive = TRUE, force = TRUE), add = TRUE)
  args <- c("CMD", "INSTALL", "--no-test-load", "-l", lib, pkgdir)
  out <- tryCatch(suppressWarnings(
    if (nzchar(Sys.which("timeout")))
      system2("timeout", c(as.character(timeout_s), "R", args),
              stdout = TRUE, stderr = TRUE)
    else
      system2("R", args, stdout = TRUE, stderr = TRUE)),
    error = function(e) conditionMessage(e))
  if (!length(out)) return("")
  hits <- grep("error|cannot|not found|unable|fatal|undefined|no package|unexpected|failed",
               out, ignore.case = TRUE, value = TRUE)
  paste(utils::tail(if (length(hits)) hits else out, 3L), collapse = " | ")
}

# System requirements: covr recompiles the TARGET from source under
# instrumentation, so it needs the target's build-time system libraries
# (-dev headers). Dependencies stay as r2u binaries and already pull their
# runtime libs, so only the target's own SystemRequirements must be resolved.

# Session cache of apt packages already installed this run, so a system library
# a prior package pulled in is not re-shelled through apt-get.
.SYSREQS_DONE <- new.env(parent = emptyenv())

#' Resolve a package's SystemRequirements text to Ubuntu apt package names.
#'
#' Uses pkgdepends' bundled, offline system-requirements database (the same
#' mapping pak uses). Falls back to the live Posit PPM sysreqs API ONLY when
#' the package actually declares SystemRequirements the offline DB did not
#' match, so the ~half of CRAN that declares none never touches the network.
#' Returns character(0) for empty/unresolvable input.
.resolve_sysreqs_apt <- function(sysreqs_text, pkgname = NULL,
                                 platform = SYSREQS_PLATFORM) {
  has_text <- !is.null(sysreqs_text) && !is.na(sysreqs_text) &&
              nzchar(trimws(sysreqs_text))
  apt <- character(0)
  if (has_text && requireNamespace("pkgdepends", quietly = TRUE)) {
    m <- tryCatch(
      pkgdepends::sysreqs_db_match(sysreqs_text, sysreqs_platform = platform),
      error = function(e) NULL)
    if (!is.null(m) && length(m) && !is.null(m[[1]]) && nrow(m[[1]]))
      apt <- unique(unlist(m[[1]]$packages))
  }
  # Network fallback only for packages that DECLARE sysreqs but did not match.
  if (length(apt) == 0L && has_text && !is.null(pkgname) && nzchar(pkgname)) {
    p   <- strsplit(platform, "-", fixed = TRUE)[[1]]
    url <- sprintf(paste0("https://packagemanager.posit.co/__api__/repos/1/",
                          "sysreqs?all=false&pkgname=%s&distribution=%s&release=%s"),
                   utils::URLencode(pkgname), p[1], p[2])
    js  <- tryCatch(jsonlite::fromJSON(url, simplifyVector = FALSE),
                    error = function(e) NULL)
    if (length(js$requirements))
      apt <- unique(unlist(lapply(js$requirements,
                                  function(r) unlist(r$requirements$packages))))
  }
  apt[nzchar(apt)]
}

#' Install the target package's system requirements as apt -dev packages,
#' before covr recompiles it from source. Best-effort: a resolution or apt
#' failure never aborts, so a genuinely missing library is still recorded as
#' build_fail by covr exactly as before. Returns (invisibly) the apt packages
#' it attempted.
install_sysreqs <- function(pkgdir) {
  desc <- tryCatch(read.dcf(file.path(pkgdir, "DESCRIPTION")),
                   error = function(e) NULL)
  if (is.null(desc)) return(invisible(character(0)))
  g   <- function(f) if (f %in% colnames(desc)) desc[1, f] else ""
  sr  <- g("SystemRequirements")
  apt <- .resolve_sysreqs_apt(sr, pkgname = g("Package"))
  todo <- setdiff(apt, ls(.SYSREQS_DONE))
  if (length(todo) == 0L) {
    if (nzchar(trimws(sr)) && length(apt) == 0L)
      message(sprintf("    sysreqs: '%s' resolved to no apt packages", trimws(sr)))
    return(invisible(character(0)))
  }
  # Runs in the parent process (outside the covr timeout), so bound it with
  # `timeout` in case an apt mirror stalls; a hang here would otherwise stall
  # the whole shard. The exit code is logged so a failed install is visible.
  message(sprintf("    sysreqs: apt-get install %s", paste(todo, collapse = " ")))
  code <- tryCatch(suppressWarnings(system2(
      "timeout", c("300", "apt-get", "install", "-y", "--no-install-recommends", todo),
      stdout = FALSE, stderr = FALSE, env = "DEBIAN_FRONTEND=noninteractive")),
    error = function(e) -1L)
  message(sprintf("    sysreqs: apt-get exit %s", code))
  if (identical(as.integer(code), 0L)) for (a in todo) assign(a, TRUE, envir = .SYSREQS_DONE)
  invisible(todo)
}

#' covr's package_coverage() installs the package with tracing hooks and then
#' runs `tools::testInstalledPackage(..., types = "tests")`. When a test file
#' fails or errors, that returns a non-zero result and covr calls its internal
#' `show_failures()`, which raises a condition classed "covr_error" whose
#' message is the tail of the failing Rout file. That is a genuine R error
#' (caught by tryCatch(error =)), not a warning: a package with failing tests
#' does NOT come back from package_coverage() as a normal return value, so
#' treating "no R error" as "tests passed" silently misreports failing suites
#' as ok. Confirmed empirically with a fixture package whose test asserts a
#' wrong value: package_coverage() throws, and
#' `inherits(e, "covr_error")` is TRUE, versus a plain "simpleError" for a
#' genuine build/install failure (for example a source file that fails to
#' parse).
#'
#' The coverage trace files that back the line/function counts are written
#' to `install_path` before `show_failures()` is reached, so if
#' package_coverage() is called with `clean = FALSE` and an install_path we
#' control, those trace files are still on disk after the error propagates.
#' `.recover_partial_coverage()` rebuilds a coverage object from them and
#' immediately derives every summary (summary row, file table, function
#' table, raw serialization) from that same object, all inside one tryCatch.
#' Reconstruction and summarization are kept atomic on purpose: if a future
#' covr version keeps these internal function names but changes the shape of
#' the object they return, we want a single NULL result (which the caller
#' turns into an explicit NA), not a reconstruction that "succeeds" and then
#' an uncaught error out of summarise_coverage/file_coverage/function_coverage
#' further down.
.recover_partial_coverage <- function(pkgdir, install_path, package, version) {
  tryCatch({
    trace_files <- list.files(install_path, pattern = "^covr_trace_",
                              full.names = TRUE, recursive = TRUE)
    if (length(trace_files) == 0L) return(NULL)
    merged <- covr:::merge_coverage(trace_files)
    pkg    <- covr:::as_package(pkgdir)
    cov    <- covr:::as_coverage(merged, package = pkg, root = pkgdir)
    cov    <- covr:::exclude(cov, line_exclusions = c("src/RcppExports.cpp",
                    "R/RcppExports.R", "src/cpp11.cpp", "R/cpp11.R"),
                   path = pkgdir)
    list(
      summary = summarise_coverage(cov),
      file    = .tag_pv(file_coverage(cov), package, version),
      func    = .tag_pv(function_coverage(cov), package, version),
      raw     = serialize(cov, connection = NULL)
    )
  }, error = function(e) NULL)
}

#' Defensive secondary signal: if some covr/testthat combination ever reports
#' a failed test file as a warning rather than an error (not observed in the
#' r2u environment this pipeline targets, but cheap to guard against), treat
#' it the same way as the error path.
.warn_is_test_failure <- function(msgs) {
  length(msgs) > 0 &&
    any(grepl("test.*fail|fail.*test|FAIL\\s+[1-9]", msgs, ignore.case = TRUE))
}

#' Executed inside a fresh `callr::r()` subprocess (see `run_unit_dir`), so
#' covr's own shelled-out work (`tools::testInstalledPackage()`, `R CMD
#' INSTALL`, `R CMD build` -- all driven via `system()`/`system2()`) can be
#' killed at the OS-process level if it hangs. `R.utils::withTimeout()` /
#' `setTimeLimit()` cannot do that: they only interrupt R's own evaluator,
#' which never regains control while a child process is blocked in
#' `system()`/`system2()`, so a genuinely hung package would never time out
#' and would deadlock the whole shard. Confirmed empirically: a fixture
#' whose test sleeps well past the configured timeout only ever returned
#' once its own sleep finished, not at the timeout. `callr::r(timeout = )`
#' kills the subprocess and its children instead.
#'
#' A fresh subprocess starts from an empty session, so this function must
#' be self-contained: the first thing it does is re-source the pipeline
#' scripts it needs, rather than relying on closures over the parent
#' process, which callr cannot transport.
#'
#' `mode = "unit"` installs dependencies and runs the tests-type coverage
#' pass, classifying the result exactly as before (build_fail / covr_error
#' / test_error with recovered partial coverage / ok), and returns a plain
#' list that `run_unit_dir` turns into its summary/file/func/raw result
#' without touching covr internals itself.
#'
#' `mode = "type_pct"` runs one by-type (examples/vignettes) coverage pass
#' and returns just its line percentage. It is its own subprocess call,
#' bounded by the same `timeout_s` as the tests pass but run separately, so
#' a hang while building vignettes (say) cannot erase an already-earned
#' "ok" tests result the way folding every type into one subprocess call
#' would.
.covr_subprocess <- function(mode, repo_root, pkgdir,
                            package = NULL, version = NULL, type = NULL) {
  setwd(repo_root)
  source("scripts/config.R")
  source("scripts/sources.R")
  source("scripts/coverage.R")
  apply_determinism()

  if (identical(mode, "type_pct")) {
    return(tryCatch(
      round(covr::percent_coverage(
        covr::package_coverage(pkgdir, type = type, quiet = TRUE), by = "line"), 4),
      error = function(e) NA_real_))
  }

  failed_soft <- tryCatch(.install_deps(pkgdir), error = function(e) NULL)

  # clean = FALSE and an install_path we control let us recover coverage
  # traces after a test-failure error (see .recover_partial_coverage above).
  # We take over the cleanup covr would otherwise have done itself.
  install_path <- tempfile("covr_lib_")
  dir.create(install_path, showWarnings = FALSE)
  on.exit({
    unlink(install_path, recursive = TRUE, force = TRUE)
    try(covr:::clean_objects(pkgdir), silent = TRUE)
    try(covr:::clean_gcov(pkgdir), silent = TRUE)
    try(covr:::clean_parse_data(), silent = TRUE)
  }, add = TRUE)

  test_warnings <- character(0)
  cov <- tryCatch(
    withCallingHandlers(
      covr::package_coverage(pkgdir, type = "tests", quiet = TRUE,
                             clean = FALSE, install_path = install_path),
      warning = function(w) {
        test_warnings <<- c(test_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) structure(
      list(msg = conditionMessage(e), is_test_failure = inherits(e, "covr_error")),
      class = "cov_err")
  )

  if (inherits(cov, "cov_err")) {
    if (isTRUE(cov$is_test_failure)) {
      recovered <- .recover_partial_coverage(pkgdir, install_path, package, version)
      return(list(status = "test_error", fail_reason = substr(cov$msg, 1, 300),
                 summary = recovered$summary, file = recovered$file,
                 func = recovered$func, raw = recovered$raw))
    }
    if (.is_build_failure(cov$msg)) {
      detail <- tryCatch(.capture_install_error(pkgdir), error = function(e) "")
      reason <- if (nzchar(detail)) paste0(cov$msg, " ", detail) else cov$msg
      return(list(status = "build_fail", fail_reason = substr(reason, 1, 300)))
    }
    return(list(status = "covr_error", fail_reason = substr(cov$msg, 1, 300)))
  }

  s   <- summarise_coverage(cov)
  fcv <- .tag_pv(file_coverage(cov), package, version)
  fnv <- .tag_pv(function_coverage(cov), package, version)
  raw <- serialize(cov, connection = NULL)

  if (.warn_is_test_failure(test_warnings)) {
    bad <- test_warnings[grepl("test.*fail|fail.*test|FAIL\\s+[1-9]",
                               test_warnings, ignore.case = TRUE)]
    return(list(status = "test_error", fail_reason = substr(bad[1], 1, 300),
               summary = s, file = fcv, func = fnv, raw = raw))
  }

  list(status = "ok", summary = s, file = fcv, func = fnv, raw = raw,
      suggests_failed = if (length(failed_soft)) paste(failed_soft, collapse = ",")
                        else NA_character_)
}

#' Run covr over an already-extracted package directory. The covr work
#' happens in a `callr::r()` subprocess (see `.covr_subprocess`) bounded by
#' `timeout_s`; if it is not back within that budget, callr kills the
#' subprocess and any children it spawned, and this is recorded as
#' covr_status = "timeout" instead of hanging the whole shard forever.
#' Defaults to the pipeline-wide PER_UNIT_TIMEOUT_S but can be overridden,
#' for example in tests.
run_unit_dir <- function(package, version, pkgdir, timeout_s = PER_UNIT_TIMEOUT_S) {
  fp <- apply_determinism()
  t0 <- proc.time()[["elapsed"]]
  base <- data.frame(
    package = package, version = version,
    r_version = fp$r_version, covr_version = fp$covr_version,
    gcov_version = fp$gcov_version, locale = fp$locale, seed = fp$seed,
    stringsAsFactors = FALSE
  )
  finish <- function(status, tests_passed = FALSE, extra = list()) {
    s <- base
    s$covr_status <- status
    s$tests_passed <- tests_passed
    for (n in names(extra)) s[[n]] <- extra[[n]]
    s$run_seconds <- round(proc.time()[["elapsed"]] - t0, 1)
    for (col in c("line_pct","expr_pct","compiled_line_pct",
                  "coverage_tests_pct","coverage_examples_pct","coverage_vignettes_pct"))
      if (is.null(s[[col]])) s[[col]] <- NA_real_
    list(summary = s, file = NULL, func = NULL, raw = NULL)
  }

  if (!.has_tests(pkgdir)) return(finish("no_tests"))

  # Pre-seed the target's build-time system libraries (apt -dev packages) so
  # covr can recompile it from source. Done here in the parent process, before
  # the timed covr subprocess, so apt time is not charged against the per-unit
  # timeout and a retried subprocess does not re-install. Best-effort.
  try(install_sysreqs(pkgdir), silent = TRUE)

  repo_root <- .PIPELINE_ROOT
  wr <- tryCatch(
    callr::r(.covr_subprocess,
            args = list(mode = "unit", repo_root = repo_root, pkgdir = pkgdir,
                        package = package, version = version),
            timeout = timeout_s),
    error = function(e) e
  )

  if (inherits(wr, "callr_timeout_error")) {
    return(finish("timeout", tests_passed = FALSE,
                  extra = list(fail_reason = sprintf(
                    "covr subprocess exceeded the %ds timeout and was terminated",
                    timeout_s))))
  }
  if (inherits(wr, "error")) {
    return(finish("covr_error",
                  extra = list(fail_reason = substr(conditionMessage(wr), 1, 300))))
  }

  if (wr$status %in% c("build_fail", "covr_error")) {
    return(finish(wr$status, extra = list(fail_reason = wr$fail_reason)))
  }

  if (identical(wr$status, "test_error")) {
    extra <- list(fail_reason = wr$fail_reason)
    if (!is.null(wr$summary)) {
      for (n in names(wr$summary)) extra[[n]] <- wr$summary[[n]]
      extra$coverage_tests_pct <- wr$summary$line_pct
    }
    out <- finish("test_error", tests_passed = FALSE, extra = extra)
    out$file <- wr$file
    out$func <- wr$func
    out$raw  <- wr$raw
    return(out)
  }

  # status == "ok": tests passed. examples/vignettes are each their own
  # subprocess call, bounded by the same timeout_s, so a hang in one of
  # them cannot erase this result (see .covr_subprocess).
  type_pct <- function(tp) tryCatch(
    callr::r(.covr_subprocess,
            args = list(mode = "type_pct", repo_root = repo_root,
                        pkgdir = pkgdir, type = tp),
            timeout = timeout_s),
    error = function(e) NA_real_)

  extra <- as.list(wr$summary)
  extra$coverage_tests_pct     <- wr$summary$line_pct
  extra$coverage_examples_pct  <- type_pct("examples")
  extra$coverage_vignettes_pct <- type_pct("vignettes")
  extra$suggests_failed        <- wr$suggests_failed

  out <- finish("ok", tests_passed = TRUE, extra = extra)
  out$file <- wr$file
  out$func <- wr$func
  out$raw  <- wr$raw
  out
}

#' Production entry: fetch the source, then run.
run_unit <- function(package, version, workdir) {
  pkgdir <- fetch_source(package, version, workdir)
  if (is.null(pkgdir)) {
    fp <- apply_determinism()
    s <- data.frame(package = package, version = version, covr_status = "build_fail",
                    tests_passed = FALSE, fail_reason = "source fetch failed",
                    line_pct = NA_real_, run_seconds = 0,
                    r_version = fp$r_version, covr_version = fp$covr_version,
                    stringsAsFactors = FALSE)
    return(list(summary = s, file = NULL, func = NULL, raw = NULL))
  }
  run_unit_dir(package, version, pkgdir)
}
