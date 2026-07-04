# Run covr under a fixed deterministic environment and extract metrics.

COVR_STATUS <- c("ok", "no_tests", "build_fail", "sysreq_fail",
                 "test_error", "timeout", "covr_error")

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
      file    = cbind(package = package, version = version, file_coverage(cov)),
      func    = cbind(package = package, version = version, function_coverage(cov)),
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

#' Run covr over an already-extracted package directory. `timeout_s` bounds
#' each covr::package_coverage() call (the "tests" run and each by-type
#' run); a package that hangs (an infinite loop, a blocking network call, a
#' test that never returns) is recorded as covr_status = "timeout" instead
#' of hanging the whole shard forever. Defaults to the pipeline-wide
#' PER_UNIT_TIMEOUT_S but can be overridden, for example in tests.
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
      R.utils::withTimeout(
        covr::package_coverage(pkgdir, type = "tests", quiet = TRUE,
                               clean = FALSE, install_path = install_path),
        timeout = timeout_s, onTimeout = "error"
      ),
      warning = function(w) {
        test_warnings <<- c(test_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) structure(
      list(msg = conditionMessage(e), is_test_failure = inherits(e, "covr_error"),
           is_timeout = inherits(e, "TimeoutException")),
      class = "cov_err")
  )

  if (inherits(cov, "cov_err")) {
    if (isTRUE(cov$is_timeout)) {
      return(finish("timeout", tests_passed = FALSE,
                    extra = list(fail_reason = substr(cov$msg, 1, 300))))
    }
    if (isTRUE(cov$is_test_failure)) {
      recovered <- .recover_partial_coverage(pkgdir, install_path, package, version)
      extra <- list(fail_reason = substr(cov$msg, 1, 300))
      if (!is.null(recovered)) {
        rs <- recovered$summary
        for (n in names(rs)) extra[[n]] <- rs[[n]]
        extra$coverage_tests_pct <- rs$line_pct
      }
      out <- finish("test_error", tests_passed = FALSE, extra = extra)
      if (!is.null(recovered)) {
        out$file <- recovered$file
        out$func <- recovered$func
        out$raw  <- recovered$raw
      }
      return(out)
    }
    status <- if (grepl("compilation failed|non-zero exit", cov$msg)) "build_fail" else "covr_error"
    return(finish(status, extra = list(fail_reason = substr(cov$msg, 1, 300))))
  }

  if (.warn_is_test_failure(test_warnings)) {
    s   <- summarise_coverage(cov)
    fcv <- file_coverage(cov)
    fnv <- function_coverage(cov)
    extra <- as.list(s)
    extra$coverage_tests_pct <- s$line_pct
    bad <- test_warnings[grepl("test.*fail|fail.*test|FAIL\\s+[1-9]", test_warnings, ignore.case = TRUE)]
    extra$fail_reason <- substr(bad[1], 1, 300)
    out <- finish("test_error", tests_passed = FALSE, extra = extra)
    out$file <- cbind(package = package, version = version, fcv)
    out$func <- cbind(package = package, version = version, fnv)
    out$raw  <- serialize(cov, connection = NULL)
    return(out)
  }

  s   <- summarise_coverage(cov)
  fcv <- file_coverage(cov)
  fnv <- function_coverage(cov)
  # by-type: examples and vignettes reinstall the target; keep them separate.
  # Bounded by the same timeout as the tests run, since these also invoke
  # package code (rendering vignettes, running examples) that can hang.
  type_pct <- function(tp) tryCatch(
    round(covr::percent_coverage(
      R.utils::withTimeout(covr::package_coverage(pkgdir, type = tp, quiet = TRUE),
                           timeout = timeout_s, onTimeout = "error"),
      by = "line"), 4),
    error = function(e) NA_real_)
  extra <- as.list(s)
  extra$coverage_tests_pct     <- s$line_pct
  extra$coverage_examples_pct  <- type_pct("examples")
  extra$coverage_vignettes_pct <- type_pct("vignettes")
  extra$suggests_failed        <- if (length(failed_soft)) paste(failed_soft, collapse = ",") else NA_character_

  out <- finish("ok", tests_passed = TRUE, extra = extra)
  out$file <- cbind(package = package, version = version, fcv)
  out$func <- cbind(package = package, version = version, fnv)
  out$raw  <- serialize(cov, connection = NULL)
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
