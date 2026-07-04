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
  set.seed(seed)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
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

# --- extractors (append to scripts/coverage.R) ---

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

#' One-row summary. True line counts (from a per-line tally), true expression
#' counts (from the traced-expression data frame), line/expression
#' percentages, and the compiled split.
summarise_coverage <- function(cov) {
  df <- .cov_df(cov)
  r  <- df[!df$is_compiled, , drop = FALSE]
  cc <- df[df$is_compiled, , drop = FALSE]
  pct <- function(x) if (nrow(x) == 0L) NA_real_ else round(100 * mean(x$covered), 4)

  tl <- covr::tally_coverage(cov, by = "line")
  tl <- tl[!.is_compiled_file(tl$filename), , drop = FALSE]
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
    zero_coverage_lines    = nrow(tl) - lines_covered,
    compiled_line_pct      = pct(cc),
    compiled_lines_total   = nrow(cc),
    compiled_lines_covered = sum(cc$covered),
    stringsAsFactors = FALSE
  )
}

#' Per-file coverage.
file_coverage <- function(cov) {
  df <- .cov_df(cov)
  agg <- stats::aggregate(covered ~ filename, df, function(x) c(sum(x), length(x)))
  data.frame(
    file          = agg$filename,
    lines_covered = agg$covered[, 1],
    lines_total   = agg$covered[, 2],
    coverage_pct  = round(100 * agg$covered[, 1] / agg$covered[, 2], 4),
    stringsAsFactors = FALSE
  )
}

#' Per-function coverage. Joined on (file, label). covr labels S4 methods by
#' signature and R6/RC methods by bare name; we keep the label verbatim and
#' also carry the file so downstream reconciliation with the analyzer is
#' possible, and we never assume a 1:1 name match.
function_coverage <- function(cov) {
  df <- .cov_df(cov)
  df <- df[!is.na(df$functions) & nzchar(df$functions), , drop = FALSE]
  if (nrow(df) == 0L) {
    return(data.frame(file = character(), label = character(),
                      lines_total = integer(), lines_covered = integer(),
                      coverage_pct = numeric(), stringsAsFactors = FALSE))
  }
  key <- paste(df$filename, df$functions, sep = "\x1f")
  agg <- stats::aggregate(covered ~ key, df, function(x) c(sum(x), length(x)))
  parts <- do.call(rbind, strsplit(agg$key, "\x1f", fixed = TRUE))
  data.frame(
    file          = parts[, 1],
    label         = parts[, 2],
    lines_covered = agg$covered[, 1],
    lines_total   = agg$covered[, 2],
    coverage_pct  = round(100 * agg$covered[, 1] / agg$covered[, 2], 4),
    stringsAsFactors = FALSE
  )
}

# --- unit runner (append to scripts/coverage.R) ---

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

#' Run covr over an already-extracted package directory.
run_unit_dir <- function(package, version, pkgdir) {
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

  cov <- tryCatch(
    covr::package_coverage(pkgdir, type = "tests", quiet = TRUE),
    error = function(e) structure(list(msg = conditionMessage(e)), class = "cov_err")
  )
  if (inherits(cov, "cov_err")) {
    status <- if (grepl("compilation failed|non-zero exit", cov$msg)) "build_fail" else "covr_error"
    return(finish(status, extra = list(fail_reason = substr(cov$msg, 1, 300))))
  }

  s   <- summarise_coverage(cov)
  fcv <- file_coverage(cov)
  fnv <- function_coverage(cov)
  # by-type: examples and vignettes reinstall the target; keep them separate.
  type_pct <- function(tp) tryCatch(
    round(covr::percent_coverage(covr::package_coverage(pkgdir, type = tp, quiet = TRUE), by = "line"), 4),
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
