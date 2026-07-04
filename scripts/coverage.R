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
