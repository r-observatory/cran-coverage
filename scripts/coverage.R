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
