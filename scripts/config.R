# Pipeline-wide constants for the coverage stream.

SHARD_SIZE        <- 10L                 # units per shard; kept small so each shard finishes
                                         # and uploads well inside the job timeout, making
                                         # progress durable even when heavy packages are slow
DB_FILENAME       <- "cran-coverage.db"
PER_UNIT_TIMEOUT_S <- 1200L              # 20 min hard cap per package
COVERAGE_SEED     <- 20260704L
COVR_TYPES        <- c("tests", "examples", "vignettes")

# Bounded retry of transient failures. A package that fails for a transient
# reason (a mid-run shared-object replacement -> covr_error, a one-off
# timeout, a flaky dependency install -> build_fail) is re-attempted on
# later runs, up to MAX_ATTEMPTS total, instead of being permanently skipped
# on its first failure. Genuinely-broken packages perma-skip once they hit
# the cap so the budget is not burned re-failing them forever. no_tests,
# test_error, and ok are terminal outcomes and are never re-attempted.
MAX_ATTEMPTS      <- 3L
RETRYABLE_STATUS  <- c("build_fail", "covr_error", "timeout")
