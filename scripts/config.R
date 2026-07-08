# Pipeline-wide constants for the coverage stream.

SHARD_SIZE        <- 10L                 # units per shard; kept small so each shard finishes
                                         # and uploads well inside the job timeout, making
                                         # progress durable even when heavy packages are slow
DB_FILENAME       <- "cran-coverage.db"
PER_UNIT_TIMEOUT_S <- 1800L              # 30 min hard cap per covr pass; covr
                                         # instrumentation plus NOT_CRAN tests
                                         # make heavy suites (e.g. renv) exceed
                                         # 20 min
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

# Platform key for resolving a package's SystemRequirements to apt packages.
# Hardcoded because the collect container is pinned to rocker/r2u:noble
# (Ubuntu 24.04); revisit if the base image changes.
SYSREQS_PLATFORM  <- "ubuntu-24.04"

# Where to install R package dependencies from. "r2u" (default) uses the rolling
# rocker/r2u apt binaries via bspm. "ppm" pins install.packages to a dated Posit
# PPM Ubuntu-noble binary snapshot, so dependency binaries always match the
# running R and never drift out of ABI with it (the rolling-image failure mode).
# Env-overridable so a run can pick a source without a code change.
PACKAGE_SOURCE    <- Sys.getenv("PACKAGE_SOURCE", "r2u")
# Dated PPM snapshot ("YYYY-MM-DD" or "latest"); only used when PACKAGE_SOURCE=ppm.
PPM_SNAPSHOT      <- Sys.getenv("PPM_SNAPSHOT", "latest")
