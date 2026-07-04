# Pipeline-wide constants for the coverage stream.

SHARD_SIZE        <- 60L                 # units per shard; small, each is a full build+test
DB_FILENAME       <- "cran-coverage.db"
PER_UNIT_TIMEOUT_S <- 1200L              # 20 min hard cap per package
COVERAGE_SEED     <- 20260704L
MAX_UNIT_FAILURES <- 3L                  # consecutive failures before perma-skip
COVR_TYPES        <- c("tests", "examples", "vignettes")
