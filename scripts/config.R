# Pipeline-wide constants for the coverage stream.

SHARD_SIZE        <- 10L                 # units per shard; kept small so each shard finishes
                                         # and uploads well inside the job timeout, making
                                         # progress durable even when heavy packages are slow
DB_FILENAME       <- "cran-coverage.db"
PER_UNIT_TIMEOUT_S <- 1200L              # 20 min hard cap per package
COVERAGE_SEED     <- 20260704L
COVR_TYPES        <- c("tests", "examples", "vignettes")
