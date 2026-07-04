test_that("config constants have sane values", {
  expect_true(is.numeric(SHARD_SIZE) && SHARD_SIZE > 0L)
  expect_identical(DB_FILENAME, "cran-coverage.db")
  expect_identical(COVERAGE_SEED, 20260704L)
  expect_setequal(COVR_TYPES, c("tests", "examples", "vignettes"))
})
