test_that("apply_determinism sets locale, threads, NOT_CRAN, and seed", {
  fp <- apply_determinism()
  expect_identical(Sys.getenv("NOT_CRAN"), "true")
  expect_identical(Sys.getenv("OMP_NUM_THREADS"), "1")
  expect_identical(getOption("mc.cores"), 1L)
  expect_identical(fp$seed, COVERAGE_SEED)
  expect_true(grepl("UTF-8", fp$locale, ignore.case = TRUE) || fp$locale != "")
})

test_that("status vocabulary is complete", {
  expect_true(all(c("ok","no_tests","build_fail","sysreq_fail",
                    "test_error","timeout","covr_error") %in% COVR_STATUS))
})
