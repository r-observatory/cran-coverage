test_that("run_unit classifies a package with no tests as no_tests", {
  d <- tempfile("nt_"); dir.create(file.path(d, "R"), recursive = TRUE)
  writeLines(c("Package: notest","Version: 1.0","Title: t","Description: t.",
               "License: MIT"), file.path(d, "DESCRIPTION"))
  writeLines("export(f)", file.path(d, "NAMESPACE"))
  writeLines("f <- function() 1", file.path(d, "R", "f.R"))
  res <- run_unit_dir("notest", "1.0", d)   # test-only helper over a local dir
  expect_identical(res$summary$covr_status, "no_tests")
  expect_true(is.na(res$summary$line_pct))
})

test_that("run_unit_dir produces ok coverage on the fixture", {
  skip_if_not_installed("covr"); skip_if_not_installed("testthat")
  pkg <- make_fixture_pkg()               # defined in test-coverage-extract.R
  res <- run_unit_dir("covfix", "1.0", pkg)
  expect_identical(res$summary$covr_status, "ok")
  expect_equal(res$summary$line_pct, 50)
  expect_true(res$summary$tests_passed)
  expect_true(is.raw(res$raw))
})
