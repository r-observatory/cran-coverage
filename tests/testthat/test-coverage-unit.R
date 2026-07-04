test_that("run_unit classifies a package with no tests as no_tests", {
  d <- tempfile("nt_"); dir.create(file.path(d, "R"), recursive = TRUE)
  writeLines(c("Package: notest","Version: 1.0","Title: t","Description: t.",
               "License: MIT"), file.path(d, "DESCRIPTION"))
  writeLines("export(f)", file.path(d, "NAMESPACE"))
  writeLines("f <- function() 1", file.path(d, "R", "f.R"))
  res <- run_unit_dir("notest", "1.0", d)   # run_unit_dir over a local dir; run_unit delegates to it in production
  expect_identical(res$summary$covr_status, "no_tests")
  expect_true(is.na(res$summary$line_pct))
})

test_that("run_unit_dir produces ok coverage on the fixture", {
  skip_if_not_installed("covr"); skip_if_not_installed("testthat")
  pkg <- make_fixture_pkg()               # defined in helper-fixture.R
  res <- run_unit_dir("covfix", "1.0", pkg)
  expect_identical(res$summary$covr_status, "ok")
  expect_equal(res$summary$line_pct, 50)
  expect_true(res$summary$tests_passed)
  expect_true(is.raw(res$raw))
})

test_that("run_unit_dir classifies a failing test suite as test_error, not ok", {
  skip_if_not_installed("covr"); skip_if_not_installed("testthat")
  pkg <- make_failing_pkg()               # defined in helper-fixture.R
  res <- run_unit_dir("covfail", "1.0", pkg)
  expect_identical(res$summary$covr_status, "test_error")
  expect_false(res$summary$tests_passed)
  expect_true(is.character(res$summary$fail_reason))
  expect_true(nzchar(res$summary$fail_reason))
  expect_false(is.na(res$summary$line_pct))
})

test_that("run_unit_dir classifies a hanging test suite as timeout, killed near the deadline, not a deadlock", {
  skip_if_not_installed("covr"); skip_if_not_installed("testthat")
  skip_if_not_installed("callr")
  pkg <- make_hanging_pkg()               # defined in helper-fixture.R; sleeps 120s
  timeout_s <- 5
  elapsed <- system.time(
    res <- run_unit_dir("covhang", "1.0", pkg, timeout_s = timeout_s)
  )[["elapsed"]]
  expect_identical(res$summary$covr_status, "timeout")
  expect_false(res$summary$tests_passed)
  expect_true(is.character(res$summary$fail_reason))
  expect_true(nzchar(res$summary$fail_reason))
  expect_true(is.na(res$summary$line_pct))
  expect_null(res$raw)
  # Proves the subprocess was actually killed near the timeout deadline,
  # not waited out until the fixture's 120s sleep completed: a cooperative
  # timeout (R.utils::withTimeout/setTimeLimit) cannot interrupt covr's
  # shelled-out test run, so this elapsed-time bound is what the prior
  # (broken) fix lacked and could not have passed.
  expect_lt(elapsed, timeout_s + 20)
})
