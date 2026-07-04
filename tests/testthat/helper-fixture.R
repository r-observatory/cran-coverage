make_fixture_pkg <- function() {
  d <- tempfile("cov_"); dir.create(file.path(d, "R"), recursive = TRUE)
  dir.create(file.path(d, "tests", "testthat"), recursive = TRUE)
  writeLines(c("Package: covfix","Version: 1.0","Title: t","Description: t.",
               "License: MIT","Suggests: testthat"), file.path(d, "DESCRIPTION"))
  writeLines(c("export(add)","export(untested)"), file.path(d, "NAMESPACE"))
  writeLines(c("add <- function(a, b) a + b",
               "untested <- function(x) if (x > 0) \"pos\" else \"neg\""),
             file.path(d, "R", "f.R"))
  writeLines("library(testthat); library(covfix); test_check(\"covfix\")",
             file.path(d, "tests", "testthat.R"))
  writeLines("test_that(\"add\", { expect_equal(add(1,2), 3) })",
             file.path(d, "tests", "testthat", "test-add.R"))
  d
}

#' A package whose test suite fails: add() returns a+b, but the test asserts
#' the wrong value. Used to check that a failing test run is classified
#' honestly (covr_status "test_error", tests_passed FALSE) instead of "ok".
make_failing_pkg <- function() {
  d <- tempfile("covfail_"); dir.create(file.path(d, "R"), recursive = TRUE)
  dir.create(file.path(d, "tests", "testthat"), recursive = TRUE)
  writeLines(c("Package: covfail","Version: 1.0","Title: t","Description: t.",
               "License: MIT","Suggests: testthat"), file.path(d, "DESCRIPTION"))
  writeLines("export(add)", file.path(d, "NAMESPACE"))
  writeLines("add <- function(a, b) a + b", file.path(d, "R", "f.R"))
  writeLines("library(testthat); library(covfail); test_check(\"covfail\")",
             file.path(d, "tests", "testthat.R"))
  writeLines("test_that(\"add\", { expect_equal(add(1,2), 999) })",
             file.path(d, "tests", "testthat", "test-add.R"))
  d
}
