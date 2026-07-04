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
