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

test_that("summary/file/function extraction on a known fixture", {
  skip_if_not_installed("covr")
  skip_if_not_installed("testthat")
  apply_determinism()
  pkg <- make_fixture_pkg()
  cov <- covr::package_coverage(pkg, quiet = TRUE)

  s <- summarise_coverage(cov)
  expect_equal(s$line_pct, 50)                 # add covered, untested not
  expect_true(s$lines_total >= 2)

  fc <- file_coverage(cov)
  expect_true(any(grepl("f\\.R$", fc$file)))

  fn <- function_coverage(cov)
  expect_setequal(fn$label, c("add", "untested"))
  expect_equal(fn$coverage_pct[fn$label == "add"], 100)
  expect_equal(fn$coverage_pct[fn$label == "untested"], 0)
})
