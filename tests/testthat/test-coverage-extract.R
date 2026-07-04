# make_fixture_pkg() is defined in tests/testthat/helper-fixture.R and shared
# across test files.

test_that("summary/file/function extraction on a known fixture", {
  skip_if_not_installed("covr")
  skip_if_not_installed("testthat")
  apply_determinism()
  pkg <- make_fixture_pkg()
  cov <- covr::package_coverage(pkg, quiet = TRUE)

  s <- summarise_coverage(cov)
  expect_equal(s$line_pct, 50)                 # add covered, untested not
  expect_true(s$lines_total >= 2)
  expect_equal(s$lines_total, 2)
  expect_equal(s$lines_covered, 1)
  expect_equal(s$expr_total, 2)
  expect_equal(s$expr_covered, 1)
  expect_true(is.na(s$compiled_line_pct))

  fc <- file_coverage(cov)
  expect_true(any(grepl("f\\.R$", fc$file)))

  fn <- function_coverage(cov)
  expect_setequal(fn$label, c("add", "untested"))
  expect_equal(fn$coverage_pct[fn$label == "add"], 100)
  expect_equal(fn$coverage_pct[fn$label == "untested"], 0)
})
