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

test_that(".tag_pv adds package/version columns and survives a 0-row table", {
  nz <- data.frame(file = "R/x.R", lines_covered = 1L, lines_total = 2L,
                   coverage_pct = 50, stringsAsFactors = FALSE)
  got <- .tag_pv(nz, "pkg", "1.0")
  expect_equal(names(got), c("package", "version", names(nz)))
  expect_equal(got$package, "pkg")
  expect_equal(got$version, "1.0")
  expect_equal(got$file, "R/x.R")
  # An empty coverage object (all tests skipped) makes file_coverage() return
  # 0 rows; the old cbind(package=, version=, df) crashed on exactly this.
  z <- nz[0, , drop = FALSE]
  expect_error(cbind(package = "pkg", version = "1.0", z), "differing number of rows")
  gz <- .tag_pv(z, "pkg", "1.0")
  expect_equal(nrow(gz), 0L)
  expect_equal(names(gz), c("package", "version", names(nz)))
})
