# Capturing the real reason a package failed to build/install, instead of
# covr's generic "Package installation did not succeed."

test_that(".capture_install_error surfaces the real install error", {
  skip_on_os("windows")
  pkg <- make_broken_pkg()
  msg <- .capture_install_error(pkg, timeout_s = 120L)
  expect_true(is.character(msg) && length(msg) == 1L && nzchar(msg))
  # the actual R CMD INSTALL parse failure, not a generic message
  expect_match(msg, "error|unexpected|covbroken|INSTALL", ignore.case = TRUE)
})

test_that(".is_build_failure feeds the detailed reason (integration)", {
  skip_on_os("windows")
  skip_if_not_installed("covr")
  # A package with tests but broken R source reaches covr and fails to install.
  d <- make_broken_pkg()
  dir.create(file.path(d, "tests", "testthat"), recursive = TRUE)
  writeLines("library(testthat); library(covbroken); test_check(\"covbroken\")",
             file.path(d, "tests", "testthat.R"))
  writeLines("test_that(\"x\", { expect_true(TRUE) })",
             file.path(d, "tests", "testthat", "test-x.R"))
  res <- suppressMessages(run_unit_dir("covbroken", "1.0", d, timeout_s = 180L))
  expect_equal(res$summary$covr_status, "build_fail")
  # the reason is no longer only the generic covr message
  expect_true(nchar(res$summary$fail_reason) > nchar("Package installation did not succeed."))
})
