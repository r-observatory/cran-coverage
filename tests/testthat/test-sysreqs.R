# System-requirements resolution (pre-seed apt -dev libs before covr rebuilds
# the target from source) and the build-failure classifier.

test_that(".is_build_failure buckets install/compile failures, not covr errors", {
  expect_true(.is_build_failure("Package installation did not succeed."))
  expect_true(.is_build_failure("compilation failed for package 'x'"))
  expect_true(.is_build_failure("system command 'R' failed, non-zero exit status"))
  expect_false(.is_build_failure("arguments imply differing number of rows: 1, 0"))
  expect_false(.is_build_failure("some other covr internal error"))
})

test_that(".resolve_sysreqs_apt maps declared requirements and no-ops on none", {
  # No SystemRequirements -> empty, and (per the fixed gate) no network call.
  expect_identical(.resolve_sysreqs_apt(""), character(0))
  expect_identical(.resolve_sysreqs_apt(NA_character_), character(0))

  skip_if_not_installed("pkgdepends")
  apt <- .resolve_sysreqs_apt("GNU GSL", pkgname = "abn")
  expect_true(is.character(apt))
  expect_true(length(apt) >= 1L)                 # resolves to a -dev lib offline
  expect_true(all(nzchar(apt)))
})

test_that("install_sysreqs reads the target DESCRIPTION and is best-effort", {
  rm(list = ls(.SYSREQS_DONE), envir = .SYSREQS_DONE)   # isolate from other tests
  mkpkg <- function(sysreqs) {
    d <- tempfile("srq_"); dir.create(d)
    dcf <- c("Package: fixture", "Version: 1.0", "Title: t", "Description: t.",
             "License: MIT", if (nzchar(sysreqs)) paste0("SystemRequirements: ", sysreqs))
    writeLines(dcf, file.path(d, "DESCRIPTION"))
    d
  }
  # No SystemRequirements -> nothing attempted.
  expect_identical(suppressMessages(install_sysreqs(mkpkg(""))), character(0))

  skip_if_not_installed("pkgdepends")
  # Declares a requirement -> returns the apt packages it attempted (the
  # actual apt-get is best-effort and simply no-ops where apt is unavailable).
  attempted <- suppressMessages(install_sysreqs(mkpkg("GNU GSL")))
  expect_true(is.character(attempted) && length(attempted) >= 1L)
})
