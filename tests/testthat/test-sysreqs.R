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

# --- PPM full-tree system requirements ------------------------------------
# Under PACKAGE_SOURCE=ppm the dependency binaries come from PPM, which (unlike
# r2u/bspm) does NOT auto-install their runtime system libraries, so the whole
# dependency tree's sysreqs must be resolved, not just the target's.

.mkdb <- function(rows) {
  cols <- c("Package", "Version", "Depends", "Imports", "LinkingTo",
            "Suggests", "SystemRequirements")
  m <- matrix("", nrow = length(rows), ncol = length(cols),
              dimnames = list(names(rows), cols))
  m[, "Package"] <- names(rows); m[, "Version"] <- "1.0"
  for (nm in names(rows)) for (f in names(rows[[nm]])) m[nm, f] <- rows[[nm]][[f]]
  m
}

.mkpkg_desc <- function(fields) {
  d <- tempfile("trg_"); dir.create(d)
  base <- c("Package: target", "Version: 1.0", "Title: t", "Description: t.",
            "License: MIT")
  extra <- vapply(names(fields), function(f) paste0(f, ": ", fields[[f]]),
                  character(1))
  writeLines(c(base, extra), file.path(d, "DESCRIPTION"))
  d
}

test_that(".dep_tree returns the recursive hard-dependency names from the db", {
  db <- .mkdb(list(
    A = c(Imports = "B, C"),
    B = c(Imports = "D"),
    C = c(LinkingTo = "E"),
    D = c(), E = c()
  ))
  pkgdir <- .mkpkg_desc(list(Imports = "A", Depends = "R (>= 4.0)"))
  tree <- .dep_tree(pkgdir, db)
  expect_setequal(tree, c("A", "B", "C", "D", "E"))  # recursive, R dropped
})

test_that(".resolve_tree_sysreqs_apt resolves a DEPENDENCY's sysreqs, not just the target's", {
  skip_if_not_installed("pkgdepends")
  # Target declares nothing; a transitive dependency declares GNU GSL. The tree
  # resolver must still return apt packages -- proving it walked the deps.
  db <- .mkdb(list(A = c(Imports = "B"), B = c(SystemRequirements = "GNU GSL")))
  pkgdir <- .mkpkg_desc(list(Imports = "A"))
  apt <- .resolve_tree_sysreqs_apt(pkgdir, db)
  expect_true(is.character(apt) && length(apt) >= 1L)
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
