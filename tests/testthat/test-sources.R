test_that("latest_version reads the version from an available.packages matrix", {
  db <- matrix(c("dplyr", "1.1.4"), nrow = 1,
               dimnames = list("dplyr", c("Package", "Version")))
  expect_identical(latest_version("dplyr", db), "1.1.4")
  expect_true(is.na(latest_version("nope", db)))
})

test_that("fetch_source downloads and extracts a small package (network)", {
  skip_on_cran()
  skip_if_offline()
  dest <- tempfile("src_"); dir.create(dest)
  on.exit(unlink(dest, recursive = TRUE, force = TRUE))
  db <- available.packages(repos = "https://cloud.r-project.org")
  v <- latest_version("praise", db)
  skip_if(is.na(v), "praise not available")
  pkgdir <- fetch_source("praise", v, dest)
  expect_true(!is.null(pkgdir) && file.exists(file.path(pkgdir, "DESCRIPTION")))
})
