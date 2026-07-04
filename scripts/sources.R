# Acquire the latest-version source tarball for a CRAN package.

#' Latest CRAN version for a package from an available.packages() matrix.
latest_version <- function(package, db) {
  if (!package %in% rownames(db)) return(NA_character_)
  as.character(db[package, "Version"])
}

#' Download and extract the CRAN source tarball. Returns the extracted package
#' directory, or NULL if the download or extraction fails.
fetch_source <- function(package, version, dest,
                         repo = "https://cloud.r-project.org") {
  tarball <- sprintf("%s_%s.tar.gz", package, version)
  urls <- c(
    file.path(repo, "src", "contrib", tarball),
    file.path(repo, "src", "contrib", "Archive", package, tarball)
  )
  local <- file.path(dest, tarball)
  ok <- FALSE
  for (u in urls) {
    ok <- tryCatch(
      utils::download.file(u, local, quiet = TRUE, mode = "wb") == 0L,
      error = function(e) FALSE, warning = function(w) FALSE
    )
    if (ok && file.exists(local) && file.size(local) > 0L) break
  }
  if (!ok) return(NULL)
  res <- tryCatch(utils::untar(local, exdir = dest), error = function(e) 1L)
  if (!identical(res, 0L)) return(NULL)
  pkgdir <- file.path(dest, package)
  if (!file.exists(file.path(pkgdir, "DESCRIPTION"))) return(NULL)
  pkgdir
}
