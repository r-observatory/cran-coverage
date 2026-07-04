# Sharded, resumable runner. Mirrors cran-code-metrics/scripts/update.R.

select_shard <- function(universe, analyzed, size) {
  todo <- universe$package[
    is.na(analyzed[universe$package]) |
    analyzed[universe$package] != universe$latest_version]
  todo <- sort(as.character(todo[!is.na(todo)]))
  utils::head(todo, size)
}

run_shard <- function(io, out_dir, shard_size = SHARD_SIZE) {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  db_path <- file.path(out_dir, DB_FILENAME)
  con <- open_db(db_path); on.exit(DBI::dbDisconnect(con), add = TRUE)
  raw_dir <- file.path(out_dir, "raw"); dir.create(raw_dir, showWarnings = FALSE)

  universe <- io$package_list()
  analyzed <- analyzed_versions(con)
  shard    <- select_shard(universe, analyzed, shard_size)

  processed <- 0L
  for (pkg in shard) {
    v <- universe$latest_version[universe$package == pkg][1]
    wd <- tempfile(paste0("cov_", pkg, "_")); dir.create(wd)
    res <- tryCatch(io$run(pkg, v, wd),
      error = function(e) list(summary = data.frame(package = pkg, version = v,
        covr_status = "covr_error", line_pct = NA_real_,
        fail_reason = conditionMessage(e), stringsAsFactors = FALSE),
        file = NULL, func = NULL, raw = NULL))
    upsert_coverage(con, res$summary, res$file, res$func)
    if (!is.null(res$raw)) write_raw_object(raw_dir, pkg, v, res$raw)
    unlink(wd, recursive = TRUE, force = TRUE)
    processed <- processed + 1L
  }
  manifest <- list(processed = processed, shard_size = length(shard),
                   remaining = max(0L, length(select_shard(universe,
                     analyzed_versions(con), .Machine$integer.max)) ))
  writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE),
             file.path(out_dir, "manifest.json"))
  manifest
}

default_io <- function() {
  list(
    package_list = function() {
      db <- available.packages(repos = "https://cloud.r-project.org")
      data.frame(package = rownames(db),
                 latest_version = as.character(db[, "Version"]),
                 stringsAsFactors = FALSE, row.names = NULL)
    },
    run = function(package, version, workdir) run_unit(package, version, workdir)
  )
}

# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------
if (identical(sys.nframe(), 0L)) {
  args <- commandArgs(trailingOnly = TRUE)
  out_dir <- if (length(args) >= 1L) args[[1]] else "out"
  run_shard(default_io(), out_dir)
  message("Done.")
}
