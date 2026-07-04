# scripts/export.R: SQLite schema and schema-flexible upsert for the coverage stream.

#' Coerce logical columns in a data.frame to 0/1 INTEGER.
#'
#' SQLite has no native boolean type. This helper converts every logical
#' column to integer (TRUE -> 1L, FALSE -> 0L, NA -> NA_integer_) so that
#' downstream reads are stable regardless of driver type inference.
#'
#' @param df A data.frame. Non-logical columns are unchanged.
#' @return A copy of df with all logical columns replaced by integer.
.coerce_logicals <- function(df) {
  for (col in names(df)) {
    if (is.logical(df[[col]])) {
      df[[col]] <- as.integer(df[[col]])
    }
  }
  df
}

#' Open (or create) the coverage pipeline SQLite database.
#'
#' The two child tables (coverage_file, coverage_function) have fixed
#' schemas and are created here if absent. coverage_summary is created
#' lazily by upsert_coverage the first time data is written, since its
#' schema is schema-flexible (grown via ALTER TABLE as new metric columns
#' appear).
#'
#' @param path File path for the SQLite database.
#' @return An open DBI connection. The caller is responsible for calling
#'   DBI::dbDisconnect() when done.
open_db <- function(path) {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS coverage_file (
    package TEXT, version TEXT, file TEXT,
    lines_total INTEGER, lines_covered INTEGER, coverage_pct REAL)")
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS coverage_function (
    package TEXT, version TEXT, file TEXT, label TEXT,
    lines_total INTEGER, lines_covered INTEGER, coverage_pct REAL)")
  con
}

#' Query the analyzed version per package from the DB.
#'
#' @param con Open DBI connection to the coverage pipeline database.
#' @return Named character vector of version keyed by package. Empty
#'   character vector when coverage_summary does not exist yet or is empty.
analyzed_versions <- function(con) {
  if (!"coverage_summary" %in% DBI::dbListTables(con)) return(character(0))
  df <- DBI::dbGetQuery(con, "SELECT package, version FROM coverage_summary")
  if (nrow(df) == 0L) return(character(0))
  stats::setNames(as.character(df$version), as.character(df$package))
}

#' Delete prior rows for each (package, version) group in df, then append.
#'
#' Silently no-ops if df is NULL or empty.
.del_and_append <- function(con, tbl, df) {
  if (is.null(df) || nrow(df) == 0L) return(invisible())
  for (pv in split(df, list(df$package, df$version), drop = TRUE)) {
    DBI::dbExecute(con, sprintf("DELETE FROM %s WHERE package=? AND version=?", tbl),
                   params = list(pv$package[1], pv$version[1]))
    DBI::dbAppendTable(con, tbl, pv)
  }
  invisible()
}

#' Upsert one shard's rows into the coverage pipeline database.
#'
#' For each (package, version) present in summary_df, deletes prior rows
#' from coverage_summary and appends the fresh rows. Schema growth for
#' coverage_summary: if summary_df contains columns not yet present in the
#' table, ALTER TABLE ... ADD COLUMN is issued for each before the append.
#' If the table does not exist yet, it is created from summary_df
#' (schema-flexible) and indexed on (package, version).
#'
#' file_df and func_df are upserted the same way (delete matching
#' package/version rows, then append) into coverage_file and
#' coverage_function respectively.
#'
#' Logical columns in all three data.frames are coerced to 0/1 INTEGER.
#'
#' @param con        Open DBI connection from open_db().
#' @param summary_df data.frame; columns package + version required.
#' @param file_df    data.frame; columns package, version, file,
#'   lines_total, lines_covered, coverage_pct. May be NULL.
#' @param func_df    data.frame; columns package, version, file, label,
#'   lines_total, lines_covered, coverage_pct. May be NULL.
#' @return invisible(TRUE)
upsert_coverage <- function(con, summary_df, file_df, func_df) {
  sw <- .coerce_logicals(summary_df)
  if (!"coverage_summary" %in% DBI::dbListTables(con)) {
    DBI::dbWriteTable(con, "coverage_summary", sw)
    DBI::dbExecute(con, "CREATE UNIQUE INDEX idx_cov_pkg_ver
                         ON coverage_summary(package, version)")
  } else {
    existing <- DBI::dbListFields(con, "coverage_summary")
    for (col in setdiff(names(sw), existing)) {
      type <- if (is.numeric(sw[[col]])) "REAL" else "TEXT"
      DBI::dbExecute(con, sprintf(
        "ALTER TABLE coverage_summary ADD COLUMN \"%s\" %s", col, type))
    }
    for (i in seq_len(nrow(sw))) {
      DBI::dbExecute(con, "DELETE FROM coverage_summary WHERE package=? AND version=?",
                     params = list(sw$package[i], sw$version[i]))
    }
    DBI::dbAppendTable(con, "coverage_summary", sw)
  }

  .del_and_append(con, "coverage_file", .coerce_logicals(file_df))
  .del_and_append(con, "coverage_function", .coerce_logicals(func_df))
  invisible(TRUE)
}

# Raw covr object store: the serialized covr coverage object for each
# package/version is kept on disk, partitioned by package first letter, and
# bundled into per-partition tarballs for release publishing.

#' Partition key for a package's raw covr object: its lowercased first
#' letter, or "0" for anything that doesn't start with a-z (digits,
#' punctuation).
#'
#' @param package Package name.
#' @return A single-character partition key.
raw_partition <- function(package) {
  ch <- tolower(substr(package, 1, 1))
  if (grepl("[a-z]", ch)) ch else "0"
}

#' Write one package/version's serialized raw covr object to its partition.
#'
#' No-ops when raw is NULL (statuses that never produced a coverage object,
#' for example no_tests or build_fail).
#'
#' @param dir     Root directory of the raw object store (partitions are
#'   created underneath it).
#' @param package Package name.
#' @param version Package version.
#' @param raw     The raw vector to persist (from serialize(cov, NULL)), or
#'   NULL.
#' @return invisible(NULL).
write_raw_object <- function(dir, package, version, raw) {
  if (is.null(raw)) return(invisible(NULL))
  part <- file.path(dir, raw_partition(package))
  dir.create(part, showWarnings = FALSE, recursive = TRUE)
  saveRDS(raw, file.path(part, sprintf("%s_%s.rds", package, version)),
          compress = "xz")
}

#' Tar each partition directory into its own covr-raw-<partition>.tar.gz,
#' so the whole store can be published and re-seeded as a set of
#' per-partition release assets rather than one growing-forever archive.
#'
#' @param dir     Root directory of the raw object store (one
#'   subdirectory per partition, as populated by write_raw_object()).
#' @param out_dir Directory to write the covr-raw-*.tar.gz files into;
#'   created if absent.
#' @return invisible(NULL).
bundle_partitions <- function(dir, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  for (part in list.dirs(dir, recursive = FALSE)) {
    name <- basename(part)
    tar  <- file.path(out_dir, sprintf("covr-raw-%s.tar.gz", name))
    utils::tar(tar, files = part, compression = "gzip", tar = "internal")
  }
}
