# Tests for the manifest integrity / completeness core (scripts/export.R):
# file_sha256(), coverage_complete(), summary_integrity_core(), and the way
# the workflow merges the core into the release manifest.json.

# Build a tiny, real canonical database on disk via the pipeline's own schema
# (open_db creates coverage_file/coverage_function; the first upsert creates
# coverage_summary). Returns the file path.
build_coverage_db <- function() {
  db <- tempfile(fileext = ".db")
  con <- open_db(db)
  upsert_coverage(
    con,
    data.frame(package = c("a", "b"), version = "1", covr_status = "ok",
               line_pct = c(50, 80), stringsAsFactors = FALSE),
    data.frame(package = "a", version = "1", file = "R/x.R",
               lines_total = 2, lines_covered = 1, coverage_pct = 50),
    NULL
  )
  DBI::dbDisconnect(con)
  db
}

test_that("summary_integrity_core reports filename, bytes, sha256, tables, complete", {
  db <- build_coverage_db()
  on.exit(unlink(db))

  core <- summary_integrity_core(db, complete = TRUE)

  expect_equal(core$db_filename, basename(db))
  # db_bytes is a double (not cast to integer) so files >= ~2 GiB do not
  # overflow to NA; compare against the uncast file.size() directly.
  expect_type(core$db_bytes, "double")
  expect_equal(core$db_bytes, file.size(db))
  # sha256 is lowercase 64-char hex of the exact file bytes
  expect_match(core$db_sha256, "^[0-9a-f]{64}$")
  # tables maps every user table to its row count (no sqlite_% internal tables)
  expect_equal(core$tables,
               list(coverage_file = 1L, coverage_function = 0L, coverage_summary = 2L))
  # complete is passed through by the caller
  expect_true(core$complete)
})

test_that("summary_integrity_core sha256 matches an independent digest of the bytes", {
  # Compute the expected hash via an external CLI tool, independent of
  # file_sha256()'s own preferred backend (digest/openssl), so this test
  # genuinely cross-checks the code path instead of re-running the same
  # library. Skip only if neither tool is on PATH (both are expected on CI).
  sha256sum_bin <- Sys.which("sha256sum")
  shasum_bin    <- Sys.which("shasum")
  if (!nzchar(sha256sum_bin) && !nzchar(shasum_bin)) {
    skip("neither sha256sum nor shasum is on PATH")
  }

  db <- build_coverage_db()
  on.exit(unlink(db))

  core <- summary_integrity_core(db, complete = FALSE)

  if (nzchar(sha256sum_bin)) {
    out <- system2(sha256sum_bin, shQuote(db), stdout = TRUE)
  } else {
    out <- system2(shasum_bin, c("-a", "256", shQuote(db)), stdout = TRUE)
  }
  independent <- tolower(sub("\\s.*$", "", out[1]))

  expect_equal(core$db_sha256, independent)
})

test_that("coverage_complete derives completeness honestly from remaining", {
  # complete only when the full CRAN universe has been analyzed
  expect_true(coverage_complete(0L))
  expect_false(coverage_complete(1L))
  expect_false(coverage_complete(5000L))
  # an unavailable universe (NA remaining) is never complete
  expect_false(coverage_complete(NA_integer_))
})

test_that("manifest merges the integrity core as top-level fields (JSON round-trip)", {
  db <- build_coverage_db()
  on.exit(unlink(db), add = TRUE)

  # Mirror the workflow's manifest assembly: existing fields + the core.
  remaining <- 3L
  man <- list(processed = 2L,
              remaining = remaining,
              generated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))
  man <- c(man, summary_integrity_core(db, complete = coverage_complete(remaining)))

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(jsonlite::toJSON(man, auto_unbox = TRUE, pretty = TRUE), tmp)

  parsed <- jsonlite::fromJSON(tmp)
  # existing fields preserved
  expect_equal(parsed$processed, 2L)
  expect_equal(parsed$remaining, 3L)
  expect_true(nzchar(parsed$generated_at))
  # new top-level integrity/completeness core
  expect_equal(parsed$db_filename, basename(db))
  expect_equal(parsed$db_bytes, file.size(db))
  expect_match(parsed$db_sha256, "^[0-9a-f]{64}$")
  expect_equal(parsed$tables$coverage_summary, 2L)
  expect_equal(parsed$tables$coverage_file, 1L)
  # remaining = 3 -> not complete, serialized as JSON false
  expect_false(parsed$complete)
})
