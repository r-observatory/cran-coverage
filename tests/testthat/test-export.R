test_that("upsert creates tables, grows schema, and replaces per package-version", {
  db <- tempfile(fileext = ".db"); on.exit(unlink(db))
  con <- open_db(db); on.exit(DBI::dbDisconnect(con), add = TRUE)
  s1 <- data.frame(package = "a", version = "1", line_pct = 50, covr_status = "ok",
                   stringsAsFactors = FALSE)
  f1 <- data.frame(package = "a", version = "1", file = "R/x.R",
                   lines_total = 2, lines_covered = 1, coverage_pct = 50)
  fn1 <- data.frame(package = "a", version = "1", file = "R/x.R", label = "g",
                    lines_total = 2, lines_covered = 1, coverage_pct = 50)
  upsert_coverage(con, s1, f1, fn1)
  expect_equal(DBI::dbGetQuery(con, "SELECT line_pct FROM coverage_summary")$line_pct, 50)

  # new column appears on a later shard
  s2 <- data.frame(package = "b", version = "2", line_pct = 80, covr_status = "ok",
                   expr_pct = 79, stringsAsFactors = FALSE)
  upsert_coverage(con, s2, NULL, NULL)
  expect_true("expr_pct" %in% DBI::dbListFields(con, "coverage_summary"))

  # replace a's row
  s1b <- data.frame(package = "a", version = "1", line_pct = 99, covr_status = "ok")
  upsert_coverage(con, s1b, NULL, NULL)
  got <- DBI::dbGetQuery(con, "SELECT line_pct FROM coverage_summary WHERE package='a'")
  expect_equal(got$line_pct, 99)

  av <- analyzed_versions(con)
  expect_identical(unname(av[["a"]]), "1")
})

test_that("summary_row_count reports rows, and 0 for absent/empty/corrupt files", {
  expect_equal(summary_row_count(tempfile()), 0L)          # no file
  empty <- tempfile(fileext = ".db"); on.exit(unlink(empty), add = TRUE)
  con <- open_db(empty); DBI::dbDisconnect(con)            # child tables only
  expect_equal(summary_row_count(empty), 0L)               # no coverage_summary
  db <- tempfile(fileext = ".db"); on.exit(unlink(db), add = TRUE)
  con <- open_db(db)
  upsert_coverage(con, data.frame(package = c("a", "b"), version = c("1", "1"),
                                  covr_status = "ok", stringsAsFactors = FALSE),
                  NULL, NULL)
  DBI::dbDisconnect(con)
  expect_equal(summary_row_count(db), 2L)
  bogus <- tempfile(fileext = ".db"); on.exit(unlink(bogus), add = TRUE)
  writeLines("not a database", bogus)
  expect_equal(summary_row_count(bogus), 0L)
})

test_that("analyzed_state returns version, status and attempts, defaulting attempts", {
  db <- tempfile(fileext = ".db"); on.exit(unlink(db))
  con <- open_db(db); on.exit(DBI::dbDisconnect(con), add = TRUE)
  # a DB written before the attempts column existed
  upsert_coverage(con, data.frame(package = "old", version = "1",
                                  covr_status = "covr_error", stringsAsFactors = FALSE),
                  NULL, NULL)
  st <- analyzed_state(con)
  expect_equal(st$covr_status[st$package == "old"], "covr_error")
  expect_equal(st$attempts[st$package == "old"], 0L)       # NA/missing -> 0
  # empty DB -> zero-row frame with the expected columns
  db2 <- tempfile(fileext = ".db"); on.exit(unlink(db2), add = TRUE)
  con2 <- open_db(db2); on.exit(DBI::dbDisconnect(con2), add = TRUE)
  st2 <- analyzed_state(con2)
  expect_equal(nrow(st2), 0L)
  expect_true(all(c("package", "version", "covr_status", "attempts") %in% names(st2)))
})

test_that("merge_shards unions per-shard databases into one", {
  mk <- function(pkgs) {
    p <- tempfile(fileext = ".db")
    con <- open_db(p)
    upsert_coverage(con,
      data.frame(package = pkgs, version = "1", covr_status = "ok",
                 line_pct = 50, stringsAsFactors = FALSE),
      data.frame(package = pkgs, version = "1", file = "R/x.R",
                 lines_total = 2, lines_covered = 1, coverage_pct = 50),
      NULL)
    DBI::dbDisconnect(con)
    p
  }
  s1 <- mk(c("a", "b")); s2 <- mk(c("c", "d"))
  on.exit(unlink(c(s1, s2)))
  out <- tempfile(fileext = ".db"); on.exit(unlink(out), add = TRUE)
  merge_shards(c(s1, s2), out)
  con <- open_db(out); on.exit(DBI::dbDisconnect(con), add = TRUE)
  got <- DBI::dbGetQuery(con, "SELECT package FROM coverage_summary ORDER BY package")
  expect_equal(got$package, c("a", "b", "c", "d"))
  expect_equal(summary_row_count(out), 4L)
  fcount <- DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM coverage_file")$n
  expect_equal(fcount, 4L)
})

test_that("merge_shards upserts onto an existing database (monotonic merge)", {
  base <- tempfile(fileext = ".db"); on.exit(unlink(base), add = TRUE)
  con <- open_db(base)
  upsert_coverage(con, data.frame(package = c("stuck", "keep"), version = "1",
                                  covr_status = c("build_fail", "ok"),
                                  line_pct = c(NA, 60), stringsAsFactors = FALSE),
                  NULL, NULL)
  DBI::dbDisconnect(con)
  # a shard that recovers 'stuck' and adds 'new', but does not carry 'keep'
  shard <- tempfile(fileext = ".db"); on.exit(unlink(shard), add = TRUE)
  con <- open_db(shard)
  upsert_coverage(con, data.frame(package = c("stuck", "new"), version = "1",
                                  covr_status = "ok", line_pct = c(90, 70),
                                  stringsAsFactors = FALSE), NULL, NULL)
  DBI::dbDisconnect(con)
  # merge the shard ONTO a copy of the base: recoveries apply, nothing drops
  out <- tempfile(fileext = ".db"); on.exit(unlink(out), add = TRUE)
  file.copy(base, out)
  n <- merge_shards(shard, out)
  con <- open_db(out); on.exit(DBI::dbDisconnect(con), add = TRUE)
  got <- DBI::dbGetQuery(con, "SELECT package, covr_status FROM coverage_summary")
  st  <- stats::setNames(got$covr_status, got$package)
  expect_equal(unname(st[["stuck"]]), "ok")   # recovered (row updated)
  expect_equal(unname(st[["keep"]]),  "ok")   # preserved (shard lacked it)
  expect_true("new" %in% got$package)         # added
  expect_equal(n, 3L)                         # never drops rows below the base
})

test_that("bundle_partitions honours a custom asset-name prefix", {
  root <- tempfile("raw_"); dir.create(file.path(root, "a"), recursive = TRUE)
  saveRDS(1L, file.path(root, "a", "aaa_1.rds"))
  outd <- tempfile("bundle_"); on.exit(unlink(c(root, outd), recursive = TRUE))
  bundle_partitions(root, outd, prefix = "covr-raw-s2-")
  expect_true(file.exists(file.path(outd, "covr-raw-s2-a.tar.gz")))
})
