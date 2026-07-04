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
