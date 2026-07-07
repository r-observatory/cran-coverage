test_that("run_shard upserts a fake unit result", {
  db_dir <- tempfile("run_"); dir.create(db_dir)
  io <- list(
    package_list = function() data.frame(package = "a", latest_version = "1",
                                         stringsAsFactors = FALSE),
    run = function(package, version, workdir) list(
      summary = data.frame(package = package, version = version,
                           line_pct = 42, covr_status = "ok", stringsAsFactors = FALSE),
      file = NULL, func = NULL, raw = serialize(1L, NULL))
  )
  man <- run_shard(io, db_dir, shard_size = 10L)
  con <- open_db(file.path(db_dir, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT line_pct FROM coverage_summary")$line_pct, 42)
  expect_equal(man$processed, 1L)
})

test_that("run_shard retries a transient failure up to the attempt cap then stops", {
  db_dir <- tempfile("retry_"); dir.create(db_dir)
  io <- list(
    package_list = function() data.frame(package = "flaky", latest_version = "1",
                                         stringsAsFactors = FALSE),
    run = function(package, version, workdir) list(
      summary = data.frame(package = package, version = version,
                           line_pct = NA_real_, covr_status = "covr_error",
                           stringsAsFactors = FALSE),
      file = NULL, func = NULL, raw = NULL)
  )
  attempts_seen <- integer(0)
  for (i in seq_len(MAX_ATTEMPTS + 2L)) {
    man <- run_shard(io, db_dir, shard_size = 10L)
    con <- open_db(file.path(db_dir, DB_FILENAME))
    a <- DBI::dbGetQuery(con, "SELECT attempts FROM coverage_summary WHERE package='flaky'")$attempts
    DBI::dbDisconnect(con)
    attempts_seen <- c(attempts_seen, a)
  }
  # attempts climb 1,2,...,MAX_ATTEMPTS and then the package is no longer selected
  expect_equal(max(attempts_seen), MAX_ATTEMPTS)
  expect_equal(tail(attempts_seen, 1L), MAX_ATTEMPTS)   # capped, not still climbing
})
