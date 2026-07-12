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
  man <- suppressMessages(run_shard(io, db_dir, shard_size = 10L))
  con <- open_db(file.path(db_dir, DB_FILENAME))
  on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT line_pct FROM coverage_summary")$line_pct, 42)
  expect_equal(man$processed, 1L)
})

test_that("run_shard logs per-package progress with status and coverage", {
  db_dir <- tempfile("log_"); dir.create(db_dir)
  io <- list(
    package_list = function() data.frame(package = "ggplot2", latest_version = "3.5.0",
                                         stringsAsFactors = FALSE),
    run = function(package, version, workdir) list(
      summary = data.frame(package = package, version = version, line_pct = 88.5,
                           covr_status = "ok", stringsAsFactors = FALSE),
      file = NULL, func = NULL, raw = NULL)
  )
  msgs <- testthat::capture_messages(run_shard(io, db_dir, shard_size = 10L))
  expect_true(any(grepl("ggplot2", msgs)))   # names the package it is working on
  expect_true(any(grepl("ok", msgs)))        # reports the outcome
  expect_true(any(grepl("88.5", msgs)))       # and the coverage number
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
    man <- suppressMessages(run_shard(io, db_dir, shard_size = 10L))
    con <- open_db(file.path(db_dir, DB_FILENAME))
    a <- DBI::dbGetQuery(con, "SELECT attempts FROM coverage_summary WHERE package='flaky'")$attempts
    DBI::dbDisconnect(con)
    attempts_seen <- c(attempts_seen, a)
  }
  # attempts climb 1,2,...,MAX_ATTEMPTS and then the package is no longer selected
  expect_equal(max(attempts_seen), MAX_ATTEMPTS)
  expect_equal(tail(attempts_seen, 1L), MAX_ATTEMPTS)   # capped, not still climbing
})

test_that("select_shard retries a transient 'not available' build_fail past the cap", {
  universe <- data.frame(package = c("recoverable", "genuine", "done"),
                         latest_version = "1", stringsAsFactors = FALSE)
  state <- data.frame(
    package     = c("recoverable", "genuine", "done"),
    version     = "1",
    covr_status = c("build_fail", "build_fail", "ok"),
    attempts    = c(MAX_ATTEMPTS, MAX_ATTEMPTS, 0L),   # both failures at the cap
    fail_reason = c("ERROR: dependency 'Rcpp' is not available for package 'recoverable'",
                    "compilation failed for package 'genuine'", NA_character_),
    stringsAsFactors = FALSE)
  sel <- select_shard(universe, state, 10L)
  expect_true("recoverable" %in% sel)   # transient dep-resolution -> retried past the cap
  expect_false("genuine" %in% sel)      # genuine compile failure -> stays capped
  expect_false("done" %in% sel)         # terminal ok -> never
})

test_that("select_shard still works when state carries no fail_reason column", {
  universe <- data.frame(package = "capped", latest_version = "1",
                         stringsAsFactors = FALSE)
  state <- data.frame(package = "capped", version = "1",
                      covr_status = "build_fail", attempts = MAX_ATTEMPTS,
                      stringsAsFactors = FALSE)
  expect_false("capped" %in% select_shard(universe, state, 10L))  # no reason -> capped
})

test_that("analyzed_state carries fail_reason", {
  db <- tempfile(fileext = ".db"); on.exit(unlink(db))
  con <- open_db(db); on.exit(DBI::dbDisconnect(con), add = TRUE)
  upsert_coverage(con, data.frame(package = "p", version = "1",
                                  covr_status = "build_fail",
                                  fail_reason = "dependency 'x' is not available",
                                  stringsAsFactors = FALSE), NULL, NULL)
  st <- analyzed_state(con)
  expect_true("fail_reason" %in% names(st))
  expect_match(st$fail_reason[st$package == "p"], "not available")
})
