test_that("select_shard picks unanalyzed latest versions", {
  uni <- data.frame(package = c("a","b","c"), latest_version = c("1","2","3"),
                    stringsAsFactors = FALSE)
  analyzed <- c(a = "1")                      # a done, b/c not
  expect_setequal(select_shard(uni, analyzed, 10L), c("b","c"))
  expect_length(select_shard(uni, analyzed, 1L), 1L)
})

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
