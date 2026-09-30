# Re-measuring the (package, version) rows listed in data/requeue/requeue.tsv.

rq <- function(package, version, reason = "raw-object-lost") {
  data.frame(package = package, version = version, reason = reason,
             stringsAsFactors = FALSE)
}

stored <- function(package, version, status = "ok", attempts = 0L) {
  data.frame(package = package, version = version, covr_status = status,
             attempts = attempts, fail_reason = NA_character_,
             stringsAsFactors = FALSE)
}

universe_of <- function(package, version) {
  data.frame(package = package, latest_version = version, stringsAsFactors = FALSE)
}

empty_log <- function() {
  data.frame(run_id = character(0), package = character(0), version = character(0),
             covr_status = character(0), measured = integer(0),
             stringsAsFactors = FALSE)
}

# A prior bundle fetch where nothing is on the release yet.
no_bundles <- function() {
  list(expected = stats::setNames(numeric(0), character(0)), members = list(),
       digests = list(), unavailable = character(0))
}

# --- read_requeue --------------------------------------------------------------

test_that("read_requeue reads package, version and reason, and an absent file is empty", {
  f <- tempfile(fileext = ".tsv")
  writeLines(c("package\tversion\treason", "alpha\t1.0-2\traw-object-lost",
               "beta\t0.3\traw-object-lost"), f)
  q <- read_requeue(f)
  expect_identical(names(q), c("package", "version", "reason"))
  expect_identical(q$version, c("1.0-2", "0.3"))
  expect_identical(nrow(read_requeue(tempfile())), 0L)
})

test_that("read_requeue refuses missing columns, blank fields and repeated rows", {
  f <- tempfile(fileext = ".tsv")
  writeLines(c("package\tversion", "alpha\t1.0"), f)
  expect_error(read_requeue(f), "reason")
  writeLines(c("package\tversion\treason", "alpha\t\traw-object-lost"), f)
  expect_error(read_requeue(f), "blank")
  writeLines(c("package\tversion\treason", "alpha\t1.0\tx", "alpha\t1.0\ty"), f)
  expect_error(read_requeue(f), "more than once")
})

# --- requeue_state ---------------------------------------------------------------

test_that("a queued row is due only while its version is the current CRAN version", {
  raw <- tempfile("raw_"); dir.create(raw)
  s <- requeue_state(rq(c("alpha", "beta", "gone"), c("1.0", "2.0", "1.0")),
                     universe_of(c("alpha", "beta"), c("1.0", "2.1")),
                     stored(c("alpha", "beta", "gone"), c("1.0", "2.0", "1.0")),
                     raw, no_bundles(), empty_log(), "r1")
  expect_identical(s$due, c(TRUE, FALSE, FALSE))
  expect_identical(s$waiting, c(TRUE, FALSE, FALSE))
})

test_that("a queued row whose raw object exists leaves the queue", {
  raw <- tempfile("raw_"); dir.create(raw)
  write_raw_object(raw, "alpha", "1.0", serialize("cov", NULL))
  s <- requeue_state(rq(c("alpha", "apex"), c("1.0", "1.0")),
                     universe_of(c("alpha", "apex"), c("1.0", "1.0")),
                     stored(c("alpha", "apex"), "1.0"), raw, no_bundles(), empty_log(), "r1")
  expect_identical(s$waiting, c(FALSE, TRUE))
  expect_identical(s$due, c(FALSE, TRUE))
})

test_that("a queued row waits while its bundle was not fetched whole, or with no fetch record", {
  raw <- tempfile("raw_"); dir.create(raw)
  q <- rq(c("alpha", "beta"), c("1.0", "1.0"))
  uni <- universe_of(c("alpha", "beta"), c("1.0", "1.0"))
  st <- stored(c("alpha", "beta"), "1.0")
  prior <- list(expected = c("covr-raw-a.tar.gz" = 10, "covr-raw-b.tar.gz" = 10),
                members = list("covr-raw-b.tar.gz" = "out/raw/b/other_1.rds"),
                digests = list(), unavailable = "covr-raw-a.tar.gz")
  s <- requeue_state(q, uni, st, raw, prior, empty_log(), "r1")
  expect_identical(s$due, c(FALSE, TRUE))
  expect_identical(s$waiting, c(TRUE, TRUE))
  s0 <- requeue_state(q, uni, st, raw, NULL, empty_log(), "r1")
  expect_false(any(s0$due))
  expect_true(all(s0$waiting))
})

test_that("a queued row is only re-measured over a stored ok or test_error row", {
  raw <- tempfile("raw_"); dir.create(raw)
  s <- requeue_state(rq(c("alpha", "beta", "gamma"), "1.0"),
                     universe_of(c("alpha", "beta", "gamma"), "1.0"),
                     stored(c("alpha", "beta"), "1.0", c("test_error", "build_fail")),
                     raw, no_bundles(), empty_log(), "r1")
  expect_identical(s$due, c(TRUE, FALSE, FALSE))
})

test_that("a queued row is tried once per run and stops after REQUEUE_MAX_ATTEMPTS failures", {
  raw <- tempfile("raw_"); dir.create(raw)
  q <- rq(c("alpha", "beta"), "1.0")
  uni <- universe_of(c("alpha", "beta"), "1.0")
  st <- stored(c("alpha", "beta"), "1.0")
  log <- data.frame(run_id = c("r1", "r2"), package = c("alpha", "beta"),
                    version = "1.0", covr_status = "timeout", measured = 0L,
                    stringsAsFactors = FALSE)
  s <- requeue_state(q, uni, st, raw, no_bundles(), log, "r2")
  expect_identical(s$due, c(TRUE, FALSE))            # beta was tried this run
  capped <- data.frame(run_id = paste0("r", seq_len(REQUEUE_MAX_ATTEMPTS)),
                       package = "alpha", version = "1.0", covr_status = "build_fail",
                       measured = 0L, stringsAsFactors = FALSE)
  s2 <- requeue_state(q, uni, st, raw, no_bundles(), capped, "next")
  expect_identical(s2$due, c(FALSE, TRUE))
  expect_identical(s2$capped, c(TRUE, FALSE))
  expect_identical(s2$waiting, c(TRUE, TRUE))
})

test_that("requeue_state keeps only this runner's partition and names its bundle", {
  raw <- tempfile("raw_"); dir.create(raw)
  q <- rq(c("cards", "cli"), c("0.9.0", "3.6.3"))    # shard 1 and shard 0 of 4
  s <- requeue_state(q, universe_of(q$package, q$version), stored(q$package, q$version),
                     raw, no_bundles(), empty_log(), "r1", slice = list(index = 1L, count = 4L))
  expect_identical(s$package, "cards")
  expect_identical(raw_bundle_for("cards", list(index = 1L, count = 4L)),
                   "covr-raw-s1-c.tar.gz")
  expect_identical(raw_bundle_for(c("cards", "3dplot"), NULL),
                   c("covr-raw-c.tar.gz", "covr-raw-0.tar.gz"))
})

# --- select_shard with queued rows -----------------------------------------------

test_that("select_shard puts queued rows first, up to the budget, then normal work", {
  uni <- universe_of(c("new1", "q1", "q2", "q3", "done"), "1")
  st <- stored(c("q1", "q2", "q3", "done"), "1")
  expect_identical(select_shard(uni, st, 10L, requeue = c("q3", "q1", "q2"),
                                requeue_budget = 2L),
                   c("q1", "q2", "new1"))
  expect_identical(select_shard(uni, st, 10L, requeue = "q1", requeue_budget = 0L), "new1")
  expect_identical(select_shard(uni, st, 1L, requeue = c("q1", "q2"), requeue_budget = 5L), "q1")
  expect_identical(select_shard(uni, st, 10L, rank = "q3", requeue = c("q1", "q3"),
                                requeue_budget = 5L),
                   c("q3", "q1", "new1"))
  # Queued rows alone are selected even when no normal work is due.
  expect_identical(select_shard(uni[uni$package != "new1", ], st, 10L, requeue = "q2",
                                requeue_budget = 1L), "q2")
})

test_that("select_shard ignores queued packages outside its partition", {
  uni <- universe_of(c("cards", "cli"), c("0.9.0", "3.6.3"))
  st <- stored(uni$package, uni$latest_version)
  expect_identical(select_shard(uni, st, 10L, slice = list(index = 1L, count = 4L),
                                requeue = c("cards", "cli"), requeue_budget = 5L),
                   "cards")
})

# --- run_shard ---------------------------------------------------------------------

# An out/ directory holding stored rows for each package, as a prior run left
# them, and a record that the raw bundle fetch found nothing on the release.
requeue_fixture <- function(pkgs, version = "1.0", prior = no_bundles()) {
  out <- tempfile("rq_"); dir.create(file.path(out, "rawbundles"), recursive = TRUE)
  con <- open_db(file.path(out, DB_FILENAME))
  upsert_coverage(con,
    data.frame(package = pkgs, version = version, covr_status = "ok", line_pct = 50,
               attempts = 2L, fail_reason = NA_character_, stringsAsFactors = FALSE),
    data.frame(package = rep(pkgs, each = 2L), version = rep(version, each = 2L),
               file = c("R/a.R", "R/old.R"), lines_total = 10L, lines_covered = 5L,
               coverage_pct = 50, stringsAsFactors = FALSE),
    data.frame(package = pkgs, version = version, file = "R/a.R", label = "f",
               lines_total = 10L, lines_covered = 5L, coverage_pct = 50,
               stringsAsFactors = FALSE))
  DBI::dbDisconnect(con)
  if (!is.null(prior)) saveRDS(prior, file.path(out, "rawbundles", "prior-state.rds"))
  out
}

stored_rows <- function(out) {
  con <- open_db(file.path(out, DB_FILENAME)); on.exit(DBI::dbDisconnect(con))
  q <- function(t) DBI::dbGetQuery(con, sprintf(
    "SELECT * FROM %s ORDER BY package, version%s", t,
    if (t == "coverage_summary") "" else ", file"))
  list(summary = q("coverage_summary"), file = q("coverage_file"),
       func = q("coverage_function"))
}

# A unit runner that records each call and returns `status`; ok and test_error
# carry fresh coverage and a raw object, anything else carries neither.
recording_io <- function(pkgs, version = "1.0", status = "ok") {
  calls <- character(0)
  list(
    calls = function() calls,
    package_list = function() universe_of(pkgs, version),
    run = function(package, version, workdir) {
      calls <<- c(calls, package)
      if (identical(status, "error")) stop("covr subprocess died")
      if (!status %in% c("ok", "test_error")) {
        return(list(summary = data.frame(package = package, version = version,
                                         covr_status = status, line_pct = NA_real_,
                                         fail_reason = "compilation failed",
                                         stringsAsFactors = FALSE),
                    file = NULL, func = NULL, raw = NULL))
      }
      list(summary = data.frame(package = package, version = version, covr_status = status,
                                line_pct = 80, fail_reason = NA_character_,
                                stringsAsFactors = FALSE),
           file = data.frame(package = package, version = version, file = "R/a.R",
                             lines_total = 10L, lines_covered = 8L, coverage_pct = 80,
                             stringsAsFactors = FALSE),
           func = data.frame(package = package, version = version, file = "R/a.R",
                             label = "f", lines_total = 10L, lines_covered = 8L,
                             coverage_pct = 80, stringsAsFactors = FALSE),
           raw = serialize(paste("cov", package), NULL))
    })
}

test_that("a failed re-measure keeps the stored rows and their attempt count", {
  for (status in c("build_fail", "timeout", "error")) {
    out <- requeue_fixture("alpha")
    before <- stored_rows(out)
    io <- recording_io("alpha", status = status)
    man <- suppressMessages(run_shard(io, out, requeue = rq("alpha", "1.0"), run_id = "r1"))
    expect_identical(io$calls(), "alpha")
    expect_identical(stored_rows(out), before)
    expect_false(file.exists(raw_object_path(file.path(out, "raw"), "alpha", "1.0")))
    expect_identical(unlist(man$requeue),
                     c(queued = 1L, measured = 0L, failed = 1L, left = 1L, capped = 0L))
    # Tried once this run; the next run tries it again.
    man2 <- suppressMessages(run_shard(io, out, requeue = rq("alpha", "1.0"), run_id = "r1"))
    expect_identical(io$calls(), "alpha")
    expect_identical(man2$remaining, 0L)
    suppressMessages(run_shard(io, out, requeue = rq("alpha", "1.0"), run_id = "r2"))
    expect_identical(io$calls(), c("alpha", "alpha"))
  }
})

test_that("a successful re-measure replaces the stored rows and writes the raw object", {
  out <- requeue_fixture(c("alpha", "apex"))
  io <- recording_io(c("alpha", "apex"))
  man <- suppressMessages(run_shard(io, out, requeue = rq("alpha", "1.0"), run_id = "r1"))
  expect_identical(io$calls(), "alpha")
  rows <- stored_rows(out)
  a <- rows$summary[rows$summary$package == "alpha", ]
  expect_equal(a$line_pct, 80)
  expect_equal(a$attempts, 2)                        # the retry count is untouched
  expect_identical(rows$file$file[rows$file$package == "alpha"], "R/a.R")
  expect_equal(rows$file$lines_covered[rows$file$package == "alpha"], 8)
  expect_equal(rows$func$lines_covered[rows$func$package == "alpha"], 8)
  # The other package's rows are left alone.
  expect_equal(rows$summary$line_pct[rows$summary$package == "apex"], 50)
  expect_identical(nrow(rows$file[rows$file$package == "apex", ]), 2L)
  obj <- raw_object_path(file.path(out, "raw"), "alpha", "1.0")
  expect_identical(unserialize(readRDS(obj)), "cov alpha")
  expect_identical(unlist(man$requeue),
                   c(queued = 1L, measured = 1L, failed = 0L, left = 0L, capped = 0L))
  # With its object on disk the row has left the queue.
  man2 <- suppressMessages(run_shard(io, out, requeue = rq("alpha", "1.0"), run_id = "r2"))
  expect_identical(io$calls(), "alpha")
  expect_identical(unlist(man2$requeue),
                   c(queued = 0L, measured = 0L, failed = 0L, left = 0L, capped = 0L))
})

test_that("a re-measure with no function rows clears the stored ones", {
  out <- requeue_fixture("alpha")
  io <- recording_io("alpha")
  run <- io$run
  io$run <- function(package, version, workdir) {
    r <- run(package, version, workdir)
    r$func <- NULL
    r
  }
  suppressMessages(run_shard(io, out, requeue = rq("alpha", "1.0"), run_id = "r1"))
  rows <- stored_rows(out)
  expect_equal(rows$summary$line_pct, 80)
  expect_identical(nrow(rows$func), 0L)
  expect_identical(rows$file$file, "R/a.R")
})

test_that("failing tests do not replace a stored ok row, but do replace a stored test_error row", {
  out <- requeue_fixture("alpha")
  before <- stored_rows(out)
  io <- recording_io("alpha", status = "test_error")
  man <- suppressMessages(run_shard(io, out, requeue = rq("alpha", "1.0"), run_id = "r1"))
  expect_identical(stored_rows(out), before)
  expect_false(file.exists(raw_object_path(file.path(out, "raw"), "alpha", "1.0")))
  expect_identical(man$requeue$failed, 1L)

  out2 <- requeue_fixture("alpha")
  con <- open_db(file.path(out2, DB_FILENAME))
  DBI::dbExecute(con, "UPDATE coverage_summary SET covr_status = 'test_error'")
  DBI::dbDisconnect(con)
  man2 <- suppressMessages(run_shard(io, out2, requeue = rq("alpha", "1.0"), run_id = "r1"))
  expect_identical(man2$requeue$measured, 1L)
  expect_equal(stored_rows(out2)$summary$line_pct, 80)
  expect_true(file.exists(raw_object_path(file.path(out2, "raw"), "alpha", "1.0")))
})

test_that("the queued-row budget holds across one run's shards and resets for the next run", {
  pkgs <- c("ant", "ape", "arc", "ash", "awl")
  out <- requeue_fixture(pkgs)
  io <- recording_io(pkgs)
  go <- function(run) suppressMessages(run_shard(io, out, shard_size = 2L,
    requeue = rq(pkgs, "1.0"), requeue_budget = 3L, run_id = run))
  m1 <- go("r1")
  expect_identical(io$calls(), c("ant", "ape"))
  expect_identical(m1$remaining, 1L)                 # one queued row left in the budget
  m2 <- go("r1")
  expect_identical(io$calls(), c("ant", "ape", "arc"))
  expect_identical(m2$remaining, 0L)
  go("r1")
  expect_length(io$calls(), 3L)
  m4 <- go("r2")
  expect_identical(io$calls(), c("ant", "ape", "arc", "ash", "awl"))
  expect_identical(unlist(m4$requeue),
                   c(queued = 2L, measured = 2L, failed = 0L, left = 0L, capped = 0L))
})

test_that("a row that keeps failing stops being tried after REQUEUE_MAX_ATTEMPTS runs", {
  out <- requeue_fixture("alpha")
  io <- recording_io("alpha", status = "covr_error")
  for (i in seq_len(REQUEUE_MAX_ATTEMPTS + 1L)) {
    man <- suppressMessages(run_shard(io, out, requeue = rq("alpha", "1.0"),
                                      run_id = paste0("r", i)))
  }
  expect_length(io$calls(), REQUEUE_MAX_ATTEMPTS)
  expect_identical(unlist(man$requeue),
                   c(queued = 1L, measured = 0L, failed = 0L, left = 1L, capped = 1L))
})

test_that("queued rows are not measured without a record of the raw bundle fetch", {
  out <- requeue_fixture("alpha", prior = NULL)
  io <- recording_io("alpha")
  man <- suppressMessages(run_shard(io, out, requeue = rq("alpha", "1.0"), run_id = "r1"))
  expect_length(io$calls(), 0L)
  expect_identical(man$requeue$left, 1L)
})

test_that("normal work is unchanged when nothing is queued", {
  out <- requeue_fixture("alpha")
  io <- recording_io(c("alpha", "brand"))
  man <- suppressMessages(run_shard(io, out, run_id = "r1"))
  expect_identical(io$calls(), "brand")
  expect_null(man$requeue)
  con <- open_db(file.path(out, DB_FILENAME)); on.exit(DBI::dbDisconnect(con))
  expect_false(any(c("requeue_log", "requeue_runs") %in% DBI::dbListTables(con)))
})

test_that("a re-measured object reaches its shard bundle through the guarded upload", {
  pfx <- "covr-raw-s1-"
  # The release's c bundle holds cards but has lost casebase (both shard 1 of 4).
  rel <- tempfile("rel_"); dir.create(rel)
  src <- tempfile("src_"); dir.create(src)
  write_raw_object(file.path(src, "out", "raw"), "cards", "0.9.0", serialize("old", NULL))
  withr::with_dir(src, bundle_partitions("out/raw", rel, prefix = pfx))
  sizes <- stats::setNames(file.size(list.files(rel, full.names = TRUE)), list.files(rel))
  download <- function(names, dir) {
    file.copy(file.path(rel, names), file.path(dir, names), overwrite = TRUE)
    invisible(TRUE)
  }
  root <- tempfile("run_"); dir.create(root)
  withr::local_dir(root)
  st <- fetch_prior_bundles(sizes, "out/rawbundles", download, pause = function(n) NULL)
  saveRDS(st, "out/rawbundles/prior-state.rds")
  con <- open_db(file.path("out", DB_FILENAME))
  upsert_coverage(con, stored(c("cards", "casebase"), c("0.9.0", "0.10.7")), NULL, NULL)
  DBI::dbDisconnect(con)
  io <- recording_io(c("cards", "casebase"), c("0.9.0", "0.10.7"))
  man <- suppressMessages(run_shard(io, "out", slice = list(index = 1L, count = 4L),
    requeue = rq(c("cards", "casebase"), c("0.9.0", "0.10.7")), run_id = "r1"))
  expect_identical(io$calls(), "casebase")           # cards' object is already in the bundle
  plan <- suppressMessages(prepare_raw_upload("out/rawbundles/prior-state.rds", "out/raw",
    "out/bundle", pfx, 1L, 4L, download, pause = function(n) NULL))
  row <- plan[plan$name == paste0(pfx, "c.tar.gz"), ]
  expect_true(row$upload)
  expect_identical(row$reason, "prior objects all kept")
  expect_identical(c(row$prior, row$new), c(1L, 2L))
  expect_setequal(basename(bundle_members(file.path("out/bundle", row$name))),
                  c("cards_0.9.0.rds", "casebase_0.10.7.rds"))
})

# --- counts for the run summary and manifest ---------------------------------------

test_that("requeue_totals sums this run's counts over the shard databases", {
  mk <- function(rows) {
    p <- tempfile(fileext = ".db"); con <- open_db(p)
    for (r in rows) record_requeue_run(con, r$run, r$counts)
    DBI::dbDisconnect(con)
    p
  }
  cnt <- function(q, m, f, l, cp = 0L) list(queued = q, measured = m, failed = f,
                                            left = l, capped = cp)
  s0 <- mk(list(list(run = "r0", counts = cnt(9L, 1L, 0L, 8L)),
                list(run = "r1", counts = cnt(8L, 3L, 1L, 5L, 1L))))
  s1 <- mk(list(list(run = "r0", counts = cnt(4L, 2L, 0L, 2L))))   # no work this run
  s2 <- tempfile(fileext = ".db"); DBI::dbDisconnect(open_db(s2))  # nothing queued
  tot <- requeue_totals(c(s0, s1, s2, tempfile()), "r1")
  expect_identical(unlist(tot),
                   c(queued = 10L, measured = 3L, failed = 1L, left = 7L, capped = 1L))
  expect_null(requeue_totals(s2, "r1"))
})
