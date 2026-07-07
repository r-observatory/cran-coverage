# Tests for the resumable shard-selection logic: popularity ordering,
# bounded failure retry, and matrix partition slicing.

# --- package_partition -------------------------------------------------------

test_that("package_partition is deterministic and within range", {
  expect_identical(package_partition("dplyr", 8L), package_partition("dplyr", 8L))
  vals <- package_partition(c("a", "ggplot2", "zzz", "Rcpp"), 8L)
  expect_true(all(vals >= 0L & vals < 8L))
  expect_length(vals, 4L)
})

test_that("package_partition spreads packages across all buckets", {
  names <- sprintf("pkg%04d", seq_len(2000))
  buckets <- package_partition(names, 8L)
  expect_setequal(sort(unique(buckets)), 0:7)          # every bucket used
  counts <- tabulate(buckets + 1L, nbins = 8L)
  expect_lt(max(counts) / min(counts), 1.5)            # roughly balanced
})

# --- retry bookkeeping -------------------------------------------------------

test_that("is_retryable is true only for transient-failure statuses", {
  expect_true(all(is_retryable(c("build_fail", "covr_error", "timeout"))))
  expect_false(any(is_retryable(c("ok", "no_tests", "test_error"))))
})

test_that("next_attempts increments only on retryable outcomes and treats NA as 0", {
  expect_equal(next_attempts(0L, "covr_error"), 1L)
  expect_equal(next_attempts(2L, "timeout"), 3L)
  expect_equal(next_attempts(NA_integer_, "build_fail"), 1L)
  expect_equal(next_attempts(2L, "ok"), 2L)            # success does not bump
  expect_equal(next_attempts(NA_integer_, "no_tests"), 0L)
})

# --- select_shard ------------------------------------------------------------

empty_state <- function() {
  data.frame(package = character(0), version = character(0),
             covr_status = character(0), attempts = integer(0),
             stringsAsFactors = FALSE)
}

test_that("select_shard picks packages with no row or a stale version", {
  uni <- data.frame(package = c("a", "b", "c"), latest_version = c("1", "2", "3"),
                    stringsAsFactors = FALSE)
  state <- data.frame(package = "a", version = "1", covr_status = "ok",
                      attempts = 0L, stringsAsFactors = FALSE)
  expect_setequal(select_shard(uni, state, 10L), c("b", "c"))  # a is done
  # a's latest bumps to a new version -> a becomes todo again
  uni2 <- transform(uni, latest_version = ifelse(package == "a", "9", latest_version))
  expect_true("a" %in% select_shard(uni2, state, 10L))
})

test_that("select_shard treats no_tests and test_error as terminal", {
  uni <- data.frame(package = c("a", "b"), latest_version = c("1", "1"),
                    stringsAsFactors = FALSE)
  state <- data.frame(package = c("a", "b"), version = c("1", "1"),
                      covr_status = c("no_tests", "test_error"),
                      attempts = c(0L, 0L), stringsAsFactors = FALSE)
  expect_length(select_shard(uni, state, 10L), 0L)
})

test_that("select_shard re-attempts transient failures under the cap and stops at it", {
  uni <- data.frame(package = c("a", "b"), latest_version = c("1", "1"),
                    stringsAsFactors = FALSE)
  under <- data.frame(package = "a", version = "1", covr_status = "covr_error",
                      attempts = MAX_ATTEMPTS - 1L, stringsAsFactors = FALSE)
  expect_true("a" %in% select_shard(uni, under, 10L))
  atcap <- data.frame(package = "a", version = "1", covr_status = "covr_error",
                      attempts = MAX_ATTEMPTS, stringsAsFactors = FALSE)
  expect_false("a" %in% select_shard(uni, atcap, 10L))
})

test_that("select_shard orders by popularity rank, then alphabetically", {
  uni <- data.frame(package = c("aaa", "zzz", "ggplot2"),
                    latest_version = c("1", "1", "1"), stringsAsFactors = FALSE)
  rank <- c("ggplot2", "zzz")                 # aaa is unranked -> last
  expect_equal(select_shard(uni, empty_state(), 3L, rank = rank),
               c("ggplot2", "zzz", "aaa"))
})

test_that("select_shard prioritizes never-attempted work over re-attempts", {
  uni <- data.frame(package = c("fresh", "flaky"), latest_version = c("1", "1"),
                    stringsAsFactors = FALSE)
  state <- data.frame(package = "flaky", version = "1", covr_status = "timeout",
                      attempts = 1L, stringsAsFactors = FALSE)
  # both are todo, but the never-attempted one comes first
  expect_equal(select_shard(uni, state, 1L), "fresh")
})

test_that("select_shard restricts to its matrix partition", {
  uni <- data.frame(package = sprintf("pkg%03d", 1:200),
                    latest_version = rep("1", 200), stringsAsFactors = FALSE)
  got <- select_shard(uni, empty_state(), 1000L,
                      slice = list(index = 3L, count = 8L))
  expect_true(all(package_partition(got, 8L) == 3L))
  expect_gt(length(got), 0L)
  # the union of all partitions covers the whole universe exactly once
  all_parts <- unlist(lapply(0:7, function(i)
    select_shard(uni, empty_state(), 1000L, slice = list(index = i, count = 8L))))
  expect_setequal(all_parts, uni$package)
})

# --- subset_partition (matrix cutover / self-heal seeding) -------------------

test_that("subset_partition keeps only its partition's packages and their rows", {
  src <- tempfile(fileext = ".db"); on.exit(unlink(src))
  con <- open_db(src)
  pk <- sprintf("p%03d", 1:120)
  upsert_coverage(con,
    data.frame(package = pk, version = "1", covr_status = "ok",
               attempts = 0L, stringsAsFactors = FALSE),
    data.frame(package = pk, version = "1", file = "R/x.R",
               lines_total = 1, lines_covered = 1, coverage_pct = 100),
    NULL)
  DBI::dbDisconnect(con)
  dst <- tempfile(fileext = ".db"); on.exit(unlink(dst), add = TRUE)
  n <- subset_partition(src, dst, index = 2L, count = 8L)
  con2 <- open_db(dst); on.exit(DBI::dbDisconnect(con2), add = TRUE)
  got <- DBI::dbGetQuery(con2, "SELECT package FROM coverage_summary")$package
  expect_true(all(package_partition(got, 8L) == 2L))       # only partition 2
  expect_equal(n, length(got))
  frows <- DBI::dbGetQuery(con2, "SELECT DISTINCT package FROM coverage_file")$package
  expect_setequal(frows, got)                              # its file rows came along
})

# --- popularity_rank ---------------------------------------------------------

test_that("popularity_rank reads the ranked list and skips comments", {
  f <- tempfile(fileext = ".txt"); on.exit(unlink(f))
  writeLines(c("# a comment", "", "ggplot2", "dplyr", "  Rcpp  "), f)
  expect_equal(popularity_rank(f), c("ggplot2", "dplyr", "Rcpp"))
  expect_identical(popularity_rank(tempfile()), character(0))  # absent -> empty
})
