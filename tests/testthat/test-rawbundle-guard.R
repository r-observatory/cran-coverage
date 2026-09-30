# Prior raw bundles must be fetched intact before a rebuilt bundle may replace
# them on the release, and a rebuilt bundle may never drop an object for a
# package that is still in this shard.

PFX <- "covr-raw-s1-"

# Shard 1 of 4 holds cards, casebase, camtrapR, cpp11, caret, cowplot;
# cli and crayon partition to shard 0.
write_objects <- function(root, objects) {
  for (o in objects) {
    write_raw_object(file.path(root, "out", "raw"), sub("_.*$", "", o),
                     sub("^[^_]*_", "", o), serialize(o, NULL))
  }
}

# A fake internal release: a directory of bundles built the way the collect
# job builds them (relative out/raw paths), plus its name -> size listing.
make_release <- function(objects) {
  root <- tempfile("rel_"); dir.create(root)
  write_objects(root, objects)
  withr::with_dir(root, bundle_partitions("out/raw", "release", prefix = PFX))
  rel <- file.path(root, "release")
  files <- list.files(rel, full.names = TRUE)
  list(dir = rel, sizes = stats::setNames(file.size(files), basename(files)))
}

# Downloader standing in for `gh release download`: copies the named assets,
# except those listed in `fail` (left absent) or `truncate` (half written, as
# gh leaves a file whose transfer broke), for the first `bad_calls` calls.
fake_download <- function(rel_dir, fail = character(0), truncate = character(0),
                          bad_calls = Inf) {
  calls <- 0L
  function(names, dir) {
    calls <<- calls + 1L
    bad <- calls <= bad_calls
    for (n in names) {
      src <- file.path(rel_dir, n)
      if (bad && n %in% fail) next
      if (bad && n %in% truncate) {
        b <- readBin(src, "raw", file.size(src))
        writeBin(b[seq_len(length(b) %/% 2L)], file.path(dir, n))
        next
      }
      file.copy(src, file.path(dir, n), overwrite = TRUE)
    }
    if (bad && any(names %in% c(fail, truncate))) stop("HTTP 502")
    invisible(TRUE)
  }
}

no_pause <- function(attempt) invisible(NULL)

test_that("bundle_members lists objects and rejects a missing, short or corrupt file", {
  rel <- make_release(c("cards_0.9.0", "casebase_0.10.7"))
  f <- file.path(rel$dir, paste0(PFX, "c.tar.gz"))
  expect_setequal(basename(bundle_members(f, rel$sizes[[basename(f)]])),
                  c("cards_0.9.0.rds", "casebase_0.10.7.rds"))
  expect_null(bundle_members(file.path(rel$dir, "absent.tar.gz")))
  expect_null(bundle_members(f, rel$sizes[[basename(f)]] + 1))
  short <- tempfile(fileext = ".tar.gz")
  writeBin(readBin(f, "raw", file.size(f))[1:100], short)
  expect_null(bundle_members(short))
})

test_that("a new shard with no prior bundle on the release uploads as before", {
  root <- tempfile("run_"); dir.create(root)
  withr::local_dir(root)
  dl <- function(names, dir) stop("nothing should be downloaded")
  st <- fetch_prior_bundles(stats::setNames(numeric(0), character(0)),
                            "out/rawbundles", dl, pause = no_pause)
  expect_length(st$unavailable, 0L)
  expect_length(st$members, 0L)
  write_objects(".", c("cards_0.9.0", "cpp11_0.5.2"))
  bundle_partitions("out/raw", "out/bundle", prefix = PFX)
  plan <- plan_raw_uploads("out/bundle", st, 1L, 4L, PFX)
  expect_true(all(plan$upload))
  expect_setequal(plan$name, paste0(PFX, "c.tar.gz"))
  expect_identical(plan$prior[1], 0L)
  expect_identical(plan$new[1], 2L)
})

test_that("a successful download is extracted and a grown bundle uploads", {
  rel <- make_release(c("cards_0.9.0", "casebase_0.10.7", "dplyr_1.1.4"))
  root <- tempfile("run_"); dir.create(root)
  withr::local_dir(root)
  st <- fetch_prior_bundles(rel$sizes, "out/rawbundles", fake_download(rel$dir),
                            pause = no_pause)
  expect_length(st$unavailable, 0L)
  expect_setequal(names(st$members), names(rel$sizes))
  expect_true(file.exists("out/raw/c/cards_0.9.0.rds"))
  expect_true(file.exists("out/raw/d/dplyr_1.1.4.rds"))
  write_objects(".", "cowplot_1.2.0")
  bundle_partitions("out/raw", "out/bundle", prefix = PFX)
  plan <- plan_raw_uploads("out/bundle", st, 1L, 4L, PFX)
  c_row <- plan[plan$name == paste0(PFX, "c.tar.gz"), ]
  expect_true(c_row$upload)
  expect_identical(c(c_row$prior, c_row$new, c_row$dropped), c(2L, 3L, 0L))
  expect_true(all(plan$upload))
})

test_that("a download that keeps failing is not extracted and its bundle is held back", {
  rel <- make_release(c("cards_0.9.0", "casebase_0.10.7", "dplyr_1.1.4"))
  c_name <- paste0(PFX, "c.tar.gz"); d_name <- paste0(PFX, "d.tar.gz")
  root <- tempfile("run_"); dir.create(root)
  withr::local_dir(root)
  pauses <- 0L
  st <- fetch_prior_bundles(rel$sizes, "out/rawbundles",
                            fake_download(rel$dir, truncate = c_name),
                            attempts = 3L, pause = function(a) pauses <<- pauses + 1L)
  expect_identical(pauses, 2L)
  expect_identical(st$unavailable, c_name)
  expect_setequal(names(st$members), d_name)
  # A truncated file is never partially extracted.
  expect_false(dir.exists("out/raw/c"))
  # This run measures one more c package; its rebuilt c bundle would hold only
  # that object, so it must not replace the intact one on the release.
  write_objects(".", "cowplot_1.2.0")
  bundle_partitions("out/raw", "out/bundle", prefix = PFX)
  plan <- plan_raw_uploads("out/bundle", st, 1L, 4L, PFX)
  expect_false(plan$upload[plan$name == c_name])
  expect_match(plan$reason[plan$name == c_name], "could not be downloaded")
  expect_true(plan$upload[plan$name == d_name])
})

test_that("a download that fails once is retried and then used", {
  rel <- make_release(c("cards_0.9.0", "casebase_0.10.7"))
  c_name <- paste0(PFX, "c.tar.gz")
  root <- tempfile("run_"); dir.create(root)
  withr::local_dir(root)
  st <- fetch_prior_bundles(rel$sizes, "out/rawbundles",
                            fake_download(rel$dir, fail = c_name, bad_calls = 1L),
                            pause = no_pause)
  expect_length(st$unavailable, 0L)
  expect_true(file.exists("out/raw/c/casebase_0.10.7.rds"))
})

test_that("a rebuilt bundle missing objects for packages still in the shard is refused", {
  rel <- make_release(c("cards_0.9.0", "casebase_0.10.7", "camtrapR_3.1.0", "caret_7.0-1"))
  c_name <- paste0(PFX, "c.tar.gz")
  root <- tempfile("run_"); dir.create(root)
  withr::local_dir(root)
  st <- fetch_prior_bundles(rel$sizes, "out/rawbundles", fake_download(rel$dir),
                            pause = no_pause)
  # Lose three extracted objects, then measure one new package: the rebuilt
  # bundle (2 objects) would replace one holding 4.
  unlink(c("out/raw/c/casebase_0.10.7.rds", "out/raw/c/camtrapR_3.1.0.rds",
           "out/raw/c/caret_7.0-1.rds"))
  write_objects(".", "cowplot_1.2.0")
  bundle_partitions("out/raw", "out/bundle", prefix = PFX)
  plan <- plan_raw_uploads("out/bundle", st, 1L, 4L, PFX)
  row <- plan[plan$name == c_name, ]
  expect_false(row$upload)
  expect_identical(c(row$prior, row$new, row$dropped, row$left_shard), c(4L, 2L, 3L, 0L))
  expect_match(row$reason, "still in this shard")
})

test_that("a same-size swap that loses an in-shard object is refused too", {
  rel <- make_release(c("cards_0.9.0", "casebase_0.10.7"))
  c_name <- paste0(PFX, "c.tar.gz")
  root <- tempfile("run_"); dir.create(root)
  withr::local_dir(root)
  st <- fetch_prior_bundles(rel$sizes, "out/rawbundles", fake_download(rel$dir),
                            pause = no_pause)
  unlink("out/raw/c/casebase_0.10.7.rds")
  write_objects(".", "cowplot_1.2.0")
  bundle_partitions("out/raw", "out/bundle", prefix = PFX)
  plan <- plan_raw_uploads("out/bundle", st, 1L, 4L, PFX)
  expect_false(plan$upload[plan$name == c_name])
})

test_that("dropping only objects for packages that left the shard is allowed", {
  rel <- make_release(c("cards_0.9.0", "cli_3.6.3", "crayon_1.5.3"))
  c_name <- paste0(PFX, "c.tar.gz")
  root <- tempfile("run_"); dir.create(root)
  withr::local_dir(root)
  st <- fetch_prior_bundles(rel$sizes, "out/rawbundles", fake_download(rel$dir),
                            pause = no_pause)
  unlink(c("out/raw/c/cli_3.6.3.rds", "out/raw/c/crayon_1.5.3.rds"))
  bundle_partitions("out/raw", "out/bundle", prefix = PFX)
  plan <- plan_raw_uploads("out/bundle", st, 1L, 4L, PFX)
  row <- plan[plan$name == c_name, ]
  expect_true(row$upload)
  expect_identical(c(row$prior, row$new, row$dropped, row$left_shard), c(3L, 1L, 2L, 2L))
})

test_that("prepare_raw_upload retries a held-back prior, and uploads nothing without a record", {
  rel <- make_release(c("cards_0.9.0", "casebase_0.10.7", "dplyr_1.1.4"))
  c_name <- paste0(PFX, "c.tar.gz"); d_name <- paste0(PFX, "d.tar.gz")
  root <- tempfile("run_"); dir.create(root)
  withr::local_dir(root)
  # Fetch fails for c at the start of the run...
  st <- fetch_prior_bundles(rel$sizes, "out/rawbundles",
                            fake_download(rel$dir, fail = c_name),
                            attempts = 2L, pause = no_pause)
  expect_identical(st$unavailable, c_name)
  saveRDS(st, "out/rawbundles/prior-state.rds")
  write_objects(".", "cowplot_1.2.0")
  # ...and succeeds when retried before the upload, so the new object and the
  # prior ones are uploaded together.
  plan <- prepare_raw_upload("out/rawbundles/prior-state.rds", "out/raw", "out/bundle",
                             PFX, 1L, 4L, fake_download(rel$dir), pause = no_pause)
  row <- plan[plan$name == c_name, ]
  expect_true(row$upload)
  expect_identical(c(row$prior, row$new), c(2L, 3L))
  expect_setequal(basename(bundle_members(file.path("out/bundle", c_name))),
                  c("cards_0.9.0.rds", "casebase_0.10.7.rds", "cowplot_1.2.0.rds"))

  # With no record of what was fetched, nothing is uploaded.
  plan2 <- prepare_raw_upload("out/rawbundles/absent.rds", "out/raw", "out/bundle",
                              PFX, 1L, 4L, fake_download(rel$dir), pause = no_pause)
  expect_true(nrow(plan2) > 0L)
  expect_false(any(plan2$upload))
})

test_that("shard_bundle_sizes reads this shard's bundles from the release listing", {
  f <- tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(list(assets = data.frame(
    name = c("covr-raw-s1-a.tar.gz", "covr-raw-s1-c.tar.gz", "covr-raw-s10-a.tar.gz",
             "covr-raw-a.tar.gz", "cran-coverage-shard-1.db"),
    size = c(10, 20, 30, 40, 50)))), f)
  expect_identical(shard_bundle_sizes(f, PFX),
                   c("covr-raw-s1-a.tar.gz" = 10, "covr-raw-s1-c.tar.gz" = 20))
  g <- tempfile(fileext = ".json")
  writeLines('{"assets":[]}', g)
  expect_length(shard_bundle_sizes(g, PFX), 0L)
})
