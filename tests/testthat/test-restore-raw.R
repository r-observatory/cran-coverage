# The one-off restore of raw covr objects from covr-raw-a.tar.gz into the
# covr-raw-s<i>-a.tar.gz bundles, run against a fake gh and a fake release.

RS_REPO <- "r-observatory/cran-coverage"

# Shards of 4: ade4, arm, askpass, assertthat -> 0; alabama, amap, aws -> 1.
rs_bundle <- function(objects, prefix) {
  d <- tempfile("objs_"); dir.create(d)
  for (o in objects) {
    write_raw_object(file.path(d, "out", "raw"), sub("_.*$", "", o),
                     sub("^[^_]*_", "", o), serialize(o, NULL))
  }
  withr::with_dir(d, bundle_partitions("out/raw", "bundles", prefix = prefix))
  file.path(d, "bundles", paste0(prefix, "a.tar.gz"))
}

# A fake internal release (assets kept by id so a rename keeps the bytes) and a
# gh stand-in that records every call. `fail` names operations that exit 1:
# "upload", "DELETE", "PATCH"; "PATCH-noop" exits 0 without renaming.
# `bad_digest` names assets whose listed sha256 is wrong.
fake_release <- function(files, fail = character(0), bad_digest = character(0)) {
  dir <- tempfile("rel_"); dir.create(dir)
  st <- new.env()
  st$calls <- list(); st$next_id <- 500L
  st$assets <- data.frame(id = character(0), name = character(0),
                          stringsAsFactors = FALSE)
  add <- function(path, name) {
    st$next_id <- st$next_id + 1L
    id <- as.character(st$next_id)
    file.copy(path, file.path(dir, id))
    st$assets <- rbind(st$assets, data.frame(id = id, name = name,
                                             stringsAsFactors = FALSE))
    id
  }
  for (n in names(files)) add(files[[n]], n)
  res <- function(status = 0L, out = character(0)) list(status = status, out = out)
  listing <- function() {
    a <- st$assets
    if (nrow(a) == 0L) return(character(0))
    f <- file.path(dir, a$id)
    dg <- paste0("sha256:", vapply(f, file_sha256, ""))
    dg[a$name %in% bad_digest] <- paste0("sha256:", strrep("0", 64))
    paste(a$id, a$name, file.size(f), "uploaded", dg, sep = "\t")
  }
  gh <- function(args) {
    st$calls[[length(st$calls) + 1L]] <- args
    # Outside a checkout gh release needs the repository named.
    if (args[1] == "release" && !identical(args[which(args == "--repo") + 1L], RS_REPO)) {
      return(res(1L))
    }
    if (identical(args[1:2], c("api", sprintf("repos/%s/releases/tags/internal", RS_REPO)))) {
      return(res(out = "77"))
    }
    if (identical(args[1:2], c("api", "--paginate"))) return(res(out = listing()))
    if (identical(args[1:2], c("release", "download"))) {
      d <- args[which(args == "--dir") + 1L]
      hit <- st$assets[st$assets$name %in% args[which(args == "--pattern") + 1L], ]
      for (i in seq_len(nrow(hit))) {
        file.copy(file.path(dir, hit$id[i]), file.path(d, hit$name[i]))
      }
      return(res(if (nrow(hit)) 0L else 1L))
    }
    if (identical(args[1:2], c("release", "upload"))) {
      if ("upload" %in% fail || basename(args[4]) %in% st$assets$name) return(res(1L))
      add(args[4], basename(args[4]))
      return(res())
    }
    if (identical(args[1:3], c("api", "-X", "DELETE"))) {
      id <- basename(args[4])
      if ("DELETE" %in% fail || !id %in% st$assets$id) return(res(1L))
      st$assets <- st$assets[st$assets$id != id, ]
      return(res())
    }
    if (identical(args[1:3], c("api", "-X", "PATCH"))) {
      if ("PATCH" %in% fail) return(res(1L))
      if ("PATCH-noop" %in% fail) return(res())
      nm <- sub("^name=", "", args[6])
      if (nm %in% st$assets$name) return(res(1L))
      st$assets$name[st$assets$id == basename(args[4])] <- nm
      return(res())
    }
    stop("fake gh: unexpected call: ", paste(args, collapse = " "))
  }
  list(gh = gh, calls = function() st$calls, assets = function() st$assets,
       file = function(name) file.path(dir, st$assets$id[st$assets$name == name]),
       id = function(name) st$assets$id[st$assets$name == name])
}

# The old bundle, shard bundles s0-a and s1-a, and a manifest of the objects
# the shard bundles lack. askpass is in both the old bundle and s0-a, so it is
# not in the manifest; alabama 2022.4-1 in s1-a stands for an object collected
# after the old bundle was made.
rs_setup <- function(s0 = c("askpass_1.2.1", "assertthat_0.2.1"),
                     s1 = c("aws_0.6-2", "alabama_2022.4-1"),
                     extra = list(), ...) {
  restore <- c("ade4_1.7-22", "arm_1.14-4", "alabama_2023.1.0", "amap_0.8-20")
  old <- rs_bundle(c(restore, "askpass_1.2.1"), "covr-raw-")
  files <- c(list("covr-raw-a.tar.gz" = old,
                  "covr-raw-s0-a.tar.gz" = rs_bundle(s0, "covr-raw-s0-"),
                  "covr-raw-s1-a.tar.gz" = rs_bundle(s1, "covr-raw-s1-"),
                  "cran-coverage-shard-0.db" = old), extra)
  src <- file.path(dirname(dirname(old)), "out", "raw", "a", paste0(restore, ".rds"))
  man <- data.frame(shard = c("0", "0", "1", "1"),
                    bundle = rep(c("covr-raw-s0-a.tar.gz", "covr-raw-s1-a.tar.gz"), each = 2),
                    package = sub("_.*$", "", restore),
                    version = sub("^[^_]*_", "", restore), covr_status = "ok",
                    member = paste0("out/raw/a/", restore, ".rds"),
                    md5 = unname(tools::md5sum(src)), stringsAsFactors = FALSE)
  root <- tempfile("run_"); dir.create(root)
  mpath <- file.path(root, "manifest.tsv")
  utils::write.table(man, mpath, sep = "\t", quote = FALSE, row.names = FALSE)
  list(root = root, manifest = mpath, rel = fake_release(files, ...),
       old = list(name = "covr-raw-a.tar.gz", size = file.size(old),
                  sha256 = file_sha256(old)))
}

rs_run <- function(s, shards = 0:1, ...) {
  restore_main(s$rel$gh, repo = RS_REPO, tag = "internal", shards = shards,
               manifest_path = s$manifest, old = s$old, summary_path = "summary.md",
               pause = function(n) invisible(NULL), ...)
}

rs_writes <- function(calls) {
  Filter(function(a) identical(a[1:2], c("release", "upload")) ||
           (identical(a[1:2], c("api", "-X")) && a[3] %in% c("DELETE", "PATCH")), calls)
}

rs_members <- function(path) sort(basename(bundle_members(path)))

rs_list_call <- function() {
  c("api", "--paginate", sprintf("repos/%s/releases/77/assets?per_page=100", RS_REPO),
    "--jq", RESTORE_ASSET_JQ)
}

test_that("the committed manifest agrees with the shard layout", {
  m <- read_restore_manifest(test_path("..", "..", "data", "restore", "restore-manifest.tsv"))
  expect_identical(nrow(m), 368L)
  expect_identical(as.vector(table(m$shard)), c(104L, 87L, 81L, 96L))
  bad <- m
  bad$shard[1] <- "1"
  f <- tempfile(fileext = ".tsv")
  utils::write.table(bad, f, sep = "\t", quote = FALSE, row.names = FALSE)
  expect_error(read_restore_manifest(f), "shard layout")
})

test_that("parse_restore_shards reads the dispatch input", {
  expect_identical(parse_restore_shards("0 1 2 3"), 0:3)
  expect_identical(parse_restore_shards("2,3"), 2:3)
  expect_identical(parse_restore_shards(" 1 "), 1L)
  expect_error(parse_restore_shards("4"), "shard")
  expect_error(parse_restore_shards("1 1"), "shard")
  expect_error(parse_restore_shards(""), "shard")
})

test_that("the count rule wants prior objects all kept plus the manifest rows", {
  plan <- function(name = "covr-raw-s0-a.tar.gz", prior = 94L, new = 198L,
                   upload = TRUE, reason = "prior objects all kept") {
    data.frame(name = name, prior = prior, new = new, dropped = 0L, left_shard = 0L,
               upload = upload, held = !upload, reason = reason,
               stringsAsFactors = FALSE)
  }
  expect_true(check_restore_count(plan(), "covr-raw-s0-a.tar.gz", 94L, 104L))
  # Objects collected since the old bundle count toward the fetched prior.
  expect_true(check_restore_count(plan("covr-raw-s3-a.tar.gz", 93L, 189L),
                                  "covr-raw-s3-a.tar.gz", 93L, 96L))
  expect_error(check_restore_count(plan(new = 197L), "covr-raw-s0-a.tar.gz", 94L, 104L),
               "expected 198")
  expect_error(check_restore_count(plan(prior = 95L, new = 199L), "covr-raw-s0-a.tar.gz",
                                   94L, 104L), "expected 198")
  expect_error(check_restore_count(plan(reason = "new bundle"), "covr-raw-s0-a.tar.gz",
                                   94L, 104L), "new bundle")
  expect_error(check_restore_count(plan(new = 94L, upload = FALSE,
                                        reason = "unchanged since the prior copy"),
                                   "covr-raw-s0-a.tar.gz", 94L, 104L), "unchanged")
  expect_error(check_restore_count(rbind(plan(), plan("covr-raw-s0-b.tar.gz")),
                                   "covr-raw-s0-a.tar.gz", 94L, 104L), "one rebuilt bundle")
})

test_that("the happy path swaps each bundle with exactly the expected gh commands", {
  s <- rs_setup()
  withr::local_dir(s$root)
  old0 <- s$rel$id("covr-raw-s0-a.tar.gz"); old1 <- s$rel$id("covr-raw-s1-a.tar.gz")
  old_a <- s$rel$id("covr-raw-a.tar.gz")
  expect_identical(rs_run(s), 0L)
  new0 <- s$rel$id("covr-raw-s0-a.tar.gz"); new1 <- s$rel$id("covr-raw-s1-a.tar.gz")
  asset <- function(id) sprintf("repos/%s/releases/assets/%s", RS_REPO, id)
  expect_identical(s$rel$calls(), list(
    c("api", sprintf("repos/%s/releases/tags/internal", RS_REPO), "--jq", ".id"),
    rs_list_call(),
    c("release", "download", "internal", "--repo", RS_REPO, "--dir", "out/restore/old",
      "--pattern", "covr-raw-a.tar.gz"),
    c("release", "download", "internal", "--repo", RS_REPO, "--dir", "rawbundles",
      "--pattern", "covr-raw-s0-a.tar.gz"),
    c("release", "download", "internal", "--repo", RS_REPO, "--dir", "rawbundles",
      "--pattern", "covr-raw-s1-a.tar.gz"),
    c("release", "upload", "internal", "out/unsent/restore-s0-a.tgz", "--repo", RS_REPO),
    rs_list_call(),
    c("api", "-X", "DELETE", asset(old0)),
    c("api", "-X", "PATCH", asset(new0), "-f", "name=covr-raw-s0-a.tar.gz"),
    rs_list_call(),
    c("release", "upload", "internal", "out/unsent/restore-s1-a.tgz", "--repo", RS_REPO),
    rs_list_call(),
    c("api", "-X", "DELETE", asset(old1)),
    c("api", "-X", "PATCH", asset(new1), "-f", "name=covr-raw-s1-a.tar.gz"),
    rs_list_call()))
  expect_setequal(s$rel$assets()$name, c("covr-raw-a.tar.gz", "covr-raw-s0-a.tar.gz",
                                         "covr-raw-s1-a.tar.gz", "cran-coverage-shard-0.db"))
  # The old bundle is never deleted.
  expect_identical(s$rel$id("covr-raw-a.tar.gz"), old_a)
  expect_identical(rs_members(s$rel$file("covr-raw-s0-a.tar.gz")),
                   c("ade4_1.7-22.rds", "arm_1.14-4.rds", "askpass_1.2.1.rds",
                     "assertthat_0.2.1.rds"))
  expect_identical(rs_members(s$rel$file("covr-raw-s1-a.tar.gz")),
                   c("alabama_2022.4-1.rds", "alabama_2023.1.0.rds", "amap_0.8-20.rds",
                     "aws_0.6-2.rds"))
  expect_length(list.files("out/unsent"), 0L)
  sm <- readLines("summary.md")
  expect_true("| covr-raw-s0-a.tar.gz | 2 | 2 | 4 | swapped |" %in% sm)
  expect_true("| covr-raw-s1-a.tar.gz | 2 | 2 | 4 | swapped |" %in% sm)
})

test_that("covr-raw-a.tar.gz that does not match its size and sha256 stops the restore", {
  s <- rs_setup()
  withr::local_dir(s$root)
  s$old$sha256 <- strrep("0", 64)
  expect_identical(rs_run(s), 1L)
  expect_length(rs_writes(s$rel$calls()), 0L)
  expect_true(any(grepl("covr-raw-a.tar.gz does not match", readLines("summary.md"))))
  s2 <- rs_setup()
  withr::local_dir(s2$root)
  s2$old$size <- s2$old$size + 1
  expect_identical(rs_run(s2), 1L)
  expect_length(rs_writes(s2$rel$calls()), 0L)
})

test_that("a shard bundle that does not match its listed sha256 stops the restore", {
  s <- rs_setup(bad_digest = "covr-raw-s1-a.tar.gz")
  withr::local_dir(s$root)
  before <- s$rel$assets()
  expect_identical(rs_run(s), 1L)
  # Shard 0 checked out, but nothing is written until every shard has.
  expect_length(rs_writes(s$rel$calls()), 0L)
  expect_identical(s$rel$assets(), before)
  expect_true(any(grepl("covr-raw-s1-a.tar.gz could not be fetched", readLines("summary.md"))))
})

test_that("a manifest object already in the shard's store stops the restore", {
  s <- rs_setup(s0 = c("askpass_1.2.1", "assertthat_0.2.1", "arm_1.14-4"))
  withr::local_dir(s$root)
  expect_identical(rs_run(s), 1L)
  expect_length(rs_writes(s$rel$calls()), 0L)
  expect_true(any(grepl("arm_1.14-4.rds", readLines("summary.md"))))
})

test_that("a restored object whose md5 differs from the manifest stops the restore", {
  s <- rs_setup()
  withr::local_dir(s$root)
  m <- utils::read.delim(s$manifest, colClasses = "character")
  m$md5[3] <- strrep("f", 32)
  utils::write.table(m, s$manifest, sep = "\t", quote = FALSE, row.names = FALSE)
  expect_identical(rs_run(s), 1L)
  expect_length(rs_writes(s$rel$calls()), 0L)
  expect_true(any(grepl("md5", readLines("summary.md"))))
})

test_that("a rebuilt bundle holding more than the prior plus the restored objects stops the restore", {
  s <- rs_setup()
  withr::local_dir(s$root)
  # A stray object in shard 1's store: the guard still plans an upload with
  # prior objects all kept, but the count is one over.
  orig <- prepare_raw_upload
  withr::defer(assign("prepare_raw_upload", orig, envir = globalenv()))
  assign("prepare_raw_upload", function(state_path, raw_dir, ...) {
    if (grepl("s1$", getwd())) write_raw_object(raw_dir, "aws", "0.1", serialize("stray", NULL))
    orig(state_path, raw_dir, ...)
  }, envir = globalenv())
  expect_identical(rs_run(s), 1L)
  expect_length(rs_writes(s$rel$calls()), 0L)
  expect_true(any(grepl("expected 4 (2 prior + 2 restored)", readLines("summary.md"), fixed = TRUE)))
})

test_that("a temporary name already on the release stops the restore before any write", {
  f <- tempfile(); writeLines("left over", f)
  s <- rs_setup(extra = list("restore-s1-a.tgz" = f))
  withr::local_dir(s$root)
  expect_identical(rs_run(s), 1L)
  expect_length(rs_writes(s$rel$calls()), 0L)
})

test_that("a dry run builds and checks every bundle but writes nothing", {
  s <- rs_setup()
  withr::local_dir(s$root)
  expect_identical(rs_run(s, dry_run = TRUE), 0L)
  expect_length(rs_writes(s$rel$calls()), 0L)
  sm <- readLines("summary.md")
  expect_true(any(grepl("Dry run", sm)))
  expect_true("| covr-raw-s0-a.tar.gz | 2 | 2 | 4 | dry run, not uploaded |" %in% sm)
})

test_that("a failed upload deletes nothing", {
  s <- rs_setup(fail = "upload")
  withr::local_dir(s$root)
  before <- s$rel$assets()
  expect_identical(rs_run(s), 1L)
  expect_identical(s$rel$assets(), before)
  expect_false(any(vapply(s$rel$calls(), function(a) a[3] %in% c("DELETE", "PATCH"), TRUE)))
  sm <- readLines("summary.md")
  expect_true("| covr-raw-s0-a.tar.gz | 2 | 2 |  | unchanged, stopped before the delete |" %in% sm)
  expect_true(any(grepl("Dispatch again with shards '0 1'", sm, fixed = TRUE)))
})

test_that("a failed rename after the delete leaves the temporary asset and fails with the recovery command", {
  s <- rs_setup(fail = "PATCH")
  withr::local_dir(s$root)
  old0 <- s$rel$id("covr-raw-s0-a.tar.gz")
  expect_identical(rs_run(s), 1L)
  calls <- vapply(s$rel$calls(), paste, "", collapse = " ")
  up <- grep("^release upload internal out/unsent/restore-s0-a.tgz --repo ", calls)
  del <- grep(" -X DELETE ", calls)
  patch <- grep(" -X PATCH ", calls)
  expect_length(up, 1L); expect_length(del, 1L)
  expect_true(length(patch) >= 1L)
  # upload, then a listing that verifies it, then delete, then rename.
  expect_identical(calls[up + 1L], paste(rs_list_call(), collapse = " "))
  expect_true(up + 1L < del && del < min(patch))
  # The rebuilt bundle stays on the release under its temporary name and in
  # out/unsent for the artifact; shard 1 is left alone.
  tmp <- s$rel$id("restore-s0-a.tgz")
  expect_length(tmp, 1L)
  expect_false(old0 %in% s$rel$assets()$id)
  expect_identical(file_sha256(s$rel$file("restore-s0-a.tgz")),
                   file_sha256("out/unsent/restore-s0-a.tgz"))
  expect_false(any(grepl("restore-s1-a.tgz", calls)))
  expect_true("covr-raw-a.tar.gz" %in% s$rel$assets()$name)
  fix <- sprintf("gh api -X PATCH repos/%s/releases/assets/%s -f name=covr-raw-s0-a.tar.gz",
                 RS_REPO, tmp)
  sm <- readLines("summary.md")
  expect_true(any(grepl(fix, sm, fixed = TRUE)))
  # Once recovered, shard 0 is done; only shard 1 needs another dispatch.
  expect_true(any(grepl("Dispatch again with shards '1'", sm, fixed = TRUE)))
})

test_that("a rename the release does not show fails with the recovery command", {
  s <- rs_setup(fail = "PATCH-noop")
  withr::local_dir(s$root)
  expect_identical(rs_run(s), 1L)
  tmp <- s$rel$id("restore-s0-a.tgz")
  expect_length(tmp, 1L)
  expect_true(file.exists("out/unsent/restore-s0-a.tgz"))
  fix <- sprintf("gh api -X PATCH repos/%s/releases/assets/%s -f name=covr-raw-s0-a.tar.gz",
                 RS_REPO, tmp)
  expect_true(any(grepl(fix, readLines("summary.md"), fixed = TRUE)))
})

test_that("a failed delete leaves the temporary asset and names both recovery commands", {
  s <- rs_setup(fail = "DELETE")
  withr::local_dir(s$root)
  old0 <- s$rel$id("covr-raw-s0-a.tar.gz")
  expect_identical(rs_run(s), 1L)
  tmp <- s$rel$id("restore-s0-a.tgz")
  expect_length(tmp, 1L)
  expect_false(any(vapply(s$rel$calls(), function(a) identical(a[3], "PATCH"), TRUE)))
  sm <- paste(readLines("summary.md"), collapse = "\n")
  expect_match(sm, sprintf("gh api -X DELETE repos/%s/releases/assets/%s", RS_REPO, old0),
               fixed = TRUE)
  expect_match(sm, sprintf("gh api -X PATCH repos/%s/releases/assets/%s -f name=covr-raw-s0-a.tar.gz",
                           RS_REPO, tmp), fixed = TRUE)
})

test_that("gh_cli passes each argument through intact and reports the exit status", {
  bin <- tempfile("bin_"); dir.create(bin)
  writeLines(c("#!/bin/sh", "for a in \"$@\"; do printf '%s\\n' \"$a\"; done",
               "exit ${FAKE_GH_STATUS:-0}"), file.path(bin, "gh"))
  Sys.chmod(file.path(bin, "gh"), "755")
  withr::local_envvar(PATH = paste(bin, Sys.getenv("PATH"), sep = .Platform$path.sep))
  args <- c("api", "--paginate", "repos/x/y/releases/1/assets?per_page=100",
            "--jq", RESTORE_ASSET_JQ)
  r <- gh_cli(args)
  expect_identical(r$status, 0L)
  expect_identical(r$out, args)
  withr::local_envvar(FAKE_GH_STATUS = "3")
  expect_identical(gh_cli("x")$status, 3L)
})
