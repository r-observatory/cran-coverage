# scripts/restore.R: one-off restore of raw covr objects from the pre-shard
# covr-raw-a.tar.gz into the covr-raw-s<i>-a.tar.gz bundles on the internal
# release. Needs export.R and update.R. Run from the repository root:
#   GH_REPO=owner/repo Rscript scripts/restore.R

RESTORE_OLD_BUNDLE <- list(
  name = "covr-raw-a.tar.gz", size = 33167512,
  sha256 = "f7de53f17b2dbd9a864fa9bf019dac9b6705a5ab800dbaeb1ad464b616265132")
RESTORE_MANIFEST <- "data/restore/restore-manifest.tsv"
RESTORE_SHARD_COUNT <- 4L
RESTORE_ASSET_JQ <- ".[] | [.id, .name, .size, .state, .digest] | @tsv"
RESTORE_NEEDS_RECOVERY <- "stopped after the upload, see the recovery below"

restore_bundle_name <- function(shard) sprintf("covr-raw-s%d-a.tar.gz", as.integer(shard))

# No shard download pattern (covr-raw-s<i>-*.tar.gz) matches this name.
restore_temp_name <- function(shard) sprintf("restore-s%d-a.tgz", as.integer(shard))

#' Run gh with each argument passed intact.
#' @return list(status = exit status, out = stdout lines).
gh_cli <- function(args) {
  out <- suppressWarnings(system2("gh", shQuote(args), stdout = TRUE))
  status <- attr(out, "status")
  list(status = if (is.null(status)) 0L else as.integer(status),
       out = as.character(out))
}

#' Shards named in the dispatch input ("0 1 2 3" or "2,3").
parse_restore_shards <- function(x) {
  s <- strsplit(trimws(x), "[[:space:],]+")[[1]]
  s <- suppressWarnings(as.integer(s[nzchar(s)]))
  if (!length(s) || anyNA(s) || anyDuplicated(s) ||
      any(s < 0L | s >= RESTORE_SHARD_COUNT)) {
    stop(sprintf("shards must be distinct values from 0 to %d, got '%s'",
                 RESTORE_SHARD_COUNT - 1L, x))
  }
  sort(s)
}

release_id <- function(gh, repo, tag) {
  r <- gh(c("api", sprintf("repos/%s/releases/tags/%s", repo, tag), "--jq", ".id"))
  id <- trimws(r$out)
  if (!identical(r$status, 0L) || length(id) != 1L || !grepl("^[0-9]+$", id)) {
    stop(sprintf("could not read the id of release '%s'", tag))
  }
  id
}

#' Every asset on the release, half created ones included (the paginated
#' assets API lists those; gh release view does not).
#' @return data.frame(id, name, size, state, digest).
list_release_assets <- function(gh, repo, rid) {
  r <- gh(c("api", "--paginate",
            sprintf("repos/%s/releases/%s/assets?per_page=100", repo, rid),
            "--jq", RESTORE_ASSET_JQ))
  if (!identical(r$status, 0L)) stop("could not list the release assets")
  f <- strsplit(r$out[nzchar(r$out)], "\t", fixed = TRUE)
  if (any(lengths(f) < 4L)) stop("unexpected line in the release asset listing")
  # strsplit drops a trailing empty field (an asset with no digest yet).
  field <- function(i) vapply(f, function(x) if (length(x) >= i) x[[i]] else "", "")
  data.frame(id = field(1), name = field(2), size = as.numeric(field(3)),
             state = field(4), digest = field(5), stringsAsFactors = FALSE)
}

#' The restore manifest: one row per object to put back, checked against the
#' shard layout so a row can only land in its own shard's -a bundle.
read_restore_manifest <- function(path) {
  m <- utils::read.delim(path, colClasses = "character", quote = "")
  miss <- setdiff(c("shard", "bundle", "package", "version", "member", "md5"), names(m))
  if (length(miss)) stop("restore manifest lacks column(s): ", paste(miss, collapse = " "))
  bad <- m$bundle != restore_bundle_name(m$shard) |
    m$member != sprintf("out/raw/a/%s_%s.rds", m$package, m$version) |
    package_partition(m$package, RESTORE_SHARD_COUNT) != as.integer(m$shard) |
    vapply(m$package, raw_partition, "", USE.NAMES = FALSE) != "a" |
    !grepl("^[0-9a-f]{32}$", m$md5)
  bad[is.na(bad)] <- TRUE
  if (any(bad)) {
    stop(sprintf("%d manifest row(s) do not agree with the shard layout: %s",
                 sum(bad), paste(utils::head(m$member[bad], 5), collapse = " ")))
  }
  if (anyDuplicated(m$member)) stop("the restore manifest lists a member twice")
  m
}

# sha256 as hex, with or without the listing's "sha256:" prefix.
file_matches <- function(path, size, sha256) {
  file.exists(path) && isTRUE(file.size(path) == size) &&
    identical(file_sha256(path), tolower(sub("^sha256:", "", sha256)))
}

#' Download covr-raw-a.tar.gz and stop unless its size and sha256 match.
fetch_old_bundle <- function(gh, repo, tag, old, dir, attempts = 3L,
                             pause = function(n) Sys.sleep(20 * n)) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, old$name)
  for (i in seq_len(attempts)) {
    if (i > 1L) pause(i - 1L)
    unlink(path)
    r <- gh(c("release", "download", tag, "--repo", repo, "--dir", dir, "--pattern", old$name))
    if (file_matches(path, old$size, old$sha256)) return(path)
    message(sprintf("%s download (attempt %d) did not match; gh exited %s", old$name, i, r$status))
  }
  stop(sprintf("%s does not match size %s and sha256 %s; nothing was changed",
               old$name, format(old$size, scientific = FALSE), old$sha256))
}

#' Downloader for fetch_prior_bundles() that also checks each file against
#' the sha256 the release lists, removing any that differ so they count as
#' not fetched.
digest_checked_downloader <- function(gh, repo, tag, digests) {
  function(names, dir) {
    r <- gh(c("release", "download", tag, "--repo", repo, "--dir", dir,
              as.vector(rbind("--pattern", names))))
    for (n in names) {
      p <- file.path(dir, n)
      want <- if (n %in% names(digests)) digests[[n]] else ""
      if (file.exists(p) && !(nzchar(want) && file_matches(p, file.size(p), want))) {
        message(sprintf("%s does not match the sha256 the release lists", n))
        unlink(p)
      }
    }
    if (!identical(r$status, 0L)) stop(sprintf("gh exited %s", r$status))
    invisible(TRUE)
  }
}

#' Extract only the manifest's members from the old bundle and check each md5.
#' @return Paths of the extracted objects, in manifest order.
extract_manifest_members <- function(tarball, manifest, exdir) {
  have <- utils::untar(tarball, list = TRUE, tar = "internal")
  miss <- setdiff(manifest$member, have)
  if (length(miss)) {
    stop(sprintf("%d manifest member(s) are not in %s: %s", length(miss),
                 basename(tarball), paste(utils::head(miss, 5), collapse = " ")))
  }
  utils::untar(tarball, files = manifest$member, exdir = exdir, tar = "internal")
  paths <- file.path(exdir, manifest$member)
  got <- unname(tools::md5sum(paths))
  bad <- is.na(got) | got != manifest$md5
  if (any(bad)) {
    stop(sprintf("%d restored object(s) do not match the md5 in the manifest: %s",
                 sum(bad), paste(utils::head(basename(paths[bad]), 5), collapse = " ")))
  }
  paths
}

#' Copy restored objects into a shard's store, never over an existing one.
add_restored_objects <- function(paths, store, prior_members = character(0)) {
  dest <- file.path(store, basename(paths))
  clash <- basename(paths)[file.exists(dest) | basename(paths) %in% basename(prior_members)]
  if (length(clash)) {
    stop(sprintf("%d manifest object(s) are already in the shard's bundle; refusing to overwrite: %s",
                 length(clash), paste(utils::head(clash, 10), collapse = " ")))
  }
  dir.create(store, recursive = TRUE, showWarnings = FALSE)
  if (!all(file.copy(paths, dest, overwrite = FALSE, copy.date = TRUE))) {
    stop("could not copy the restored objects into ", store)
  }
  invisible(dest)
}

#' A rebuilt bundle may go up only when the guard plans it as keeping every
#' prior object and it holds exactly the fetched prior plus the restored rows.
check_restore_count <- function(plan, bundle, prior_n, added) {
  row <- plan[plan$name == bundle, , drop = FALSE]
  if (nrow(plan) != 1L || nrow(row) != 1L) {
    stop(sprintf("expected one rebuilt bundle, %s; got: %s", bundle,
                 paste(plan$name, collapse = " ")))
  }
  if (!isTRUE(row$upload) || !identical(row$reason, "prior objects all kept")) {
    stop(sprintf("%s: the upload guard gave '%s', not an upload with prior objects all kept",
                 bundle, row$reason))
  }
  want <- as.integer(prior_n) + as.integer(added)
  if (!identical(as.integer(row$prior), as.integer(prior_n)) ||
      !identical(as.integer(row$new), want)) {
    stop(sprintf("%s: rebuilt %s object(s) over a prior of %s; expected %d (%d prior + %d restored)",
                 bundle, row$new, row$prior, want, as.integer(prior_n), as.integer(added)))
  }
  invisible(TRUE)
}

#' Fetch one shard's -a bundle through the guarded helpers, add the restored
#' objects and rebuild it with prepare_raw_upload().
#' @param asset That bundle's row from list_release_assets().
#' @param staged Paths of this shard's extracted, md5-checked objects.
build_restored_bundle <- function(gh, repo, tag, shard, asset, staged, work, attempts = 3L,
                                  pause = function(n) Sys.sleep(20 * n)) {
  bundle <- restore_bundle_name(shard)
  wd <- file.path(work, sprintf("s%d", shard))
  unlink(wd, recursive = TRUE)
  dir.create(wd, recursive = TRUE)
  staged <- normalizePath(staged)
  owd <- setwd(wd)
  on.exit(setwd(owd), add = TRUE)
  prior <- fetch_prior_bundles(
    stats::setNames(asset$size, bundle), "rawbundles",
    digest_checked_downloader(gh, repo, tag, stats::setNames(asset$digest, bundle)),
    exdir = ".", attempts = attempts, pause = pause)
  if (length(prior$unavailable) || is.null(prior$members[[bundle]])) {
    stop(sprintf("%s could not be fetched matching the size and sha256 the release lists; nothing was changed",
                 bundle))
  }
  before <- length(prior$members[[bundle]])
  add_restored_objects(staged, file.path("out", "raw", "a"), prior$members[[bundle]])
  saveRDS(prior, "rawbundles/prior-state.rds")
  plan <- prepare_raw_upload("rawbundles/prior-state.rds", "out/raw", "bundle",
                             sprintf("covr-raw-s%d-", shard), shard, RESTORE_SHARD_COUNT,
                             function(names, dir) stop("no prior bundle should need a refetch"),
                             exdir = ".", attempts = 1L, pause = pause)
  check_restore_count(plan, bundle, before, length(staged))
  list(shard = shard, bundle = bundle, before = before, added = length(staged),
       after = as.integer(plan$new), path = normalizePath(file.path("bundle", bundle)),
       old = asset)
}

# A swap failure, marked with whether the old asset may already be gone.
swap_stop <- function(msg, deleted) {
  stop(structure(class = c("restore_swap_error", "error", "condition"),
                 list(message = msg, call = NULL, deleted = deleted)))
}

# Lists the release until ok(assets) holds, or gives up.
await_assets <- function(gh, repo, rid, ok, attempts, pause) {
  a <- NULL
  for (i in seq_len(attempts)) {
    if (i > 1L) pause(i - 1L)
    a <- tryCatch(list_release_assets(gh, repo, rid), error = function(e) NULL)
    if (!is.null(a) && isTRUE(ok(a))) return(list(ok = TRUE, assets = a))
  }
  list(ok = FALSE, assets = a)
}

#' Replace one bundle on the release without a window where it can be lost:
#' upload under the temporary name, verify it, delete the old asset by id,
#' rename the upload, verify again. After the delete the upload is never
#' removed; a failure stops with the command that finishes the swap.
#' @param file Local rebuilt bundle, named with the temporary name.
#' @param old  The bundle's row from the listing its prior was fetched from.
swap_asset <- function(gh, repo, rid, tag, file, final, old, attempts = 3L,
                       pause = function(n) Sys.sleep(20 * n)) {
  if (!grepl("^covr-raw-s[0-9]+-a[.]tar[.]gz$", final) || !identical(old$name, final)) {
    stop(sprintf("refusing to replace %s", old$name))
  }
  temp <- basename(file)
  size <- file.size(file)
  digest <- paste0("sha256:", file_sha256(file))
  asset_path <- function(id) sprintf("repos/%s/releases/assets/%s", repo, id)
  delete_cmd <- function(id) sprintf("gh api -X DELETE %s", asset_path(id))
  rename_cmd <- function(id) sprintf("gh api -X PATCH %s -f name=%s", asset_path(id), final)
  same <- function(a, name, id, sz, dg) {
    r <- a[a$name == name, , drop = FALSE]
    nrow(r) == 1L && (is.null(id) || identical(r$id, id)) && identical(r$state, "uploaded") &&
      isTRUE(r$size == sz) && identical(r$digest, dg)
  }

  message(sprintf("uploading %s as %s (%s bytes, %s)", final, temp,
                  format(size, scientific = FALSE), digest))
  if (!identical(gh(c("release", "upload", tag, file, "--repo", repo))$status, 0L)) {
    swap_stop(sprintf(paste0("uploading %s failed; %s (asset %s) was not touched. A failed upload can ",
                             "leave a half created %s on the release; delete it by id before running ",
                             "the restore again."), temp, final, old$id, temp), deleted = FALSE)
  }
  chk <- await_assets(gh, repo, rid, function(a) {
    same(a, temp, NULL, size, digest) && same(a, final, old$id, old$size, old$digest)
  }, attempts, pause)
  up <- if (!is.null(chk$assets)) chk$assets[chk$assets$name == temp, , drop = FALSE] else NULL
  if (!chk$ok) {
    swap_stop(sprintf(paste0("the release does not show %s matching the local file, or %s (asset %s) ",
                             "changed since it was fetched; nothing was deleted.%s"),
                      temp, final, old$id,
                      if (!is.null(up) && nrow(up) == 1L)
                        sprintf(" Remove the upload before a re-run with:\n  %s", delete_cmd(up$id)) else ""),
              deleted = FALSE)
  }
  message(sprintf("replacing %s (asset %s) with asset %s; should this stop before the rename, recover with: %s",
                  final, old$id, up$id, rename_cmd(up$id)))
  if (!identical(gh(c("api", "-X", "DELETE", asset_path(old$id)))$status, 0L)) {
    swap_stop(sprintf(paste0("deleting %s (asset %s) failed. The rebuilt bundle stays on the release ",
                             "as %s (asset %s). If asset %s is still listed, delete it with\n  %s\n",
                             "then finish the swap with\n  %s"),
                      final, old$id, temp, up$id, old$id, delete_cmd(old$id), rename_cmd(up$id)),
              deleted = NA)
  }
  renamed <- FALSE
  for (i in seq_len(attempts)) {
    if (i > 1L) pause(i - 1L)
    if (identical(gh(c("api", "-X", "PATCH", asset_path(up$id), "-f",
                       paste0("name=", final)))$status, 0L)) {
      renamed <- TRUE
      break
    }
  }
  if (!renamed) {
    swap_stop(sprintf(paste0("renaming %s (asset %s) to %s failed after the old %s (asset %s) was ",
                             "deleted. %s is left on the release and holds the full rebuilt bundle. ",
                             "Recover with:\n  %s"),
                      temp, up$id, final, final, old$id, temp, rename_cmd(up$id)), deleted = TRUE)
  }
  done <- await_assets(gh, repo, rid, function(a) {
    same(a, final, up$id, size, digest) && !any(a$name == temp) && !any(a$id == old$id)
  }, attempts, pause)
  if (!done$ok) {
    swap_stop(sprintf(paste0("after the rename the release does not show %s as asset %s (%s bytes, %s). ",
                             "Asset %s holds the full rebuilt bundle; if it is still named %s, recover with:\n  %s"),
                      final, up$id, format(size, scientific = FALSE), digest, up$id, temp,
                      rename_cmd(up$id)), deleted = TRUE)
  }
  message(sprintf("%s is now asset %s (%s bytes, %s)", final, up$id,
                  format(size, scientific = FALSE), digest))
  invisible(up$id)
}

#' Job summary: object counts per bundle before and after, and any error.
write_restore_summary <- function(path, report, dry_run = FALSE, error = NULL) {
  n <- function(x) ifelse(is.na(x), "", as.character(x))
  lines <- c("## Raw covr objects restored from covr-raw-a.tar.gz", "",
             if (dry_run) c(paste("Dry run: the release was not changed.",
                                  "After is the object count of each rebuilt bundle."), ""),
             "| Bundle | Before | Restored | After | Result |",
             "|---|---:|---:|---:|---|",
             sprintf("| %s | %s | %s | %s | %s |", report$bundle, n(report$before),
                     n(report$restored), n(report$after), report$result))
  if (!is.null(error)) {
    lines <- c(lines, "", "The restore stopped:", "", "```", error, "```")
    left <- report$bundle[!report$result %in% c("swapped", RESTORE_NEEDS_RECOVERY)]
    if (!dry_run && length(left)) {
      lines <- c(lines, "", sprintf("Left as they were: %s. Dispatch again with shards '%s' once the release is sound.",
                                    paste(left, collapse = ", "),
                                    paste(sub("^covr-raw-s([0-9]+)-a[.]tar[.]gz$", "\\1", left),
                                          collapse = " ")))
    }
  }
  if (nzchar(path)) cat(lines, file = path, sep = "\n", append = TRUE) else cat(lines, sep = "\n")
  invisible(lines)
}

#' The whole restore. Everything is fetched, rebuilt and checked for every
#' shard before the first write; then each bundle is swapped in turn.
#' @return 0 on success, 1 on any failure (the summary says why).
restore_main <- function(gh = gh_cli, repo, tag = "internal", shards = 0:3,
                         manifest_path = RESTORE_MANIFEST, old = RESTORE_OLD_BUNDLE,
                         work = "out/restore", unsent = "out/unsent", dry_run = FALSE,
                         summary_path = "", attempts = 3L,
                         pause = function(n) Sys.sleep(20 * n)) {
  shards <- as.integer(shards)
  rep <- data.frame(bundle = restore_bundle_name(shards), before = NA_integer_,
                    restored = NA_integer_, after = NA_integer_, result = "not started",
                    stringsAsFactors = FALSE)
  err <- NULL
  status <- tryCatch({
    if (!nzchar(repo)) stop("GH_REPO is not set")
    m <- read_restore_manifest(manifest_path)
    m <- m[as.integer(m$shard) %in% shards, , drop = FALSE]
    rid <- release_id(gh, repo, tag)
    assets <- list_release_assets(gh, repo, rid)
    for (s in shards) {
      a <- assets[assets$name == restore_bundle_name(s), , drop = FALSE]
      if (nrow(a) != 1L || !identical(a$state, "uploaded") || !grepl("^sha256:", a$digest)) {
        stop(sprintf("%s is not on the release as one uploaded asset with a sha256",
                     restore_bundle_name(s)))
      }
      if (restore_temp_name(s) %in% assets$name) {
        stop(sprintf("%s is already on the release; delete it by id before running the restore",
                     restore_temp_name(s)))
      }
    }
    oldp <- fetch_old_bundle(gh, repo, tag, old, file.path(work, "old"), attempts, pause)
    staged <- extract_manifest_members(oldp, m, file.path(work, "old", "members"))
    built <- list()
    for (k in seq_along(shards)) {
      rows <- as.integer(m$shard) == shards[k]
      b <- build_restored_bundle(gh, repo, tag, shards[k],
                                 assets[assets$name == rep$bundle[k], , drop = FALSE],
                                 staged[rows], work, attempts, pause)
      rep$before[k] <- b$before
      rep$restored[k] <- b$added
      rep$result[k] <- "built, not swapped"
      built[[k]] <- b
    }
    if (dry_run) {
      for (k in seq_along(built)) {
        rep$after[k] <- built[[k]]$after
        rep$result[k] <- "dry run, not uploaded"
      }
    } else {
      dir.create(unsent, recursive = TRUE, showWarnings = FALSE)
      for (k in seq_along(built)) {
        b <- built[[k]]
        # Kept here until the swap is verified; a failed run saves it as an artifact.
        file <- file.path(unsent, restore_temp_name(b$shard))
        if (!file.rename(b$path, file)) stop("could not move the rebuilt bundle to ", file)
        rep$result[k] <- "swapping"
        swap_asset(gh, repo, rid, tag, file, b$bundle, b$old, attempts, pause)
        unlink(file)
        rep$after[k] <- b$after
        rep$result[k] <- "swapped"
      }
    }
    0L
  }, error = function(e) {
    rep$result[rep$result == "swapping"] <<-
      if (isFALSE(e$deleted)) "unchanged, stopped before the delete" else RESTORE_NEEDS_RECOVERY
    err <<- conditionMessage(e)
    message("restore stopped: ", err)
    1L
  })
  write_restore_summary(summary_path, rep, dry_run, err)
  status
}

if (identical(sys.nframe(), 0L)) {
  for (s in c("config.R", "export.R", "update.R")) source(file.path("scripts", s))
  status <- restore_main(
    repo = Sys.getenv("GH_REPO"), tag = Sys.getenv("INTERNAL_TAG", "internal"),
    shards = parse_restore_shards(Sys.getenv("RESTORE_SHARDS", "0 1 2 3")),
    dry_run = identical(tolower(Sys.getenv("RESTORE_DRY_RUN")), "true"),
    summary_path = Sys.getenv("GITHUB_STEP_SUMMARY"))
  quit(save = "no", status = status)
}
