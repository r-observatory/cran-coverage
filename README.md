# cran-coverage

Latest-version test coverage for CRAN packages, computed with covr in a fixed
deterministic environment and published as a rolling database plus raw covr
object tarballs.

## What it stores

- `coverage_summary` - one row per package/version: line and expression
  coverage, by-type coverage (tests/examples/vignettes), compiled-code
  coverage, the environment fingerprint, and the run status.
- `coverage_file`, `coverage_function` - per-file and per-function coverage,
  the latter joinable to the analyzer function records on (file, label).
- Raw covr objects, bundled by package first letter as `covr-raw-*.tar.gz`
  release assets.

## Determinism

en_US.UTF-8 locale, UTC, single-threaded, NOT_CRAN=true, all Suggests
installed best-effort, and a recorded RNG seed. covr runs a package's own
tests in subprocesses, so the seed set in the driving R session does not by
itself pin the coverage number; it is recorded for provenance, not as a
determinism guarantee. Every row in `coverage_summary` also records the
exact R, covr, and gcov versions, the locale, and the seed used to produce
it, so any coverage number can be traced back to the environment that made
it.

## Run

`Rscript scripts/update.R out/` processes one shard (packages that are new or
have a new release) against the prior database in `out/`, writing the updated
`coverage_summary`, `coverage_file`, and `coverage_function` tables plus any
raw covr objects under `out/raw/`. GitHub Actions runs shards on a schedule
inside `rocker/r2u:noble`, publishing the database and the raw-object
tarballs to this repository's rolling `current` release after each shard.

`data/requeue/requeue.tsv` lists package versions to measure again (package,
version, reason). Each collect leg measures up to `REQUEUE_PER_RUN` of them
per run, before its normal work, while the listed version is still the
current CRAN version and its raw covr object is missing from the shard's
bundle. A re-measure that succeeds replaces the stored rows and adds the
object to the bundle. One that fails leaves the stored rows as they were, and
the row is tried again on a later run, up to `REQUEUE_MAX_ATTEMPTS` failures.
A row drops out once its object is in the bundle, so the file needs no
editing. The run summary and `manifest.json` report how many rows were
queued, measured, failed and left.

`Rscript tests/testthat.R` runs the unit test suite.

## Feedback

Found a bug, a wrong number, or a missing package? Report it at [r-observatory/feedback](https://github.com/r-observatory/feedback/issues/new/choose). All feedback about R Observatory, the site, the data, and the pipelines, is tracked in one place.
