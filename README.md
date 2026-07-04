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
installed best-effort, fixed recorded RNG seed. Every row in
`coverage_summary` also records the exact R, covr, and gcov versions, the
locale, and the seed used to produce it, so any coverage number can be traced
back to the environment that made it.

## Run

`Rscript scripts/update.R out/` processes one shard (packages that are new or
have a new release) against the prior database in `out/`, writing the updated
`coverage_summary`, `coverage_file`, and `coverage_function` tables plus any
raw covr objects under `out/raw/`. GitHub Actions runs shards on a schedule
inside `rocker/r2u:noble`, publishing the database and the raw-object
tarballs to this repository's rolling `current` release after each shard.

`Rscript tests/testthat.R` runs the unit test suite.
