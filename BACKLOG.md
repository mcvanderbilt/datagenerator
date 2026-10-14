# Backlog

Single source of truth for work, bugs, and decisions. IDs are never reused.
Status: `Done`, `Open`, `Blocked` (waiting on a named decision), or `Dropped`.

Stages:

* **Stage 1: synthetic generators.** Parameters typed in by the user.
* **Stage 2: fit from real data.** Estimating parameters from real records,
  behind an explicit privacy acknowledgment.
* **Stage 3: public sources.** Retrieving existing data from named sources.

## Stage 1: synthetic generators

| ID | Item | Status |
|---|---|---|
| S1-01 | Package scaffold: DESCRIPTION, MIT license, NEWS, README, .Rbuildignore | Done |
| S1-02 | Input validation helpers with actionable messages | Done |
| S1-03 | Seed handling that leaves the global RNG unchanged | Done |
| S1-04 | `dg_dataset` class, provenance record, `dg_meta()`, synthetic banner | Done |
| S1-05 | `gen_numeric()` | Done |
| S1-06 | `gen_correlated()` with positive-definite check | Done |
| S1-07 | `gen_categorical()` | Done |
| S1-08 | `gen_dates()` | Done |
| S1-09 | `gen_ids()` | Done |
| S1-10 | `gen_grouped()` (random intercept, ICC recorded) | Done |
| S1-11 | `add_missingness()` for MCAR, MAR, and MNAR | Done |
| S1-12 | testthat tests for every generator, including bad input | Done |
| S1-13 | Vignettes: numeric, categorical/dates/IDs, missingness, responsible use | Done |
| S1-14 | R CMD check with no errors or warnings (notes reviewed) | Open, see run log |
| S1-15 | Set up renv for development (`renv::init()`) | Open |
| S1-16 | GitHub Actions R CMD check workflow | Open, needs approval |
| S1-17 | Add a copula option for non-normal dependence | Open, enhancement |
| S1-18 | More margins: uniform, lognormal, Poisson, beta | Open, enhancement |

## Stage 2: fit from real data

| ID | Item | Status |
|---|---|---|
| S2-01 | Design the privacy acknowledgment (e.g. `i_understand_privacy_risk = TRUE`) | Blocked on decision |
| S2-02 | `fit_*()` functions that estimate parameters from a real data frame | Open |
| S2-03 | Warn and record provenance when a fitted model came from real records | Open |
| S2-04 | Document leakage risk and a privacy check (e.g. nearest-record distance) | Open |

## Stage 3: public sources

| ID | Item | Status |
|---|---|---|
| S3-01 | Choose the first one or two sources and verify their terms and rate limits | Blocked on decision |
| S3-02 | `httr2`-based HTTP layer with rate limiting, retries, and offline errors | Open, needs approval for dependency |
| S3-03 | Cache in `tools::R_user_dir()` with a time-to-live | Open |
| S3-04 | Source adapter interface, one file per source | Open |
| S3-05 | Record source URL, retrieval date, and query in the returned object | Open |
| S3-06 | Mocked HTTP tests (no live calls) | Open |

## Bugs

| ID | Bug | Status |
|---|---|---|
| B-01 | None known yet | - |

## Enhancements (not yet staged)

| ID | Idea | Status |
|---|---|---|
| E-01 | A spec builder that composes several generators into one dataset | Open, see D-02 |
| E-02 | Tidyverse helpers, only if a function needs them | Open |

## Known limitations

| ID | Limitation |
|---|---|
| L-01 | Provenance lives as an attribute. Some dplyr verbs may return plain tibbles that lose the label. Check with `dg_meta()`. |
| L-02 | `add_missingness()` refuses data not made by this package. Real data is out of scope for Stage 1. |

## Decisions

| ID | Decision | Date |
|---|---|---|
| D-01 | Package name `datagenerator`, matching the repository name | 2026-10-13 |
| D-02 | Stage 1 uses standalone generator functions. Each returns one dataset, and the caller combines them. A spec builder is deferred (E-01). | 2026-10-13 |
| D-03 | `seed` is required on every generator so results are always reproducible | 2026-10-13 |
| D-04 | Runtime imports limited to cli, stats, tibble, utils, withr. `httr2` is deferred to Stage 3. `rlang` dropped (not used). | 2026-10-13 |
| D-05 | Tidyverse is not attached or imported. Use base R plus tibble, and add other packages only when a function needs them. | 2026-10-13 |
| D-06 | MIT license. Copyright holder: Matthew Vanderbilt (confirm spelling). | 2026-10-13 |
| D-07 | Version 0.0.0.9000 until the first public release | 2026-10-13 |
| D-08 | `add_missingness()` accepts only datagenerator output, so the synthetic label cannot be attached to real data | 2026-10-13 |

## Open questions

* Confirm the copyright holder name for the LICENSE file.
* Confirm the first Stage 3 source (S3-01).
