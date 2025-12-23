# CTSQ CRT H+B Replication — Preprocessing

Goal: prepare the Qualtrics follow-up dataset so it can be analysed in JASP. The targets pipeline reads the raw SAV export, keeps completed cases aged ≥ 18 (no RecordedDate filter), scores CTSQ/CRT/HB, computes SES and timing summaries, and exports a JASP-ready CSV.

## Data
- Raw survey export: `data/4-CTSQ-D+Follow+Up_December+11,+2025_07.18.sav`
- Companion CSV/XLSX versions remain unchanged.
- Preregistration: `report/Preregistration 4-CTSQ-D Thinking Styles CRT Replication.pdf` and the generated text copy `report/Preregistration 4-CTSQ-D Thinking Styles CRT Replication.txt`.

## Requirements
- R (≥ 4.0 recommended)
- CRAN packages: `targets`, `haven`, `summarytools`, `tibble`, `dplyr`, `purrr`, `jsonlite`, `readr`, `tidyr`
- Please **do not run `renv::restore()`** unless explicitly requested.

Install the needed packages once:

```r
install.packages(c("targets", "haven", "summarytools", "tibble"))
install.packages(c("dplyr", "purrr", "jsonlite", "readr", "tidyr"))
```

## Running the pipeline
From the repo root:

```bash
Rscript -e "targets::tar_make()"
```

The pipeline:
1) reads the SAV file (`raw_data`),
2) applies inclusion filter (workflow == completed, age ≥ 18; no date restriction),
3) writes `outputs/dfsummary.html` on the filtered data,
4) extracts CRT/HB item metadata -> `outputs/hb_crt_items.json`,
5) exports all variable value/label pairs to `outputs/value_labels.json`,
5) reads manual answer key `data/hb_crt_answer_key.json`,
6) scores CTSQ/CRT/HB (dropping items seen-before or with aids), SES, timing summaries,
7) drops participants with both CRT and HB removed (aid use can remove all CRT/HB for a person), summarizes drop reasons,
8) writes `outputs/drop_summary.csv`,
9) exports `outputs/analysis_dataset.csv`.

## Outputs
- `outputs/dfsummary.html`: dataset overview (frequencies, missingness, variable metadata).
- `_targets/`: cache directory managed by **targets**.

## Preregistration-driven analysis requirements
- Planned analyses: two Bayesian linear regressions in JASP – one with outcome `CRT_z`, one with outcome `HB_z`; predictors `CTSQ_AOT_z`, `CTSQ_PET_z`, `CTSQ_PIT_z`, `CTSQ_CMT_z`, covariates `SES`, `age_z`, `gender_z`; JASP default JZS priors (r = 0.354), β-binomial model prior (α = β = 1), g-prior α = 3; residual vs. fitted and Q–Q plot checks.
- CSV must therefore include per participant: z-scored CTSQ subscale means (`CTSQ_AOT_z`, `CTSQ_PIT_z`, `CTSQ_CMT_z`, `CTSQ_PET_z`), outcomes `CRT_z` and `HB_z` (mean proportion correct after dropping CRT items marked familiar), covariates `SES`, `age_z`, `gender_z`, and the three SES components (`income_per_household_member_z`, `education_z`, `occupational_qualification_z`) or their summed `SES` value.
- Required preprocessing before export: apply preregistered participant exclusions (consent, completeness, attention check, self-reported aids), remove CRT items marked familiar, impute missing values via MICE, compute scale means then z-transform, compute SES as the sum of the three z-scaled components, keep an identifier to track exclusions.

## Development goals derived from prereg
- Guarantee the exported `outputs/analysis_dataset.csv` contains all prereg-required variables and z-scoring/aggregation steps.
- Encode prereg exclusion rules and CRT-familiarity item drops deterministically in the pipeline, with transparent drop summaries.
- Ensure SES derivation matches prereg (sum of z income, education, occupational qualification) and is reproducible.
- Keep outputs JASP-friendly (variable names as above) and provide sanity checks (plots/summary) for residual assumptions if recreated in R.
- Document all of the above in this README and in function-level comments for reproducibility.

## Tests
- Unit tests should assert: (1) exclusion rules remove cases lacking consent/attention or reporting aids; (2) CRT scoring omits items flagged familiar per participant; (3) CTSQ subscale means and z-scores match hand calculations; (4) SES equals the sum of the three z components; (5) exported CSV columns and value ranges align with prereg needs.

## Implementation plan
- Codify exclusion and item-level filtering in targets steps before scoring.
- Add scoring functions that compute subscale means, CRT/HB accuracy, z-transform, SES sum; expose JASP-ready column names.
- Wire targets to emit both the cleaned dataset and drop summaries; fail the pipeline if required columns are missing.
- Write tests (preferably in `tests/`) that cover the checks listed above and run via `testthat`.
- Keep README and `TODO.md` in sync with any changes to variable names or preprocessing logic.
# super-memory
