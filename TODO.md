- [ ] Preprocess dataset for JASP
  - [x] Set up targets pipeline to read SAV, filter to 2025-12-11, and render `dfsummary.html`
  - [x] Emit CRT/HB item metadata JSON (`outputs/hb_crt_items.json`)
  - [x] Finalize answer key (`data/hb_crt_answer_key.json`) and load for scoring
  - [x] Implement/verify recoding & z-scores for CTSQ/CRT/HB, SES, timings
  - [x] Export cleaned dataset to JASP-ready CSV for JASP import (`outputs/analysis_dataset.csv`, `outputs/drop_summary.csv`)

- [ ] Execution reminders (run locally)
  - `RENV_CONFIG_AUTOLOAD=FALSE Rscript -e "testthat::test_dir('tests/testthat')"`
  - `RENV_CONFIG_AUTOLOAD=FALSE Rscript -e "targets::tar_make()"`

- [ ] Validate and document
  - [ ] Add tests for scoring and data filters (started: timing, SES, CTSQ scoring)
  - [x] Expand README with analysis decisions and variable mappings

- [ ] Prereg compliance hardening
  - [x] Encode prereg exclusion rules (consent, completeness, attention check, aids) as a dedicated pipeline step with drop reasons
  - [x] Enforce CRT familiarity drops before scoring and track `crt_items_scored`
  - [ ] Guard SES construction (components present, sum/mean definition, no all-NA) and keep prereg-required SES output
  - [x] Add required-column and value-range validation before exporting `analysis_dataset.csv`

- [ ] Tests to add (testthat)
  - [x] Exclusion logic removes rows failing consent/attention or reporting aids; drop reasons are counted
  - [x] CRT familiarity drop reduces `crt_items_scored` and excludes those items from accuracy
  - [x] CTSQ subscale means/z-scores match hand-calculated fixture
  - [ ] SES equals sum of z(income_per_member, education, occupation) and handles missing/zero HH members
  - [x] Final export contains all prereg-required columns and numeric ranges are finite / proportions in [0,1]
  - [ ] Deterministic outputs when imputation is used (fixed seed)
  - [x] CRT/HB scoring handles aid flags and outcome-bias pair logic
  - [x] analysis_dataset.csv contains only prereg/JASP columns (no CTSQ/duplicate CRT/HB) and proportions in [0,1]; CRT per-item columns match answer key

- [ ] Output sanity artifacts
  - [x] Add quick sanity tables/plots: distribution of CRT_av/HB_av, histograms of CTSQ subscales, and counts of drop reasons; write to `outputs/sanity/`
  - [x] Ensure these checks run from the pipeline (optional target) without blocking tar_make
  - [x] Add per-item CRT/HB correctness tables to `outputs/sanity/`
  - [x] Export validation blocks out-of-range proportions; per-item CRT columns match answer key
- [ ] Descriptive CRT/HB plots
  - [x] Add APA-themed performance plotting helper/target (distribution, item accuracy, CRT–HB scatter, speed–accuracy)
  - [ ] Run `tar_make` to render PNGs and review outputs

- [ ] Current task: audit CRT per-item correctness counts
  - [x] Add AGENTS guidance to avoid running R locally and prefer Python checks
  - [x] Add early target to dump raw SAV tibble to CSV for debugging
  - [x] Create Python helper under `tools/` to compare correctness table to answer key
  - [x] Align crt_item_correctness.csv logic with answer key (fix row-level join)
  - [x] Add response labels to correctness tables (HB factor labels)
  - [x] Split NA into seen / aid_dropped / missing in summaries
  - [x] Include item text column in correctness summaries
  - [x] Add HB13/14 outcome-pair correctness table
  - [x] Accept additional observed CRT responses (Emilys; das/srei/szreochholz variants)
  - [ ] Rerun pipeline (sanity_outputs) and verify helper shows no mismatches

- [x] Review subscale averaging patterns (analysis)
  - [x] Current repo `score_ctsq()` behaviour
  - [x] `other_repos/conservatism_climate_2` `calculate_scale_averages()`
  - [x] `other_repos/covid-morals-decision` AP-3 recoding notebook
  - [ ] Decide whether to extract a generic helper for subscale av/z scoring
