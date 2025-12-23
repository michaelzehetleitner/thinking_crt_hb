# Test Plan for CTSQ/CRT/HB Preprocessing Pipeline

## Filtering & Inclusion
- filter_sample() keeps RecordedDate = 2025-12-11, workflow == "completed", age >= 18; drops others.
- Date filter robustness to missing or malformed dates.

## CTSQ Scoring
- Likert mapping (German 6-point) to 1–6.
- Per-item numeric columns `ctsq_<sub>_<item>_num` created.
- Subscale means `_av` computed correctly with NA handling.
- Z-scores `_z` return NA on zero/NA variance.

## CRT Scoring
- Correct answers applied (8 items); text/numeric tolerance.
- Seen flags (`CRT_gesehen_*`) exclude items from scoring and counts tracked.
- Aid flags (`CRT_hilfsmittel_2/3/4`) drop all CRT items for that participant.
- Proportion `crt_av`, count `crt_num`, z-score `crt_z` computed on remaining items.
- Rows with all CRT dropped flagged via `crt_all_dropped`.

## HB Scoring
- Correct options per HB1–HB12 (HB5 = always red; HB12 = value 2).
- Outcome-bias rule: HB13/14 correct when ratings equal.
- Aid flags (`HB_hilfsmittel_2/3/4`) drop all HB items.
- Proportion `hb_av`, count `hb_num`, z-score `hb_z`; `hb_all_dropped` flagged.

## SES
- income_per_member = income / household members; z of income, education, profession; SES = mean z’s; SES_z computed with zero-variance protection.

## Timings
- duration_num, crt_time_av, hb_time_av computed from timing columns; numeric coercion with NA handling.

## Drop Summary & Final Data
- summarize_drops() counts crt_all_dropped, hb_all_dropped, both; total_start matches pre-drop n.
- apply_drop_logic() removes rows with both CRT and HB dropped.

## Exports
- Outputs exist: `outputs/analysis_dataset.csv`, `outputs/drop_summary.csv`.
- analysis_dataset columns include: scored CTSQ/CRT/HB fields, SES, timings, flags (seen/hilfsmittel), z-scores, drop indicators.
