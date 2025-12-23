# Reproducible data pipeline for CTSQ/CRT/HB follow-up study

library(targets)

tar_option_set(
  packages = c("haven", "tibble", "summarytools", "dplyr", "purrr", "jsonlite", "readr", "ggplot2", "tidyr"),
  format = "rds"
)

tar_source("R")

list(
  # Input paths
  tar_target(
    raw_data_path,
    "data/4-CTSQ-D+Follow+Up_18+December+2025_13.34.sav",
    format = "file"
  ),
  tar_target(
    answer_key_path,
    "data/hb_crt_answer_key.json",
    format = "file"
  ),

  # Load data and answer key
  tar_target(raw_data, read_raw_data(raw_data_path)),
  tar_target(
    raw_data_csv,
    {
      dir.create("outputs", showWarnings = FALSE)
      readr::write_csv(raw_data, "outputs/raw_data_debug.csv", na = "")
      "outputs/raw_data_debug.csv"
    },
    format = "file"
  ),
  tar_target(answer_key, read_answer_key(answer_key_path)),

  # Inclusion criteria + prereg exclusions
  tar_target(analysis_sample, filter_sample(raw_data) |> apply_prereg_exclusions()),

  # Descriptives
  tar_target(dfsummary_html, make_dfsummary_html(analysis_sample, "outputs/dfsummary.html"), format = "file"),
  tar_target(item_metadata, extract_crt_hb_metadata(raw_data)),
  tar_target(
    item_metadata_json,
    {
      dir.create("outputs", showWarnings = FALSE)
      jsonlite::write_json(item_metadata, "outputs/hb_crt_items.json", pretty = TRUE, auto_unbox = TRUE)
      "outputs/hb_crt_items.json"
    },
    format = "file"
  ),
  tar_target(
    value_labels_json,
    write_value_labels_json(raw_data, "outputs/value_labels.json"),
    format = "file"
  ),

  # Scoring pipeline
  tar_target(scored_ctsq, score_ctsq(analysis_sample)),
  tar_target(scored_crt, score_crt(scored_ctsq, answer_key)),
  tar_target(scored_hb, score_hb(scored_crt, answer_key)),
  tar_target(with_ses, compute_ses(scored_hb)),
  tar_target(with_timings, compute_timings(with_ses)),
  tar_target(
    crt_hb_performance_plots,
    plot_crt_hb_performance(with_timings, answer_key, "outputs/performance"),
    format = "file"
  ),
  tar_target(
    drop_summary,
    summarize_drops(
      with_timings,
      total_start = attr(analysis_sample, "n_before_prereg"),
      prereg_counts = attr(analysis_sample, "prereg_drop_counts"),
      n_after_prereg = nrow(analysis_sample)
    )
  ),
  tar_target(
    crt_seen_plot,
    plot_crt_seen(with_timings, "outputs/crt_seen_reasons.png"),
    format = "file"
  ),
  tar_target(final_data, apply_drop_logic(with_timings) |>
    prepare_analysis_export() |>
    validate_export()),
  tar_target(
    drop_summary_csv,
    {
      dir.create("outputs", showWarnings = FALSE)
      readr::write_csv(drop_summary, "outputs/drop_summary.csv", na = "")
      "outputs/drop_summary.csv"
    },
    format = "file"
  ),

  # Export analysis-ready dataset
  tar_target(
    analysis_csv,
    {
      dir.create("outputs", showWarnings = FALSE)
      readr::write_csv(final_data, "outputs/analysis_dataset.csv", na = "")
      "outputs/analysis_dataset.csv"
    },
    format = "file"
  ),

  # Optional sanity outputs (non-blocking for JASP)
  tar_target(
    sanity_outputs,
    {
      make_sanity_outputs(final_data)
      summarise_crt_hb_items(with_timings, item_metadata)
    }
  )
)
