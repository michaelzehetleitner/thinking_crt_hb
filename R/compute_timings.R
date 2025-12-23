#' Compute timing summaries
#'
#' Extracts survey duration and mean page submit times for CRT and HB items.
#'
#' @param data Tibble with timing columns from Qualtrics.
#' @return Tibble with duration_num, crt_time_av, hb_time_av.
compute_timings <- function(data) {
  out <- data

  # Survey duration
  dur_col <- names(dplyr::select(out, dplyr::matches("^Duration")))
  if (length(dur_col) == 0) dur_col <- names(dplyr::select(out, dplyr::matches("Duration")))
  if (length(dur_col) > 0) {
    out$duration_num <- suppressWarnings(as.numeric(out[[dur_col[1]]]))
  }

  # CRT timings (Qualtrics exports use Page_Submit with underscore)
  crt_time_cols <- timing_cols(out, "CRT")
  if (length(crt_time_cols)) {
    out[crt_time_cols] <- lapply(out[crt_time_cols], function(x) suppressWarnings(as.numeric(x)))
    out$crt_time_av <- rowMeans(out[crt_time_cols], na.rm = TRUE)
  }

  # HB timings
  hb_time_cols <- timing_cols(out, "HB")
  if (length(hb_time_cols)) {
    out[hb_time_cols] <- lapply(out[hb_time_cols], function(x) suppressWarnings(as.numeric(x)))
    out$hb_time_av <- rowMeans(out[hb_time_cols], na.rm = TRUE)
  }

  # Drop auxiliary timing fields; keep per-item Page_Submit plus averages
  out <- out |>
    dplyr::select(
      -dplyr::matches("^(CRT|HB)_\\d+_timing_(First_Click|Last_Click|Click_Count)$")
    )

  out
}
