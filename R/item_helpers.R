#' Helper utilities for CTSQ/CRT/HB items
#'
#' Centralises item lists and related column-name helpers so scoring functions
#' do not rely on scattered regex `grep()` calls.

#' Get item names for a scale from the answer key
#' @param answer_key Tibble with columns `item` and `scale`.
#' @param scale Character string, e.g., "CRT" or "HB".
#' @return Character vector of item names belonging to the scale.
get_items <- function(answer_key, scale) {
  items <- answer_key$item[answer_key$scale == scale]
  items[grepl(paste0("^", scale, "_\\d"), items)]
}

#' Map a CRT item (e.g., CRT_3_4) to its seen-flag column (CRT_gesehen_3)
seen_col_for_item <- function(item) {
  idx <- sub("^CRT_(\\d+).*$", "\\1", item)
  paste0("CRT_gesehen_", idx)
}

#' Qualtrics aid flag columns
crt_aid_cols <- function(data) intersect(names(data), c("CRT_hilfsmittel_2", "CRT_hilfsmittel_3", "CRT_hilfsmittel_4"))
hb_aid_cols  <- function(data) intersect(names(data), c("HB_hilfsmittel_2", "HB_hilfsmittel_3", "HB_hilfsmittel_4"))

#' Timing columns for a given prefix (CRT/HB)
timing_cols <- function(data, prefix) {
  patt <- paste0("^", prefix, "_\\d+_timing_Page[_ ]Submit$")
  names(dplyr::select(data, dplyr::matches(patt)))
}

#' CTSQ item metadata derived from column names
#' @param data Data frame containing CTSQ_* columns.
#' @return Tibble with var, subscale, item_id, num_col.
ctsq_items <- function(data) {
  vars <- names(dplyr::select(data, dplyr::starts_with("CTSQ_")))
  if (!length(vars)) return(tibble::tibble())
  tibble::tibble(var = vars) |>
    dplyr::mutate(
      subscale = tolower(sub("^CTSQ_([^_]+)_.*$", "\\1", var)),
      item_id = sub(".*_(\\d+)$", "\\1", var),
      num_col = paste0("ctsq_", subscale, "_", item_id, "_num")
    )
}
