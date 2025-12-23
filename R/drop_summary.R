#' Summarize drop reasons
#'
#' @param data Tibble after scoring (with flags: crt_all_dropped, hb_all_dropped, crt_aid_used, hb_aid_used)
#' @param total_start Number of rows before any prereg/pipeline drops.
#' @param prereg_counts Named list of prereg drop counts (e.g., no_consent, aid_used).
#' @param n_after_prereg Rows remaining after prereg exclusions (optional).
#' @return Tibble with reason and count.
summarize_drops <- function(data, total_start, prereg_counts = list(), n_after_prereg = NA_integer_) {
  if (is.null(total_start) || is.na(total_start)) total_start <- nrow(data)
  if (is.null(n_after_prereg) || is.na(n_after_prereg)) n_after_prereg <- nrow(data)

  prereg_tbl <- if (length(prereg_counts)) {
    tibble::tibble(
      reason = c("total_after_prereg", names(prereg_counts)),
      count = c(n_after_prereg, unname(unlist(prereg_counts))),
      total_start = total_start
    )
  } else {
    tibble::tibble(reason = character(), count = numeric(), total_start = numeric())
  }

  reasons <- list(
    crt_all_dropped = sum(data$crt_all_dropped, na.rm = TRUE),
    hb_all_dropped = sum(data$hb_all_dropped, na.rm = TRUE),
    both_crt_hb_dropped = sum(data$crt_all_dropped & data$hb_all_dropped, na.rm = TRUE)
  )
  pipeline_tbl <- tibble::tibble(
    reason = c("total_after_filters", names(reasons)),
    count = c(nrow(data), unname(unlist(reasons))),
    total_start = total_start
  )

  dplyr::bind_rows(prereg_tbl, pipeline_tbl)
}
