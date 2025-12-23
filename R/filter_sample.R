#' Apply inclusion criteria
#'
#' Keeps cases with workflow == "completed" and age >= 18 (if available).
#'
#' @param data Tibble with columns workflow, demo_age.
#' @return Filtered tibble.
filter_sample <- function(data) {
  out <- data
  if ("workflow" %in% names(out)) {
    out <- out[out$workflow == "completed", , drop = FALSE]
  }
  if ("demo_age" %in% names(out)) {
    age_num <- suppressWarnings(as.numeric(out$demo_age))
    out <- out[is.na(age_num) | age_num >= 18, , drop = FALSE]
  }
  tibble::as_tibble(out)
}
