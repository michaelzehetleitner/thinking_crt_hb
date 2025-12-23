#' Apply final drop logic: remove rows with both CRT and HB dropped
#'
#' @param data Tibble after scoring.
#' @return Filtered tibble.
apply_drop_logic <- function(data) {
  data[!(data$crt_all_dropped & data$hb_all_dropped), , drop = FALSE]
}
