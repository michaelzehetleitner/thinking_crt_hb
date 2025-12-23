#' Filter responses by RecordedDate (date part)
#'
#' Qualtrics stores `RecordedDate` as a character timestamp (e.g.,
#' "2025-12-11 01:40:37 GMT"). This helper keeps rows whose date part matches
#' the requested date.
#'
#' @param data A data frame containing a `RecordedDate` column.
#' @param date_keep A Date scalar to keep.
#' @return Filtered tibble with only rows from `date_keep`.
filter_by_recorded_date <- function(data, date_keep) {
  if (!"RecordedDate" %in% names(data)) {
    stop("RecordedDate column not found in data.")
  }
  date_strings <- sub(" [A-Za-z]+$", "", data$RecordedDate)
  date_only <- as.Date(substr(date_strings, 1, 10))
  tibble::as_tibble(data[date_only == date_keep, , drop = FALSE])
}
