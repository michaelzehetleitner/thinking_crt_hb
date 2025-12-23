#' Validate final analysis dataset against prereg requirements
#'
#' Stops the pipeline if required columns are missing or contain out-of-range /
#' non-finite values.
#'
#' @param data Tibble produced by prepare_analysis_export().
#' @return The same tibble (invisibly) if validation passes.
validate_export <- function(data) {
  required <- c(
    "CTSQ_AOT_z", "CTSQ_PIT_z", "CTSQ_CMT_z", "CTSQ_PET_z",
    "CRT_z", "HB_z",
    "SES", "age_z", "gender_z"
  )

  missing <- setdiff(required, names(data))
  if (length(missing)) {
    stop("Missing prereg-required columns: ", paste(missing, collapse = ", "))
  }

  check_range <- function(x, name) {
    if (any(x < 0 | x > 1, na.rm = TRUE)) {
      stop(name, " must be within [0, 1]")
    }
    invisible(TRUE)
  }

  if ("CRT_av" %in% names(data)) check_range(data$CRT_av, "CRT_av")
  if ("HB_av" %in% names(data)) check_range(data$HB_av, "HB_av")

  numeric_cols <- intersect(required, names(data))
  bad_inf <- vapply(
    numeric_cols,
    function(col) any(is.infinite(data[[col]]) | is.nan(data[[col]]), na.rm = TRUE),
    logical(1)
  )
  if (any(bad_inf)) {
    bad <- names(bad_inf)[bad_inf]
    stop("Non-finite (Inf/NaN) values detected in: ", paste(bad, collapse = ", "))
  }

  invisible(data)
}
