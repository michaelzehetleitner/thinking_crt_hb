#' Read raw Qualtrics SAV export
#'
#' Loads the SAV file produced by Qualtrics and returns a tibble without
#' modifying column types. This keeps labelled variables intact so that
#' subsequent steps can decide how to handle them.
#'
#' @param path Path to the `.sav` file.
#' @return A tibble with the raw survey data.
read_raw_data <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path)
  }
  dat <- haven::read_sav(path)
  tibble::as_tibble(dat)
}
