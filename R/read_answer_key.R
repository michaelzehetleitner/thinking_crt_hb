#' Read CRT/HB answer key from JSON
#'
#' @param path Path to hb_crt_answer_key.json
#' @return Tibble with columns item, scale, text, responses, accept_value, accept_label, notes.
read_answer_key <- function(path) {
  if (!file.exists(path)) stop("Answer key file not found: ", path)
  json <- jsonlite::read_json(path, simplifyVector = TRUE)
  tibble::as_tibble(json)
}
