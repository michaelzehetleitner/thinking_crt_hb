#' Write a skeleton answer key template for CRT and HB items
#'
#' The template lists each CRT/HB item with its response labels (if any) and
#' empty `accept_value`/`accept_label` fields for manual completion.
#'
#' @param metadata_tbl Tibble produced by extract_crt_hb_metadata().
#' @param path Output path for the template JSON.
#' @return Invisible path.
write_answer_key_template <- function(metadata_tbl, path) {
  if (nrow(metadata_tbl) == 0) {
    stop("No metadata supplied to write template.")
  }

  template <- purrr::pmap(
    metadata_tbl,
    function(scale, item, varname, text, responses) {
      list(
        item = item,
        scale = scale,
        text = text,
        responses = responses,
        accept_value = NULL,
        accept_label = NULL,
        notes = ""
      )
    }
  )

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(template, path, pretty = TRUE, auto_unbox = TRUE)
  invisible(path)
}
