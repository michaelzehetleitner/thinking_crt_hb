#' Extract item text and response options for CRT and HB items
#'
#' Uses variable labels and value labels from the SAV to build a metadata table
#' for CRT and HB items. If value labels are absent, the response options are
#' left empty.
#'
#' @param data A tibble read from the SAV (haven keeps labels).
#' @return A tibble with columns: scale, item, varname, text, responses (list).
extract_crt_hb_metadata <- function(data) {
  # keep CRT/HB items but drop timing fields
  target_vars <- grep("^(CRT_|HB_)", names(data), value = TRUE)
  target_vars <- target_vars[!grepl("timing|Click_Count|Page_Submit|First_Click|Last_Click",
                                    target_vars, ignore.case = TRUE)]

  if (!length(target_vars)) {
    return(tibble::tibble())
  }

  purrr::map_dfr(target_vars, function(v) {
    var <- data[[v]]
    label <- attr(var, "label", exact = TRUE)
    responses <- attr(var, "labels", exact = TRUE)

    # retain value/label pairs if available
    resp_list <- if (length(responses)) {
      purrr::map2(
        as.vector(unname(responses)),
        names(responses),
        ~list(value = .x, label = .y)
      )
    } else {
      list()
    }

    tibble::tibble(
      scale = ifelse(grepl("^CRT_", v), "CRT", "HB"),
      item = v,
      varname = v,
      text = if (!is.null(label)) label else "",
      responses = list(resp_list)
    )
  })
}
