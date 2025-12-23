#' Extract value/label pairs for all variables
#'
#' @param data A data frame/tibble read via haven (labels preserved).
#' @return A list/tibble with varname, var_label, value_labels (list of value/label pairs), class.
extract_value_labels <- function(data) {
  vars <- names(data)
  res <- lapply(vars, function(v) {
    x <- data[[v]]
    lbls <- attr(x, "labels", exact = TRUE)
    var_lab <- attr(x, "label", exact = TRUE)
    value_labels <- if (!is.null(lbls) && length(lbls)) {
      mapply(
        function(val, lab) list(value = unname(val), label = lab),
        as.vector(unname(lbls)),
        names(lbls),
        SIMPLIFY = FALSE
      )
    } else {
      list()
    }
    list(
      varname = v,
      var_label = if (!is.null(var_lab)) var_lab else "",
      class = paste(class(x), collapse = "|"),
      value_labels = value_labels
    )
  })
  res
}

#' Write all value/label pairs to JSON
#' @param data A data frame/tibble with haven labels.
#' @param path Output JSON path.
#' @return The path (invisibly).
write_value_labels_json <- function(data, path) {
  vals <- extract_value_labels(data)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(vals, path, pretty = TRUE, auto_unbox = TRUE)
  invisible(path)
}
