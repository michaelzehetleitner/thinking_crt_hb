#' Render an HTML dfSummary report
#'
#' Uses `summarytools::dfSummary()` to create a descriptive table of the dataset
#' and writes it to an HTML file.
#'
#' @param data A data frame or tibble.
#' @param output_path Path where the HTML file should be written.
#'
#' @return The output path (invisibly), useful for targets file tracking.
make_dfsummary_html <- function(data, output_path) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  old_opts <- summarytools::st_options(plain.ascii = FALSE, style = "grid")
  on.exit(summarytools::st_options(old_opts), add = TRUE)

  # summarytools requires a short directory name (1-5 chars) for tmp.img.dir.
  # Create it in the working directory so the relative path is valid.
  img_dir <- "img"
  dir.create(img_dir, showWarnings = FALSE)

  dfsum <- summarytools::dfSummary(
    data,
    varnumbers = TRUE,
    valid.col = TRUE,
    graph.magnif = 0.8,
    tmp.img.dir = img_dir
  )

  summarytools::view(
    dfsum,
    file = output_path,
    footnote = NA,
    silent = TRUE
  )

  invisible(output_path)
}
