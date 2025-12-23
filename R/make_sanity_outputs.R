#' Generate quick sanity tables/plots for the analysis dataset
#'
#' Non-blocking; intended as an optional target.
#'
#' @param data Tibble after prepare_analysis_export().
#' @return Invisibly, a list with paths to generated files.
make_sanity_outputs <- function(data) {
  dir.create("outputs/sanity", showWarnings = FALSE)

  # Table of drop reasons
  drop_path <- file.path("outputs", "drop_summary.csv")
  if (file.exists(drop_path)) {
    file.copy(drop_path, file.path("outputs/sanity", "drop_summary.csv"), overwrite = TRUE)
  }

  # Distributions for CRT/HB proportions
  plot_dist <- function(x, name) {
    df <- tibble::tibble(val = x) |>
      dplyr::filter(is.finite(.data$val))
    p <- ggplot2::ggplot(df, ggplot2::aes(x = val)) +
      ggplot2::geom_histogram(binwidth = 0.1, boundary = 0, fill = "#4e79a7", color = "white") +
      ggplot2::labs(title = paste(name, "distribution"), x = name, y = "Count") +
      ggplot2::coord_cartesian(xlim = c(0, 1))
    out <- file.path("outputs/sanity", paste0(name, "_hist.png"))
    ggplot2::ggsave(out, p, width = 5, height = 3, dpi = 150, device = "png", type = "cairo")
    out
  }

  paths <- list()
  if ("CRT_av" %in% names(data)) paths$crt <- plot_dist(data$CRT_av, "CRT_av")
  if ("HB_av" %in% names(data)) paths$hb <- plot_dist(data$HB_av, "HB_av")

  # CTSQ subscale histograms (z-scores)
  ctsq_cols <- intersect(names(data), c("CTSQ_AOT_z", "CTSQ_PIT_z", "CTSQ_CMT_z", "CTSQ_PET_z"))
  if (length(ctsq_cols)) {
    long <- tidyr::pivot_longer(
      tibble::as_tibble(data),
      cols = dplyr::all_of(ctsq_cols),
      names_to = "scale",
      values_to = "z"
    )
    p <- ggplot2::ggplot(long, ggplot2::aes(x = z)) +
      ggplot2::geom_histogram(binwidth = 0.25, fill = "#f28e2b", color = "white") +
      ggplot2::facet_wrap(~scale, scales = "free_y") +
      ggplot2::labs(title = "CTSQ subscale z-score distributions", x = "z", y = "Count")
    out <- file.path("outputs/sanity", "ctsq_z_hists.png")
    ggplot2::ggsave(out, p, width = 7, height = 4, dpi = 150)
    paths$ctsq <- out
  }

  invisible(paths)
}
