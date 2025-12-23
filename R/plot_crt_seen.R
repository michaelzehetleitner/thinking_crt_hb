#' Create per-item stacked bar plot of CRT familiarity/drops
#'
#' For each participant-item, classify as:
#' - hilfsmittel: participant used aids (drops all items)
#' - seen: item marked seen
#' - scored: otherwise
#'
#' @param data Tibble after scoring (needs crt_aid_used, CRT_gesehen_*, ResponseId/respondiID)
#' @param output_path Path to save PNG.
#' @return Path to saved PNG.
plot_crt_seen <- function(data, output_path) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting.")
  }
  items <- paste0("CRT_gesehen_", 1:8)
  available_items <- items[items %in% names(data)]
  if (!length(available_items)) {
    stop("No CRT_gesehen_* columns found.")
  }

  seen_map <- function(x) tolower(trimws(as.character(x))) %in% c("1", "ja", "yes", "true")

  records <- list()
  for (item in available_items) {
    item_id <- sub("CRT_gesehen_", "CRT_", item)
    seen_vec <- seen_map(data[[item]])
    aid_vec <- data$crt_aid_used %||% rep(FALSE, nrow(data))
    reason <- dplyr::case_when(
      aid_vec ~ "hilfsmittel",
      seen_vec ~ "seen",
      TRUE ~ "scored"
    )
    records[[item]] <- data.frame(
      item = item_id,
      reason = reason,
      stringsAsFactors = FALSE
    )
  }
  plot_df <- dplyr::bind_rows(records) |>
    dplyr::count(item, reason)

  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = item, y = n, fill = reason)) +
    ggplot2::geom_col(color = "black") +
    ggplot2::geom_text(ggplot2::aes(label = n), position = ggplot2::position_stack(vjust = 0.5), size = 3) +
    ggplot2::scale_fill_manual(values = c(
      scored = "#4e79a7",
      seen = "#f28e2b",
      hilfsmittel = "#e15759"
    ), name = "Reason") +
    ggplot2::labs(
      title = "CRT item familiarity and aid usage",
      x = "CRT item",
      y = "Participants"
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "right")

  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  ggplot2::ggsave(output_path, p, width = 6, height = 4, dpi = 150)
  output_path
}

# safe %||% helper
`%||%` <- function(x, y) if (is.null(x)) y else x
