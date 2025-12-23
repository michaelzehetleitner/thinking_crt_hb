#' APA-styled ggplot theme
#'
#' Light, print-friendly theme aligned with typical APA figure conventions:
#' black axes, minimal grids, and bottom legends.
#'
#' @param base_size Numeric base font size.
#' @param base_family Base font family.
#' @return A ggplot2 theme object.
theme_apa <- function(base_size = 11, base_family = "sans") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#d0d0d0", linewidth = 0.25),
      panel.border = ggplot2::element_rect(color = "black", fill = NA, linewidth = 0.4),
      axis.line = ggplot2::element_line(color = "black", linewidth = 0.4),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      legend.position = "bottom",
      strip.background = ggplot2::element_rect(color = "black", fill = "white", linewidth = 0.4),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

#' Generate descriptive CRT/HB performance plots (PNG)
#'
#' Creates APA-themed figures for score distributions, item accuracy, CRT–HB
#' association, and speed–accuracy patterns, writing PNGs to `output_dir`.
#'
#' @param data Tibble after scoring and timing (needs *_num, *_av, *_time_av).
#' @param answer_key Tibble with columns `item`, `scale`, and `text`.
#' @param output_dir Directory to write PNG outputs (created if missing).
#' @return Character vector of saved PNG paths.
plot_crt_hb_performance <- function(data, answer_key, output_dir = "outputs/performance") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for plotting.")
  }
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

  paths <- list()

  # Score distributions ----------------------------------------------------
  crt_scores <- pick_column(data, c("crt_av", "CRT_av"))
  hb_scores <- pick_column(data, c("hb_av", "HB_av"))
  dist_df <- tibble::tibble(
    scale = factor(
      c(rep("CRT", length(crt_scores)), rep("HB", length(hb_scores))),
      levels = c("CRT", "HB")
    ),
    score = c(crt_scores, hb_scores)
  ) |>
    dplyr::filter(is.finite(.data$score))

  if (nrow(dist_df)) {
    mu <- dist_df |>
      dplyr::group_by(.data$scale) |>
      dplyr::summarise(mean_score = mean(.data$score), .groups = "drop")

    p_dist <- ggplot2::ggplot(dist_df, ggplot2::aes(x = .data$score, fill = .data$scale)) +
      ggplot2::geom_histogram(
        position = "identity", alpha = 0.55, bins = 20,
        color = "white"
      ) +
      ggplot2::geom_vline(
        data = mu,
        ggplot2::aes(xintercept = .data$mean_score, color = .data$scale),
        linetype = "dashed", linewidth = 0.6
      ) +
      ggplot2::scale_fill_manual(values = c(CRT = "#4e79a7", HB = "#f28e2b")) +
      ggplot2::scale_color_manual(values = c(CRT = "#4e79a7", HB = "#f28e2b"), guide = "none") +
      ggplot2::coord_cartesian(xlim = c(0, 1)) +
      ggplot2::labs(
        title = "CRT and HB score distributions",
        x = "Proportion correct",
        y = "Participants",
        fill = "Scale"
      ) +
      theme_apa()

    dist_path <- file.path(output_dir, "score_distributions.png")
    ggplot2::ggsave(dist_path, p_dist, width = 6.5, height = 4.2, dpi = 300, device = "png")
    paths$score_distributions <- dist_path
  }

  # Item accuracy ----------------------------------------------------------
  item_perf <- dplyr::bind_rows(
    compute_item_performance(data, answer_key, "CRT"),
    compute_item_performance(data, answer_key, "HB")
  )

  if (nrow(item_perf)) {
    p_items <- ggplot2::ggplot(item_perf, ggplot2::aes(x = .data$item, y = .data$prop, fill = .data$scale)) +
      ggplot2::geom_col(color = "black", width = 0.72) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
        width = 0.18,
        linewidth = 0.35
      ) +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%.0f/%d", .data$correct, .data$n)),
        vjust = -0.35,
        size = 3
      ) +
      ggplot2::scale_y_continuous(
        labels = function(x) paste0(round(x * 100), "%"),
        limits = c(0, 1)
      ) +
      ggplot2::scale_fill_manual(values = c(CRT = "#4e79a7", HB = "#f28e2b"), guide = "none") +
      ggplot2::facet_wrap(~.data$scale, scales = "free_x", ncol = 1) +
      ggplot2::labs(
        title = "Item accuracy with 95% CI",
        x = "Item",
        y = "Proportion correct"
      ) +
      theme_apa() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        strip.text = ggplot2::element_text(face = "bold")
      )

    item_path <- file.path(output_dir, "item_accuracy.png")
    ggplot2::ggsave(item_path, p_items, width = 7.2, height = 6.8, dpi = 300, device = "png")
    paths$item_accuracy <- item_path
  }

  # CRT vs HB scatter ------------------------------------------------------
  scatter_df <- tibble::tibble(
    crt = crt_scores,
    hb = hb_scores
  )
  if (length(crt_scores) == length(hb_scores) && length(crt_scores)) {
    scatter_df <- scatter_df |>
      dplyr::filter(is.finite(.data$crt) & is.finite(.data$hb))
  } else {
    scatter_df <- tibble::tibble()
  }

  if (nrow(scatter_df)) {
    r_val <- stats::cor(scatter_df$crt, scatter_df$hb, use = "complete.obs")
    p_scatter <- ggplot2::ggplot(scatter_df, ggplot2::aes(x = .data$crt, y = .data$hb)) +
      ggplot2::geom_point(color = "#4e79a7", alpha = 0.65, size = 2) +
      ggplot2::geom_smooth(method = "lm", se = TRUE, color = "#e15759", linewidth = 0.6) +
      ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
      ggplot2::labs(
        title = "Association between CRT and HB performance",
        x = "CRT proportion correct",
        y = "HB proportion correct",
        caption = if (!is.na(r_val)) sprintf("Pearson r = %.2f", r_val) else NULL
      ) +
      theme_apa()

    scatter_path <- file.path(output_dir, "crt_hb_scatter.png")
    ggplot2::ggsave(scatter_path, p_scatter, width = 6.2, height = 4.4, dpi = 300, device = "png")
    paths$crt_hb_scatter <- scatter_path
  }

  # Speed–accuracy patterns ------------------------------------------------
  time_df <- build_speed_accuracy_df(data)

  if (nrow(time_df)) {
    p_speed <- ggplot2::ggplot(time_df, ggplot2::aes(x = .data$time, y = .data$accuracy, color = .data$scale)) +
      ggplot2::geom_point(alpha = 0.6, size = 2) +
      ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.7) +
      ggplot2::scale_color_manual(values = c(CRT = "#4e79a7", HB = "#f28e2b")) +
      ggplot2::labs(
        title = "Speed–accuracy relationship",
        x = "Mean item time (seconds)",
        y = "Proportion correct",
        color = "Scale"
      ) +
      ggplot2::coord_cartesian(ylim = c(0, 1)) +
      theme_apa()

    speed_path <- file.path(output_dir, "speed_accuracy.png")
    ggplot2::ggsave(speed_path, p_speed, width = 6.5, height = 4.2, dpi = 300, device = "png")
    paths$speed_accuracy <- speed_path
  }

  unname(unlist(paths))
}

# Helpers -----------------------------------------------------------------

pick_column <- function(data, candidates) {
  for (nm in candidates) {
    if (nm %in% names(data)) return(data[[nm]])
  }
  rep(NA_real_, nrow(data))
}

compute_item_performance <- function(data, answer_key, scale) {
  prefix <- tolower(scale)
  num_cols <- grep(paste0("^", prefix, "_\\d+_num$"), names(data), value = TRUE)
  if (!length(num_cols)) {
    return(tibble::tibble())
  }

  ordered_items <- answer_key$item[answer_key$scale == scale]

  perf <- data |>
    dplyr::select(dplyr::all_of(num_cols)) |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "col",
      values_to = "score"
    ) |>
    dplyr::mutate(
      item = toupper(sub("_NUM$", "", .data$col)),
      score = suppressWarnings(as.numeric(.data$score))
    ) |>
    dplyr::group_by(.data$item, .add = FALSE) |>
    dplyr::summarise(
      n = sum(!is.na(score)),
      correct = sum(score == 1, na.rm = TRUE),
      prop = ifelse(n > 0, correct / n, NA_real_),
      se = sqrt(prop * (1 - prop) / pmax(n, 1)),
      lower = pmax(0, prop - 1.96 * se),
      upper = pmin(1, prop + 1.96 * se),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      scale = scale,
      label = .data$item
    )

  meta <- answer_key |>
    dplyr::filter(.data$scale == scale) |>
    dplyr::select("item", "text")

  perf <- perf |>
    dplyr::left_join(meta, by = "item") |>
    dplyr::mutate(
      label = ifelse(
        !is.na(.data$text),
        paste0(.data$item, ": ", abbreviate(.data$text, minlength = 32)),
        .data$item
      )
    )

  if (length(ordered_items)) {
    ordered_items <- ordered_items[ordered_items %in% perf$item]
    perf$item <- factor(perf$item, levels = ordered_items, ordered = TRUE)
  }

  perf
}

build_speed_accuracy_df <- function(data) {
  crt_scores <- pick_column(data, c("crt_av", "CRT_av"))
  hb_scores <- pick_column(data, c("hb_av", "HB_av"))
  crt_time <- pick_column(data, c("crt_time_av", "CRT_time_av"))
  hb_time <- pick_column(data, c("hb_time_av", "HB_time_av"))

  df <- dplyr::bind_rows(
    tibble::tibble(scale = "CRT", accuracy = crt_scores, time = crt_time),
    tibble::tibble(scale = "HB", accuracy = hb_scores, time = hb_time)
  )

  df |>
    dplyr::filter(is.finite(.data$accuracy) & is.finite(.data$time))
}
