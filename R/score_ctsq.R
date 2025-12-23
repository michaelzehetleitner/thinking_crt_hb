#' Score CTSQ subscales (data-driven)
#'
#' Derives subscales and item numbers from CTSQ_* column names, converts
#' labelled/character Likert responses to their encoded numeric values, and
#' computes per-subscale means and z-scores. No hard-coded item ranges or
#' response maps are required; value labels stored in the data are used when
#' present.
#'
#' @param data Tibble containing CTSQ items (columns starting with `CTSQ_`).
#' @return Tibble with added *_num item columns, *_av means, *_z z-scores.
score_ctsq <- function(data) {
  meta <- ctsq_items(data)
  if (nrow(meta) == 0) return(data)

  # Only the four CTSQ subscales should be averaged
  valid_subscales <- c("aot", "cmt", "pit", "pet")

  # Convert a single CTSQ item to numeric using its encoded values if available.
  to_num <- function(x) {
    # labelled vectors keep numeric codes; drop labels but retain codes
    if (inherits(x, "labelled")) return(as.numeric(haven::zap_labels(x)))

    labels <- attr(x, "labels", exact = TRUE)
    if (!is.null(labels) && length(labels)) {
      mapping <- stats::setNames(as.character(unname(labels)), names(labels))
      return(suppressWarnings(as.numeric(dplyr::recode(as.character(x), !!!mapping))))
    }

    suppressWarnings(as.numeric(x))
  }

  scale_with_attrs <- function(x) {
    mu <- mean(x, na.rm = TRUE)
    sdv <- stats::sd(x, na.rm = TRUE)
    z <- if (is.na(sdv) || sdv == 0) rep(NA_real_, length(x)) else (x - mu) / sdv
    attr(z, "scaled:center") <- mu
    attr(z, "scaled:scale") <- sdv
    z
  }

  base <- data |>
    dplyr::mutate(.row_id = dplyr::row_number())

  long <- base |>
    tidyr::pivot_longer(
      cols = dplyr::starts_with("CTSQ_"),
      names_to = "var",
      values_to = "resp"
    ) |>
    dplyr::left_join(meta, by = c("var")) |>
    dplyr::mutate(resp_num = to_num(resp)) |>
    dplyr::filter(.data$subscale %in% valid_subscales)

  # per-item numeric wide
  wide_num <- long |>
    dplyr::filter(!is.na(num_col)) |>
    dplyr::select(.row_id, num_col, resp_num) |>
    tidyr::pivot_wider(names_from = num_col, values_from = resp_num)

  # subscale means wide
  wide_av <- long |>
    dplyr::filter(!is.na(subscale)) |>
    dplyr::group_by(.row_id, subscale) |>
    dplyr::summarise(av = mean(resp_num, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(av = ifelse(is.nan(av), NA_real_, av)) |>
    tidyr::pivot_wider(
      names_from = subscale,
      values_from = av,
      names_prefix = "ctsq_",
      names_glue = "ctsq_{subscale}_av"
    )

  out <- base |>
    dplyr::left_join(wide_num, by = ".row_id") |>
    dplyr::left_join(wide_av, by = ".row_id")

  av_cols <- grep("^ctsq_.*_av$", names(out), value = TRUE)

  out |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(av_cols),
        scale_with_attrs,
        .names = "{.col}_z"
      )
    ) |>
    dplyr::rename_with(~ sub("_av_z$", "_z", .x), dplyr::ends_with("_av_z")) |>
    dplyr::select(-.row_id)
}
