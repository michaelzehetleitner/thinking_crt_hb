#' Summarise per-item CRT/HB correctness
#'
#' Produces tidy summaries of correct/incorrect/NA counts for each CRT and HB item
#' using the scored *_num columns. Writes CSVs into outputs/sanity/.
#'
#' @param data Tibble after scoring (must include crt_*_num, hb_*_num and raw CRT/HB item columns).
#' @param item_meta Optional list/data frame of item metadata (fields: scale, varname).
#'   If NULL, tries to read 'outputs/hb_crt_items.json'.
#' @return Invisibly, list of file paths.
summarise_crt_hb_items <- function(data, item_meta = NULL) {
  dir.create("outputs/sanity", showWarnings = FALSE)

  if (is.null(item_meta)) {
    meta_path <- file.path("outputs", "hb_crt_items.json")
    if (file.exists(meta_path)) {
      item_meta <- jsonlite::read_json(meta_path, simplifyVector = TRUE)
    }
  }

  summarise_scale <- function(scale) {
    cols <- grep(paste0("^", tolower(scale), "_.*_num$"), names(data), value = TRUE)
    if (!length(cols)) return(NULL)

    # raw responses and metadata, if available
    raw_cols <- character()
    resp_lut <- tibble::tibble()
    text_lut <- tibble::tibble()
    seen_cols <- character()
    if (!is.null(item_meta) && "varname" %in% names(item_meta)) {
      raw_cols <- item_meta$varname[item_meta$scale == scale]
      raw_cols <- intersect(raw_cols, names(data))
      if ("responses" %in% names(item_meta)) {
        resp_lut <- purrr::map_dfr(
          seq_len(nrow(item_meta)),
          function(i) {
            resps <- item_meta$responses[[i]]
            if (length(resps) == 0) return(NULL)
            var <- item_meta$varname[i]
            purrr::map_dfr(resps, ~tibble::tibble(
              varname = var,
              response_value = as.character(.x$value),
              response_label = as.character(.x$label)
            ))
          }
        )
      }
      if ("item" %in% names(item_meta) && "text" %in% names(item_meta)) {
        text_lut <- tibble::tibble(
          item = paste0(tolower(item_meta$item[item_meta$scale == scale]), "_num"),
          item_text = item_meta$text[item_meta$scale == scale]
        )
      }
      if (scale == "CRT") {
        seen_cols <- vapply(raw_cols, seen_col_for_item, character(1))
        seen_cols <- intersect(seen_cols, names(data))
      }
    }

    wide <- tibble::as_tibble(data) |>
      dplyr::mutate(rowid = dplyr::row_number())
    long <- tidyr::pivot_longer(
      wide,
      cols = dplyr::all_of(cols),
      names_to = "item",
      values_to = "score"
    )
    # attach raw responses if available
    if (length(raw_cols)) {
      raw_long <- wide |>
        dplyr::select(dplyr::all_of(raw_cols), rowid) |>
        dplyr::mutate(dplyr::across(-rowid, as.character)) |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(raw_cols),
          names_to = "varname",
          values_to = "response"
        ) |>
        dplyr::mutate(
          item = paste0(tolower(.data$varname), "_num")
        )
      if (nrow(resp_lut)) {
        raw_long <- raw_long |>
          dplyr::left_join(resp_lut, by = c("varname", "response" = "response_value")) |>
          dplyr::mutate(response_display = dplyr::coalesce(.data$response_label, .data$response))
      } else {
        raw_long <- raw_long |>
          dplyr::mutate(response_display = .data$response)
      }
      long <- dplyr::left_join(long, raw_long[, c("rowid", "item", "response", "response_display")], by = c("rowid", "item"))
    }

    # seen flags (CRT only)
    if (length(seen_cols)) {
      seen_long <- wide |>
        dplyr::select(dplyr::all_of(seen_cols), rowid) |>
        tidyr::pivot_longer(
          cols = dplyr::all_of(seen_cols),
          names_to = "seen_var",
          values_to = "seen_raw"
        ) |>
        dplyr::mutate(
          seen = tolower(trimws(as.character(.data$seen_raw))) %in% c("1", "ja", "yes", "true"),
          item = paste0("crt_", sub("^crt_gesehen_", "", tolower(.data$seen_var)), "_num")
        ) |>
        dplyr::select(rowid, item, seen)
      long <- dplyr::left_join(long, seen_long, by = c("rowid", "item"))
    } else {
      long$seen <- FALSE
    }

    long |>
      dplyr::mutate(
        status = dplyr::case_when(
          .data$seen ~ "seen",
          is.na(.data$score) ~ "missing",
          .data$score == 1 ~ "correct",
          .data$score == 0 ~ "incorrect",
          TRUE ~ "other"
        ),
        response_display = dplyr::coalesce(.data$response_display, .data$response)
      ) |>
      dplyr::left_join(text_lut, by = "item") |>
      dplyr::mutate(item_text = dplyr::coalesce(.data$item_text, NA_character_)) |>
      dplyr::group_by(item, item_text, response_display, status) |>
      dplyr::summarise(n = dplyr::n(), .groups = "drop_last") |>
      tidyr::pivot_wider(
        names_from = "status",
        values_from = "n",
        values_fill = 0
      ) |>
      dplyr::ungroup()
  }

  crt_tbl <- summarise_scale("CRT")
  hb_tbl <- summarise_scale("HB")

  # Special summary for HB outcome-bias pair (HB_13/HB_14)
  hb_pair_tbl <- NULL
  if (all(c("HB_13", "HB_14", "hb_13_num") %in% names(data))) {
    # response label lookup for HB
    hb_resp_lut <- tibble::tibble()
    if (!is.null(item_meta) && "responses" %in% names(item_meta)) {
      hb_resp_lut <- purrr::map_dfr(
        seq_len(nrow(item_meta)),
        function(i) {
          resps <- item_meta$responses[[i]]
          if (length(resps) == 0) return(NULL)
          var <- item_meta$varname[i]
          purrr::map_dfr(resps, ~tibble::tibble(
            varname = var,
            response_value = as.character(.x$value),
            response_label = as.character(.x$label)
          ))
        }
      )
    }

    pair <- tibble::tibble(
      hb_13 = as.character(data$HB_13),
      hb_14 = as.character(data$HB_14),
      score = data$hb_13_num
    )
    # map labels separately to avoid the odd join above
    hb13_lut <- hb_resp_lut[hb_resp_lut$varname == "HB_13", c("response_value", "response_label")]
    hb14_lut <- hb_resp_lut[hb_resp_lut$varname == "HB_14", c("response_value", "response_label")]
    if (nrow(hb13_lut)) {
      pair <- pair |>
        dplyr::left_join(hb13_lut, by = c("hb_13" = "response_value")) |>
        dplyr::rename(hb_13_label = response_label)
    } else {
      pair$hb_13_label <- pair$hb_13
    }
    if (nrow(hb14_lut)) {
      pair <- pair |>
        dplyr::left_join(hb14_lut, by = c("hb_14" = "response_value")) |>
        dplyr::rename(hb_14_label = response_label)
    } else {
      pair$hb_14_label <- pair$hb_14
    }

    hb_pair_tbl <- pair |>
      dplyr::mutate(
        status = dplyr::case_when(
          is.na(.data$score) ~ "missing",
          .data$score == 1 ~ "correct",
          .data$score == 0 ~ "incorrect",
          TRUE ~ "other"
        )
      ) |>
      dplyr::count(.data$hb_13_label, .data$hb_14_label, .data$status, name = "n") |>
      tidyr::pivot_wider(
        names_from = "status",
        values_from = "n",
        values_fill = 0
      ) |>
      dplyr::arrange(.data$hb_13_label, .data$hb_14_label)
  }

  paths <- list()
  if (!is.null(crt_tbl)) {
    paths$crt <- file.path("outputs/sanity", "crt_item_correctness.csv")
    readr::write_csv(crt_tbl, paths$crt)
  }
  if (!is.null(hb_tbl)) {
    paths$hb <- file.path("outputs/sanity", "hb_item_correctness.csv")
    readr::write_csv(hb_tbl, paths$hb)
  }
  if (!is.null(hb_pair_tbl)) {
    paths$hb_pair <- file.path("outputs/sanity", "hb_outcome_pair_correctness.csv")
    readr::write_csv(hb_pair_tbl, paths$hb_pair)
  }

  invisible(paths)
}
