#' Score CRT items using an external answer key
#'
#' Applies accept_value/accept_label from the answer key, excluding items marked
#' as previously seen (CRT_gesehen_* == 'ja'). Outputs per-item numeric scores,
#' overall proportion, and z-score.
#'
#' @param data Tibble containing CRT item responses and seen flags.
#' @param answer_key Tibble/json list with fields item, accept_value, accept_label.
#' @return Tibble with new columns: crt_<item>_num (0/1), crt_av, crt_z, and
#'   bookkeeping columns crt_seen_count, crt_items_scored.
score_crt <- function(data, answer_key) {
  crt_items <- get_items(answer_key, "CRT")
  if (length(crt_items) == 0) return(data)
  out <- data

  # hilfsmittel flags: any of _2/_3/_4 used => drop all CRT items
  aid_cols <- crt_aid_cols(out)
  aid_used <- if (length(aid_cols)) {
    out |>
      dplyr::select(dplyr::all_of(aid_cols)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~!is.na(.) & trimws(as.character(.)) != "")) |>
      dplyr::transmute(aid_used = rowSums(dplyr::across(dplyr::everything())) > 0) |>
      dplyr::pull()
  } else {
    rep(FALSE, nrow(out))
  }

  # seen flags
  seen_map <- function(x) tolower(trimws(as.character(x))) %in% c("1", "ja", "yes", "true")
  seen_list <- list()

  for (item in crt_items) {
    key_row <- answer_key[answer_key$item == item, ][1, ]
    # build column name
    col_num <- paste0("crt_", sub("^CRT_", "", item), "_num")

    resp <- out[[item]]
    # default NA
    correct <- rep(NA_real_, length(resp))

    # exclude seen
    idx_seen_col <- seen_col_for_item(item)
    seen_vec <- if (idx_seen_col %in% names(out)) seen_map(out[[idx_seen_col]]) else rep(FALSE, length(resp))
    seen_list[[item]] <- seen_vec
    keep <- !seen_vec

    # numeric compare
    acc_val <- unlist(key_row$accept_value)
    acc_lab <- tolower(trimws(unlist(key_row$accept_label)))
    # ad-hoc accepted variants reported in data
    if (item == "CRT_6_4") {
      acc_lab <- unique(c(acc_lab, "emilys"))
    }
    if (item == "CRT_8_4") {
      acc_lab <- unique(c(acc_lab, "das streichholz", "sreichholz", "szreochholz"))
    }

    # coerce responses
    resp_chr <- tolower(trimws(as.character(resp)))
    missing_resp <- resp_chr == "" | is.na(resp_chr)

    is_match <- rep(FALSE, length(resp_chr))
    if (length(acc_val)) {
      is_match <- is_match | (suppressWarnings(as.numeric(resp_chr)) %in% acc_val)
    }
    if (length(acc_lab)) {
      is_match <- is_match | (resp_chr %in% acc_lab)
    }

    correct[keep & !missing_resp] <- ifelse(is_match[keep & !missing_resp], 1, 0)
    correct[keep & missing_resp] <- NA_real_
    correct[!keep] <- NA_real_

    out[[col_num]] <- correct
  }

  crt_cols <- grep("^crt_.*_num$", names(out), value = TRUE)
  # drop all CRT scores if aid used
  if (length(crt_cols)) {
    out[crt_cols][aid_used, ] <- NA_real_
  }

  scored_mat <- as.matrix(out[crt_cols])
  out$crt_items_scored <- rowSums(!is.na(scored_mat))
  out$crt_num <- rowSums(scored_mat, na.rm = TRUE)
  out$crt_av <- ifelse(out$crt_items_scored > 0, out$crt_num / out$crt_items_scored, NA_real_)

  mu <- mean(out$crt_av, na.rm = TRUE)
  sdv <- stats::sd(out$crt_av, na.rm = TRUE)
  out$crt_z <- if (!is.na(sdv) && sdv > 0) (out$crt_av - mu) / sdv else NA_real_

  if (length(seen_list)) {
    seen_mat <- do.call(cbind, seen_list)
    out$crt_seen_count <- rowSums(seen_mat, na.rm = TRUE)
  } else {
    out$crt_seen_count <- 0
  }
  out$crt_aid_used <- aid_used
  out$crt_all_dropped <- out$crt_items_scored == 0
  out
}

#' Score HB items using an external answer key
#'
#' Uses accept_value from the answer key. HB13/HB14 are scored as correct when
#' their numeric responses are equal (outcome bias rule).
#'
#' @param data Tibble with HB item responses.
#' @param answer_key Tibble/json list with fields item, accept_value, accept_label.
#' @return Tibble with hb_<item>_num per-item scores, hb_av, hb_z.
score_hb <- function(data, answer_key) {
  hb_items <- get_items(answer_key, "HB")
  if (length(hb_items) == 0) return(data)
  out <- data

  # hilfsmittel: HB_hilfsmittel_2/3/4 indicate aids; drop all HB items if used
  aid_cols <- hb_aid_cols(out)
  aid_used <- if (length(aid_cols)) {
    out |>
      dplyr::select(dplyr::all_of(aid_cols)) |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~!is.na(.) & trimws(as.character(.)) != "")) |>
      dplyr::transmute(aid_used = rowSums(dplyr::across(dplyr::everything())) > 0) |>
      dplyr::pull()
  } else {
    rep(FALSE, nrow(out))
  }

  for (item in hb_items) {
    col_num <- paste0("hb_", sub("^HB_", "", item), "_num")
    resp <- out[[item]]
    resp_num <- suppressWarnings(as.numeric(as.character(resp)))

    if (item %in% c("HB_13", "HB_14")) {
      # skip here; handled jointly later
      out[[col_num]] <- NA_real_
      next
    }

    key_row <- answer_key[answer_key$item == item, ][1, ]
    acc_val <- unlist(key_row$accept_value)
    correct <- ifelse(resp_num %in% acc_val, 1, 0)
    correct[is.na(resp_num)] <- NA_real_
    out[[col_num]] <- correct
  }

  # Outcome bias pair HB13 & HB14: correct if equal rating
  if (all(c("HB_13", "HB_14") %in% hb_items)) {
    hb13 <- suppressWarnings(as.numeric(as.character(out[["HB_13"]])))
    hb14 <- suppressWarnings(as.numeric(as.character(out[["HB_14"]])))
    pair_correct <- ifelse(!is.na(hb13) & !is.na(hb14) & hb13 == hb14, 1, 0)
    pair_correct[is.na(hb13) | is.na(hb14)] <- NA_real_
    out[["hb_13_num"]] <- pair_correct
    out[["hb_14_num"]] <- pair_correct
  }

  hb_cols <- grep("^hb_.*_num$", names(out), value = TRUE)
  if (length(hb_cols)) {
    out[hb_cols][aid_used, ] <- NA_real_
  }
  scored_mat <- as.matrix(out[hb_cols])
  out$hb_items_scored <- rowSums(!is.na(scored_mat))
  out$hb_num <- rowSums(scored_mat, na.rm = TRUE)
  out$hb_av <- ifelse(out$hb_items_scored > 0, out$hb_num / out$hb_items_scored, NA_real_)

  mu <- mean(out$hb_av, na.rm = TRUE)
  sdv <- stats::sd(out$hb_av, na.rm = TRUE)
  out$hb_z <- if (!is.na(sdv) && sdv > 0) (out$hb_av - mu) / sdv else NA_real_

  out$hb_aid_used <- aid_used
  out$hb_all_dropped <- out$hb_items_scored == 0
  out
}
