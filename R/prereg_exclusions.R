#' Apply preregistered participant exclusions
#'
#' Implements consent and self-reported aid-use exclusions using raw Qualtrics
#' columns. Attention-check handling is left as a soft flag until coding is
#' confirmed (see `prereg_flags()`).
#'
#' @param data Tibble containing raw survey responses.
#' @return Filtered tibble with attributes `n_before_prereg` and
#'   `prereg_drop_counts`.
apply_prereg_exclusions <- function(data) {
  n_before <- nrow(data)
  flags <- prereg_flags(data)

  keep <- !flags$exclude_any
  out <- data[keep, , drop = FALSE]

  attr(out, "n_before_prereg") <- n_before
  attr(out, "prereg_drop_counts") <- list(
    no_consent = sum(flags$exclude_consent, na.rm = TRUE),
    aid_used = sum(flags$exclude_aid, na.rm = TRUE),
    attention_flagged = sum(flags$exclude_attention, na.rm = TRUE)
  )
  out
}

#' Derive prereg exclusion flags
#'
#' @param data Tibble with consent, aid, and attention columns.
#' @return List of logical vectors: exclude_consent, exclude_aid, exclude_attention, exclude_any.
prereg_flags <- function(data) {
  df <- tibble::as_tibble(data)
  n <- nrow(df)

  as_yes <- function(x, yes_values) {
    y <- tolower(trimws(as.character(x)))
    num <- suppressWarnings(as.numeric(y))
    yes <- rep(FALSE, length(y))
    yes[!is.na(num) & num %in% yes_values] <- TRUE
    yes[y %in% tolower(as.character(yes_values))] <- TRUE
    yes
  }

  # Consent: info_consent == 1 (or labelled equivalent)
  consent_cols <- intersect(names(df), c("info_consent", "consent"))
  consent_ok <- rep(TRUE, n)
  if (length(consent_cols)) {
    consent_ok <- Reduce(
      `|`,
      lapply(consent_cols, function(col) {
        as_yes(
          df[[col]],
          yes_values = c(1, "1", "yes", "ja", "true", "agree", "i agree", "stimme zu", "stimme eher zu", "stimme eher nicht zu")
        )
      })
    )
  }
  exclude_consent <- !consent_ok

  # Aids: CRT/HB hilfsmittel options 2/3/4 indicate aid use. Option 1 = "No aids".
  aid_cols <- union(crt_aid_cols(df), hb_aid_cols(df))
  exclude_aid <- rep(FALSE, n)
  if (length(aid_cols)) {
    aid_mat <- dplyr::select(df, dplyr::all_of(aid_cols))
    aid_used <- aid_mat |>
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ !is.na(.) & trimws(as.character(.)) != "")) |>
      dplyr::mutate(
        aid_used = dplyr::if_any(
          dplyr::matches("hilfsmittel_[234]$"),
          ~ .x
        )
      ) |>
      dplyr::pull(.data$aid_used)
    exclude_aid <- aid_used
  }

  # Attention: CTSQ_attention exists but coding of the correct option is unknown.
  # We flag rows whose response is missing; rows with any value are retained for now.
  attention_cols <- intersect(names(df), c("CTSQ_attention"))
  exclude_attention <- rep(FALSE, n)
  if (length(attention_cols)) {
    att_vec <- df[[attention_cols[[1]]]]
    exclude_attention <- is.na(att_vec) | trimws(as.character(att_vec)) == ""
  }

  list(
    exclude_consent = exclude_consent,
    exclude_aid = exclude_aid,
    exclude_attention = exclude_attention,
    exclude_any = exclude_consent | exclude_aid | exclude_attention
  )
}
