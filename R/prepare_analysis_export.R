#' Prepare final analysis export
#'
#' Keeps scored/derived columns needed for preregistered analyses and drops
#' raw CTSQ items, attention checks, and unused demographic/timing fields.
#'
#' @param data Tibble after timing averages and drop logic.
#' @return Tibble with a reduced set of columns.
prepare_analysis_export <- function(data) {
  out <- data

  # Drop income/SES raw versions; keep *_z
  drop_ses_raw <- c("income_per_member")

  # Derive demographic z-scores if absent
  if (!"age_z" %in% names(out) && "demo_age" %in% names(out)) {
    age_num <- suppressWarnings(as.numeric(out$demo_age))
    mu <- mean(age_num, na.rm = TRUE)
    sdv <- stats::sd(age_num, na.rm = TRUE)
    out$age_z <- if (!is.na(sdv) && sdv > 0) (age_num - mu) / sdv else NA_real_
  }
  if (!"gender_z" %in% names(out) && "demo_gender" %in% names(out)) {
    gender_num <- suppressWarnings(as.numeric(out$demo_gender))
    mu <- mean(gender_num, na.rm = TRUE)
    sdv <- stats::sd(gender_num, na.rm = TRUE)
    out$gender_z <- if (!is.na(sdv) && sdv > 0) (gender_num - mu) / sdv else NA_real_
  }

  # Create CTSQ-prefixed exports directly from ctsq_* sources; keep CRT/HB aliases
  alias_pairs <- list(
    ctsq_aot_av = "CTSQ_AOT_av",
    ctsq_pit_av = "CTSQ_PIT_av",
    ctsq_cmt_av = "CTSQ_CMT_av",
    ctsq_pet_av = "CTSQ_PET_av",
    ctsq_aot_z = "CTSQ_AOT_z",
    ctsq_pit_z = "CTSQ_PIT_z",
    ctsq_cmt_z = "CTSQ_CMT_z",
    ctsq_pet_z = "CTSQ_PET_z",
    crt_z = "CRT_z",
    hb_z = "HB_z",
    crt_av = "CRT_av",
    hb_av = "HB_av"
  )
  for (src in names(alias_pairs)) {
    tgt <- alias_pairs[[src]]
    if (src %in% names(out) && !tgt %in% names(out)) {
      out[[tgt]] <- out[[src]]
    }
  }

  # Whitelist of columns to retain in the export (drop unprefixed AOT/PIT/CMT/PET)
  keep <- c(
    # Core prereg predictors/outcomes (CTSQ-prefixed only)
    "CTSQ_AOT_av", "CTSQ_PIT_av", "CTSQ_CMT_av", "CTSQ_PET_av",
    "CTSQ_AOT_z", "CTSQ_PIT_z", "CTSQ_CMT_z", "CTSQ_PET_z",
    "CRT_z", "HB_z", "CRT_av", "HB_av",
    "SES", "age_z", "gender_z",
    # QA counters/flags
    "crt_items_scored", "hb_items_scored", "crt_seen_count",
    "crt_aid_used", "hb_aid_used", "crt_all_dropped", "hb_all_dropped",
    "duration_num", "crt_time_av", "hb_time_av",
    # Requested raw/context fields
    "Duration__in_seconds_", "ResponseId", "respondiID",
    # keep all pol_* and demo_* raw fields
    grep("^pol_", names(out), value = TRUE),
    grep("^demo_", names(out), value = TRUE),
    # keep SES components if present
    "income_per_member_z", "edu_z", "profession_z"
  )

  keep <- intersect(keep, names(out))
  out <- out[, keep, drop = FALSE]

  out
}
