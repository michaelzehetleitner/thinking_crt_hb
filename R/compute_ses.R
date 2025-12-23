#' Compute socioeconomic status (SES)
#'
#' SES = mean of z(income_per_member), z(education), z(profession).
#'
#' @param data Tibble containing demo_income, demo_hhmembers, demo_edu, demo_profession.
#' @return Tibble with added columns income_per_member, income_per_member_z,
#'   edu_z, profession_z, SES, SES_z.
compute_ses <- function(data) {
  out <- data
  income <- suppressWarnings(as.numeric(out$demo_income))
  hh_members <- suppressWarnings(as.numeric(out$demo_hhmembers))

  out$income_per_member <- ifelse(
    !is.na(income) & !is.na(hh_members) & hh_members > 0,
    income / hh_members,
    NA_real_
  )

  zfun <- function(x) {
    mu <- mean(x, na.rm = TRUE)
    sdv <- stats::sd(x, na.rm = TRUE)
    if (is.na(sdv) || sdv == 0) return(rep(NA_real_, length(x)))
    z <- (x - mu) / sdv
    z[is.nan(z)] <- NA_real_
    z
  }

  out$income_per_member_z <- zfun(out$income_per_member)
  out$edu_z <- zfun(suppressWarnings(as.numeric(out$demo_edu)))
  out$profession_z <- zfun(suppressWarnings(as.numeric(out$demo_profession)))

  ses_components <- cbind(out$income_per_member_z, out$edu_z, out$profession_z)
  ses <- rowMeans(ses_components, na.rm = TRUE)
  ses[rowSums(!is.na(ses_components)) == 0] <- NA_real_
  out$SES <- ses
  out$SES_z <- zfun(out$SES)

  out
}
