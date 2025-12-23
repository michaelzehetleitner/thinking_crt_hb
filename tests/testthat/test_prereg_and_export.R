test_that("apply_prereg_exclusions drops no-consent and aid users", {
  dat <- tibble::tibble(
    info_consent = c(1, 0, 1),
    CRT_hilfsmittel_2 = c("", "", "used"),
    CTSQ_attention = c("3", "", "3"),
    filler = 1:3
  )

  res <- apply_prereg_exclusions(dat)

  expect_identical(nrow(res), 1L)
  drops <- attr(res, "prereg_drop_counts")
  expect_equal(drops$no_consent, 1)
  expect_equal(drops$aid_used, 1)
  expect_equal(drops$attention_flagged, 1)
  expect_equal(attr(res, "n_before_prereg"), 3)
})

test_that("score_crt drops familiar items before scoring", {
  answer_key <- tibble::tibble(
    item = c("CRT_1", "CRT_2"),
    scale = "CRT",
    accept_value = list(c(1), c(2)),
    accept_label = list(list(), list())
  )

  dat <- tibble::tibble(
    CRT_1 = c("1", "1"),
    CRT_2 = c("2", "0"),
    CRT_gesehen_1 = c("ja", "nein"),
    CRT_gesehen_2 = c("nein", "nein")
  )

  res <- score_crt(dat, answer_key)

  expect_equal(res$crt_items_scored, c(1, 2))
  expect_equal(res$crt_av, c(1, 0.5))
  expect_equal(round(res$crt_z, 3), c(0.707, -0.707))
  expect_true(is.na(res$crt_1_num[1]))
})

test_that("validate_export allows NA but blocks Inf/NaN", {
  ok <- tibble::tibble(
    CTSQ_AOT_z = c(NA, 0.1),
    CTSQ_PIT_z = 0,
    CTSQ_CMT_z = 0,
    CTSQ_PET_z = 0,
    CRT_z = c(NA, -1),
    HB_z = c(0.5, 0.1),
    SES = c(0.2, NA),
    age_z = c(NA, 0),
    gender_z = 0,
    CRT_av = c(0.8, 0.6),
    HB_av = c(0.4, 0.3)
  )
  expect_silent(validate_export(ok))

  bad <- ok
  bad$CRT_z[2] <- Inf
  expect_error(validate_export(bad), "CRT_z")
})

test_that("validate_export blocks proportions outside [0,1]", {
  bad <- tibble::tibble(
    CTSQ_AOT_z = 0,
    CTSQ_PIT_z = 0,
    CTSQ_CMT_z = 0,
    CTSQ_PET_z = 0,
    CRT_z = 0,
    HB_z = 0,
    SES = 0,
    age_z = 0,
    gender_z = 0,
    CRT_av = 1.2,
    HB_av = -0.1
  )
  expect_error(validate_export(bad), "CRT_av")
})

test_that("prepare_analysis_export adds prereg aliases and drops raw CTSQ", {
  dat <- tibble::tibble(
    ctsq_aot_av = c(1, 2),
    ctsq_aot_z = c(-1, 1),
    crt_z = c(0.1, -0.1),
    hb_z = c(0.2, -0.2),
    SES = c(0, 1),
    demo_age = c(20, 30),
    demo_gender = c(1, 2),
    CTSQ_AOT_1 = 1:2
  )

  res <- prepare_analysis_export(dat)

  expect_true(all(c("CTSQ_AOT_z", "CRT_z", "HB_z", "age_z", "gender_z") %in% names(res)))
  expect_false(any(grepl("^CTSQ_.*_\\d+$", names(res))))  # raw item fields removed
})

test_that("score_crt returns per-item numeric columns for all answer-key items", {
  answer_key <- tibble::tibble(
    item = c("CRT_1", "CRT_2", "CRT_3"),
    scale = "CRT",
    accept_value = list(c(1), c(2), c(3)),
    accept_label = list(list(), list(), list())
  )
  dat <- tibble::tibble(
    CRT_1 = c("1"),
    CRT_2 = c("0"),
    CRT_3 = c("3"),
    CRT_gesehen_1 = "nein",
    CRT_gesehen_2 = "nein",
    CRT_gesehen_3 = "nein"
  )

  res <- score_crt(dat, answer_key)
  item_cols <- grep("^crt_.*_num$", names(res), value = TRUE)
  expect_length(item_cols, nrow(answer_key))
  expect_equal(res$crt_items_scored, 3)
})
