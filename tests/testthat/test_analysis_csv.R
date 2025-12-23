test_that("analysis_dataset.csv matches prereg column requirements", {
  csv_path <- file.path("outputs", "analysis_dataset.csv")
  skip_if_not(file.exists(csv_path), "analysis_dataset.csv not generated yet")

  dat <- readr::read_csv(csv_path, show_col_types = FALSE)
  cols <- names(dat)

  required <- c(
    "CTSQ_AOT_av", "CTSQ_PIT_av", "CTSQ_CMT_av", "CTSQ_PET_av",
    "CTSQ_AOT_z", "CTSQ_PIT_z", "CTSQ_CMT_z", "CTSQ_PET_z",
    "CRT_z", "HB_z", "CRT_av", "HB_av",
    "SES", "age_z", "gender_z",
    "demo_gender", "demo_age", "demo_income", "demo_hhmembers"
  )
  expect_true(all(required %in% cols))

  # Duplicates should be gone
  expect_false(any(grepl("^ctsq_", cols)))
  expect_false(any(cols %in% c("AOT_Mean", "AOT_Mean_z", "PIT_Mean", "PIT_Mean_z",
                               "CMT_Mean", "CMT_Mean_z", "PET_Mean", "PET_Mean_z")))

  # Proportions in [0,1] (ignoring NA)
  for (p in c("CRT_av", "HB_av")) {
    vals <- dat[[p]]
    expect_true(all(vals[!is.na(vals)] >= 0 & vals[!is.na(vals)] <= 1))
    expect_false(any(is.infinite(vals)))
  }
})

test_that("analysis_dataset.csv has per-item CRT columns matching answer key", {
  csv_path <- file.path("outputs", "analysis_dataset.csv")
  key_path <- file.path("data", "hb_crt_answer_key.json")
  skip_if_not(file.exists(csv_path), "analysis_dataset.csv not generated yet")
  skip_if_not(file.exists(key_path), "answer key missing")

  key <- jsonlite::read_json(key_path, simplifyVector = TRUE)
  crt_items <- key$item[key$scale == "CRT"]

  dat <- readr::read_csv(csv_path, n_max = 1, show_col_types = FALSE)
  item_cols <- grep("^crt_.*_num$", names(dat), value = TRUE)

  expect_length(item_cols, length(crt_items))
})
