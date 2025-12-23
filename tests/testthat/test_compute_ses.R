test_that("compute_ses handles zero/NA household members and avoids NaN", {
  dat <- tibble::tibble(
    demo_income = c("1000", "2000", NA_character_),
    demo_hhmembers = c("0", "4", NA_character_),
    demo_edu = c(1, 2, NA),
    demo_profession = c(1, 3, NA)
  )

  res <- compute_ses(dat)

  expect_true(is.na(res$income_per_member[1]))
  expect_equal(res$income_per_member[2], 500)
  expect_true(is.na(res$SES[3]))
  expect_false(any(is.nan(res$SES)))
})
