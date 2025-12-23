test_that("compute_timings captures Page_Submit columns", {
  dat <- tibble::tibble(
    CRT_1_timing_Page_Submit = c("10", "20"),
    `CRT_2_timing_Page Submit` = c("30", NA),
    HB_1_timing_Page_Submit = c("40", "50"),
    `HB_2_timing_Page Submit` = c(NA, "60"),
    Duration_in_seconds = c("70", "80")
  )

  res <- compute_timings(dat)

  expect_equal(res$duration_num, c(70, 80))
  expect_equal(res$crt_time_av, c(20, 20))
  expect_equal(res$hb_time_av, c(40, 55))
})
