test_that("theme_apa returns a ggplot theme", {
  expect_s3_class(theme_apa(), "theme")
})

test_that("plot_crt_hb_performance writes PNG outputs", {
  data <- tibble::tibble(
    crt_1_1_num = c(1, 0, 1, 1),
    crt_2_1_num = c(0, 1, 1, 0),
    hb_1_num = c(1, 1, 0, 1),
    hb_2_num = c(0, 1, 0, 1),
    crt_av = c(0.5, 0.5, 1, 1),
    hb_av = c(0.5, 1, 0.5, 1),
    crt_time_av = c(10, 12, 9, 8),
    hb_time_av = c(15, 14, 13, 12)
  )

  answer_key <- tibble::tibble(
    item = c("CRT_1_1", "CRT_2_1", "HB_1", "HB_2"),
    scale = c("CRT", "CRT", "HB", "HB"),
    text = c("CRT item 1", "CRT item 2", "HB item 1", "HB item 2")
  )

  out_dir <- file.path(tempdir(), "perf_plots")
  paths <- plot_crt_hb_performance(data, answer_key, out_dir)

  expect_length(paths, 4)
  expect_true(all(file.exists(paths)))
})
