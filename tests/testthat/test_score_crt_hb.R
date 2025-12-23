test_that("score_crt respects aid flags and sets crt_all_dropped", {
  answer_key <- tibble::tibble(
    item = c("CRT_1"),
    scale = c("CRT"),
    accept_value = list(c(1)),
    accept_label = list(list())
  )
  dat <- tibble::tibble(
    CRT_1 = c("1", "0"),
    CRT_hilfsmittel_2 = c("", "used")
  )

  res <- score_crt(dat, answer_key)

  expect_equal(res$crt_av, c(1, NA))
  expect_equal(res$crt_all_dropped, c(FALSE, TRUE))
})

test_that("score_hb uses outcome-bias pair logic", {
  answer_key <- tibble::tibble(
    item = c("HB_1", "HB_13", "HB_14"),
    scale = c("HB", "HB", "HB"),
    accept_value = list(c(1), list(), list()),
    accept_label = list(list(), list(), list())
  )
  dat <- tibble::tibble(
    HB_1 = c("1", "0"),
    HB_13 = c(4, 3),
    HB_14 = c(4, 2)
  )

  res <- score_hb(dat, answer_key)

  expect_equal(res$hb_13_num, c(1, 0))
  expect_equal(res$hb_14_num, c(1, 0))
  expect_equal(res$hb_av, c(1, 0))
})
