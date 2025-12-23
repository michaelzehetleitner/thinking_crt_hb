test_that("score_ctsq derives subscales and uses labelled values", {
  likert_labels <- c("no" = 1, "maybe" = 2, "yes" = 3)
  item1 <- haven::labelled(c(1, 2), labels = likert_labels)
  item2 <- haven::labelled(c(2, 3), labels = likert_labels)

  dat <- tibble::tibble(
    CTSQ_AOT_1 = item1,
    CTSQ_CMT_7 = item2
  )

  res <- score_ctsq(dat)

  expect_equal(res$ctsq_aot_1_num, c(1, 2))
  expect_equal(res$ctsq_cmt_7_num, c(2, 3))
  expect_equal(res$ctsq_aot_av, c(1, 2))
  expect_equal(res$ctsq_cmt_av, c(2, 3))
  expect_equal(unname(round(res$ctsq_aot_z, 3)), c(-0.707, 0.707), check.attributes = FALSE)
})

test_that("score_ctsq falls back to numeric coercion when labels absent", {
  dat <- tibble::tibble(
    CTSQ_PIT_13 = c("1", "3", NA),
    CTSQ_PIT_14 = c("2", "3", "4")
  )

  res <- score_ctsq(dat)

  expect_equal(res$ctsq_pit_13_num, c(1, 3, NA))
  expect_equal(res$ctsq_pit_14_num, c(2, 3, 4))
  expect_equal(res$ctsq_pit_av, c(1.5, 3, 4))
})

test_that("score_ctsq preserves scale attributes on z-scores", {
  dat <- tibble::tibble(
    CTSQ_AOT_1 = 1:3,
    CTSQ_AOT_2 = 1:3
  )

  res <- score_ctsq(dat)

  expect_equal(res$ctsq_aot_av, c(1, 2, 3))
  expect_equal(unname(res$ctsq_aot_z), c(-1, 0, 1), check.attributes = FALSE)
  expect_equal(attr(res$ctsq_aot_z, "scaled:center"), 2)
  expect_equal(attr(res$ctsq_aot_z, "scaled:scale"), 1)
})
