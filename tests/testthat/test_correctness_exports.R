test_that("CRT correctness table matches golden master", {
  out_path <- file.path("outputs", "sanity", "crt_item_correctness.csv")
  gold_path <- file.path("tests", "golden_master", "crt_item_correctness.csv")
  skip_if_not(file.exists(out_path), "crt correctness not generated yet")
  skip_if_not(file.exists(gold_path), "golden master missing")

  out <- readr::read_csv(out_path, show_col_types = FALSE)
  gold <- readr::read_csv(gold_path, show_col_types = FALSE)

  expect_equal(out, gold)
})

test_that("HB correctness table matches golden master", {
  out_path <- file.path("outputs", "sanity", "hb_item_correctness.csv")
  gold_path <- file.path("tests", "golden_master", "hb_item_correctness.csv")
  skip_if_not(file.exists(out_path), "hb correctness not generated yet")
  skip_if_not(file.exists(gold_path), "golden master missing")

  out <- readr::read_csv(out_path, show_col_types = FALSE)
  gold <- readr::read_csv(gold_path, show_col_types = FALSE)

  expect_equal(out, gold)
})

test_that("HB outcome-bias pair table matches golden master", {
  out_path <- file.path("outputs", "sanity", "hb_outcome_pair_correctness.csv")
  gold_path <- file.path("tests", "golden_master", "hb_outcome_pair_correctness.csv")
  skip_if_not(file.exists(out_path), "hb outcome pair correctness not generated yet")
  skip_if_not(file.exists(gold_path), "golden master missing")

  out <- readr::read_csv(out_path, show_col_types = FALSE)
  gold <- readr::read_csv(gold_path, show_col_types = FALSE)

  expect_equal(out, gold)
})
