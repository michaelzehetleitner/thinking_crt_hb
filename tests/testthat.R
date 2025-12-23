library(testthat)

# Source project functions so tests can run outside a package context
testthat::source_dir("R")

test_dir("tests/testthat")
