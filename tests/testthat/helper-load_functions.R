# Ensure project functions are available in tests when running `testthat::test_dir()`
# Work relative to tests/testthat (testthat sets wd accordingly).
root <- normalizePath(file.path("..", ".."), mustWork = TRUE)
r_dir <- file.path(root, "R")
files <- list.files(r_dir, pattern = "\\.[Rr]$", full.names = TRUE)
env <- environment()  # helper environment that test files inherit from
for (f in files) {
  sys.source(f, envir = env, chdir = TRUE)
}
