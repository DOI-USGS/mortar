testthat::test_that("Errors thrown in `tar_init()`", {
  testthat::expect_error(
    tar_init(c("a", "b", "c"), phase_nums = 1:2),
    "`phase_nums` must be the same length as `phase_names`"
  )
  testthat::expect_error(
    tar_init(phase_names = "a", separate_phase_scripts = c(TRUE, FALSE)),
    "`separate_phase_scripts` must be logical"
  )
  testthat::expect_error(
    tar_init(phase_names = "a", overwrite = c(TRUE, FALSE)),
    "`overwrite` must be logical"
  )
})

# These tests result in the creation of a _targets.R file, that I don't think
#  we want

# testthat::test_that("Errors not thrown in `tar_init()`", {
#   testthat::expect_no_error(
#     tar_init(c("a", "b"), phase_nums = c("01", "02"), home = tempdir())
#   )
#   testthat::expect_no_error(
#     tar_init(c("a", "b"), phase_nums = 1:2, home = tempdir())
#   )
# })

