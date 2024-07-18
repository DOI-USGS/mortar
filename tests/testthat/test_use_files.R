
testthat::test_that("use_file writes to the correct file path", {
  # Set up
  temp_dir <- tempdir()
  out_file <- "out_test.txt"

  # Test
  use_file(
    inst_file = "GITIGNORE",
    inst_subdir = "template_files",
    out_file = out_file,
    home = temp_dir,
    additions = NULL
  )

  # Check that the file was created in the correct location
  testthat::expect_true(file.exists(file.path(temp_dir, out_file)))

  # Clean up
  unlink(file.path(temp_dir, out_file))
})

testthat::test_that("use_file throws an error for valid directories", {
  # Set up
  temp_dir <- tempdir()

  # Test
  testthat::expect_error(
    use_file(inst_file = "GITIGNORE",
             inst_subdir = "template_files",
             out_file = "out_test.txt",
             home = "invalid_dir",
             additions = NULL),
    "`home` must be a path to a directory that exists."
  )

  testthat::expect_error(
    use_file(inst_file = "GITIGNORE",
             inst_subdir = "invalid_dir",
             out_file = "out_test.txt",
             home = ".",
             additions = NULL),
    "`inst_subdir` must be a subdirectory of inst that exists."
  )

  testthat::expect_error(
    use_file(inst_file = "invalid_file",
             inst_subdir = "template_files",
             out_file = "out_test.txt",
             home = ".",
             additions = NULL),
    '`inst_file` must be one of .* not "invalid_file".'
  )
})
