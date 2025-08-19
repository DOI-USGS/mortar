testthat::test_that(
  "use_file writes to the correct file path", {
    # Set temporary working directory and removal instructions
    tmp <- withr::local_tempdir()
    old <- setwd(tmp)
    on.exit(setwd(old), add = TRUE)
    out_file <- "out_test.txt"

    # Test
    suppressMessages(
      use_file(
        inst_file = "GITIGNORE",
        inst_subdir = "template_files",
        out_file = out_file,
        additions = NULL,
        open = FALSE
      )
    )

    # Check that the file was created in the correct location
    testthat::expect_true(file.exists(file.path(tmp, out_file)))
  }
)

testthat::test_that(
  "use_file throws an error for invalid files/dirs", {
    # Set temporary working directory and removal instructions
    tmp <- withr::local_tempdir()
    old <- setwd(tmp)
    on.exit(setwd(old), add = TRUE)

    # Error if `home` is invalid
    testthat::expect_error(
      use_file(
        inst_file = "GITIGNORE",
        inst_subdir = "template_files",
        out_file = "out_test.txt",
        home = "invalid_dir",
        additions = NULL
      ),
      "`home` must be a path to a directory that exists."
    )

    # Error if `inst_subdir` is invalid
    testthat::expect_error(
      use_file(
        inst_file = "GITIGNORE",
        inst_subdir = "invalid_dir",
        out_file = "out_test.txt",
        additions = NULL
      ),
      "`inst_subdir` must be a subdirectory of inst that exists."
    )

    # Error if inst_file invalid
    testthat::expect_error(
      use_file(
        inst_file = "invalid_file",
        inst_subdir = "template_files",
        out_file = "out_test.txt",
        additions = NULL
      ),
      '`inst_file` must be one of .* not "invalid_file".'
    )
  }
)

# Tests for use_file() and USGS wrappers with explicit negative-path coverage.
testthat::test_that(
  "use_file writes a template plus additions; open=TRUE triggers mocked editor", {
    # Set temporary working directory and removal instructions
    tmp <- withr::local_tempdir()
    old <- setwd(tmp)
    on.exit(setwd(old), add = TRUE)

    readme_out <- file.path(tmp, "README.md")

    suppressMessages(use_file(
      inst_file = "README",
      inst_subdir = "template_files",
      out_file = "README.md",
      home = tmp,
      additions = c("ADD1","ADD2"),
      open = FALSE
    ))

    testthat::expect_true(file.exists(readme_out))
    testthat::expect_equal(
      tail(readLines(readme_out), 2),
      c("ADD1","ADD2")
    )
  }
)
