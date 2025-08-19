# Simple tests to make sure that errors work as expected.
testthat::test_that(
  "Errors thrown in `tar_init()`", {
  # Set temporary working directory and removal instructions
  tmp <- withr::local_tempdir()
  old <- setwd(tmp)
  on.exit(setwd(old), add = TRUE)

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
  testthat::expect_error(
    tar_init(phase_names = "a", use_leading_zeros = "TRUE"),
    "`use_leading_zeros` must be logical"
  )
  testthat::expect_error(
    tar_init(phase_names = "a", phase_subdirs = TRUE),
    "`phase_subdirs` must be a character vector"
  )
  testthat::expect_no_error(
    tar_init(c("a", "b"), phase_nums = 1:2)
  )
})

# Tests for tar_init() with deeper verification of _targets.R content.
# We verify:
#  - argument validation (basic smoke checks)
#  - directory creation for phases/subdirs
#  - exact/ordered content expectations inside _targets.R for both modes:
#       * separate_phase_scripts = TRUE
#       * separate_phase_scripts = FALSE
testthat::test_that(
  "tar_init creates expected structure with separate_phase_scripts = TRUE", {
  # Set temporary working directory and removal instructions
  tmp <- withr::local_tempdir()
  old <- setwd(tmp)
  on.exit(setwd(old), add = TRUE)

  tar_init(
    phase_names = c("fetch", "process"),
    phase_nums = c(1, 2),
    separate_phase_scripts = TRUE,
    phase_subdirs = c("src", "out"),
    use_leading_zeros = FALSE,
    overwrite = TRUE
  )

  # Directories exist
  testthat::expect_true(dir.exists("1_fetch/src"))
  testthat::expect_true(dir.exists("1_fetch/out"))
  testthat::expect_true(dir.exists("2_process/src"))
  testthat::expect_true(dir.exists("2_process/out"))

  # Phase scripts exist
  testthat::expect_true(file.exists("1_fetch.R"))
  testthat::expect_true(file.exists("2_process.R"))

  # _targets.R content checks
  testthat::expect_true(file.exists("_targets.R"))
  txt <- readLines("_targets.R")

  # Ensure parts of targets script file are in correct order
  idx_lib <- grep("^library\\(targets)", txt)
  idx_opts <- grep("^# Set options", txt)
  idx_phase <- grep("^# Load target list files", txt)
  idx_def <- grep("^# Define pipeline", txt)

  testthat::expect_true(all(c(idx_lib, idx_opts, idx_phase, idx_def) > 0))
  testthat::expect_lt(idx_lib, idx_opts)
  testthat::expect_lt(idx_opts, idx_phase)
  testthat::expect_lt(idx_phase, idx_def)

  # tar_source line references the individual phase files (quoted, comma-separated)
  line_after_phase <- txt[idx_phase + 1]
  testthat::expect_match(
    line_after_phase,
    'tar_source\\(c\\("1_fetch.R\", \\"2_process.R\\"))'
  )

  # pipeline definition aggregates the phase list objects
  testthat::expect_true(
    any(grepl("^c\\(p1_targets_list, p2_targets_list)$", txt))
  )
})

testthat::test_that(
  "tar_init in non-phase mode (separate_phase_scripts = FALSE) uses list()", {
  tmp <- withr::local_tempdir()
  old <- setwd(tmp)
  on.exit(setwd(old), add = TRUE)

  tar_init(
    phase_names = c("config", "run"),
    phase_nums = c(0, 1),
    separate_phase_scripts = FALSE,
    phase_subdirs = c("in", "src", "out"),
    use_leading_zeros = TRUE,
    overwrite = TRUE
  )

  # Ensure proper directories and subdirectories exist
  testthat::expect_true(dir.exists("00_config/in"))
  testthat::expect_true(dir.exists("01_run/out"))
  testthat::expect_true(file.exists("_targets.R"))

  txt <- readLines("_targets.R")
  # When separate_phase_scripts = FALSE, the pipeline definition should be 'list()'
  testthat::expect_true(any(grepl("^list\\()$", txt)))
  # And the 'tar_source' helper should be commented with the project home
  testthat::expect_true(any(
    grepl('^# tar_source\\(\\"\\")$', txt) | grepl('^# tar_source\\(".*/?")$', txt)
  ))
})
