testthat::test_that(
  "use_project_usgs() validates its arguments", {
    # Invalid readme_type should result in an error mentioning the argument
    testthat::expect_error(
      use_project_usgs(readme_type = "txt"),
      '`readme_type` must be one of "md" or "rmd", not "txt"'
    )
    # Invalid disclaimer_type should result in an error mentioning the argument
    testthat::expect_error(
      use_project_usgs(disclaimer_type = "final"),
      '`disclaimer_type` must be one of "provisional" or "approved", not "final"'
    )
  }
)

testthat::test_that(
  "use_project_usgs() creates expected files when templates exist", {
    # Skip this test entirely if the template directory isn't found.  Without
    # these files, the helper functions that write files will error because
    # system.file() can't locate the templates.
    template_dir <- system.file("template_files", package = "mortar")
    testthat::skip_if_not(dir.exists(template_dir))

    # Ensure at least one required template is present; otherwise there is
    # nothing to copy into the project directory.  README is the most
    # fundamental template so we check for it.
    testthat::skip_if_not(file.exists(file.path(template_dir, "README")))

    # Set temporary working directory and removal instructions
    tmp <- withr::local_tempdir()
    old <- setwd(tmp)
    on.exit(setwd(old), add = TRUE)

    # Run the function.  We specify open = FALSE to avoid launching an
    # editor during the test.  The `repo_url` argument is provided so that
    # CONTRIBUTING.md contains a link to the issues page.  Passing
    # `gitignore_additions` allows us to verify that our custom entries are
    # written to .gitignore.
    testthat::expect_no_error(
      suppressMessages(use_project_usgs(
        home = tmp,
        gitignore_additions = c("myfile.txt"),
        readme_type = "md",
        disclaimer_type = "provisional",
        repo_url = "https://code.usgs.gov/test/repo",
        open = FALSE
      ))
    )

    # After execution, the expected files should exist in the project directory.
    expected_files <- c(
      "README.md",
      "DISCLAIMER_PROVISIONAL.md",
      "code.json",
      "CHANGELOG.md",
      "CONTRIBUTING.md",
      "CODE_OF_CONDUCT.md",
      ".gitignore"
    )
    # Make sure each of these files exists
    for (f in expected_files) {
      testthat::expect_true(file.exists(file.path(tmp, f)))
    }
    # The .gitignore file should contain the custom addition we supplied.
    gitignore_lines <- readLines(file.path(tmp, ".gitignore"))
    testthat::expect_true(any(grepl("myfile.txt", gitignore_lines)))
  }
)

testthat::test_that(
  "use_project_usgs() with R Markdown README and approved disclaimer", {
    # Skip if the necessary templates aren't available.
    template_dir <- system.file("template_files", package = "mortar")
    testthat::skip_if_not(dir.exists(template_dir))
    testthat::skip_if_not(file.exists(file.path(template_dir, "README")))

    # Set temporary working directory and removal instructions
    tmp <- withr::local_tempdir()

    # Create project files
    suppressMessages(testthat::expect_no_error(
      use_project_usgs(
        home = tmp,
        gitignore_additions = NULL,
        readme_type = "rmd",
        disclaimer_type = "approved",
        repo_url = "https://code.usgs.gov/test/repo",
        open = FALSE
      )
    ))

    # Check that README.Rmd and the approved disclaimer exist.
    testthat::expect_true(file.exists(file.path(tmp, "README.Rmd")))
    testthat::expect_true(file.exists(file.path(tmp, "DISCLAIMER_APPROVED.md")))

    # The README.Rmd file should start with a YAML header that sets
    # `output: github_document`.  We only look at the first few lines to
    # confirm this; additional content comes from the template.
    rmd_lines <- readLines(file.path(tmp, "README.Rmd"))
    testthat::expect_true(length(rmd_lines) >= 3)

    # Check that YAML header starts with ---
    testthat::expect_identical(rmd_lines[1], "---")
    # The second line should specify output format.  We use grepl() because
    # whitespace may vary. YAML header specifies github_document output
    testthat::expect_true(grepl("output: github_document", rmd_lines[2]))
  }
)

testthat::test_that(
  "use_contributing_usgs() inserts and omits issue links correctly", {
    # Skip if contributing template doesn't exist.
    template_dir <- system.file("template_files", package = "mortar")
    testthat::skip_if_not(dir.exists(template_dir))
    testthat::skip_if_not(file.exists(file.path(template_dir, "CONTRIBUTING")))

    # Test with repo_url supplied: link to issues should be added
    tmp1 <- withr::local_tempdir()
    old <- setwd(tmp1)
    on.exit(setwd(old), add = TRUE)

    # Use open = FALSE to prevent opening editor
    suppressMessages(testthat::expect_no_error(
      use_contributing_usgs(
        home = tmp1,
        repo_url = "https://code.usgs.gov/wma/example",
        open = FALSE
      )
    ))
    contrib_path1 <- file.path(tmp1, "CONTRIBUTING.md")
    testthat::expect_true(file.exists(contrib_path1)) # repo_url in CONTRIBUTING.md
    lines1 <- readLines(contrib_path1)
    # The inserted link should appear somewhere in the file; we won't rely on
    # its exact position because line numbers may change if the template
    # evolves.
    testthat::expect_true(
      any(grepl("https://code.usgs.gov/wma/example/-/issues", lines1))
    )

    # Test without repo_url: no link should be inserted - retain generic link
    tmp2 <- withr::local_tempdir()
    suppressMessages(testthat::expect_no_error(
      use_contributing_usgs(home = tmp2, repo_url = NULL, open = FALSE)
    ))
    contrib_path2 <- file.path(tmp2, "CONTRIBUTING.md")
    testthat::expect_true(file.exists(contrib_path2))
    lines2 <- readLines(contrib_path2)
    testthat::expect_true(
      any(grepl("https://gitlab.com/namespace/repo/-/issues", lines2))
    )
  })

testthat::test_that(
  "use_file_usgs() errors when the template file is not found", {
    # use_file_usgs() should fail if inst_file is not in the template directory.
    # This test uses a temporary directory as home; since the error arises
    # before any file is created, there's no need to clean up.
    tmp <- withr::local_tempdir()
    old <- setwd(tmp)
    on.exit(setwd(old), add = TRUE)

    testthat::expect_error(
      use_file_usgs(
        inst_file = "NON_EXISTENT_TEMPLATE",
        out_file = "dummy.txt",
        home = tmp,
        open = FALSE
      ),
      "`inst_file` must be one of .*"
    )
  }
)

testthat::test_that(
  "get_usgs_gitlab_url() fails gracefully for unknown remotes", {
    # Skip this test if usethis is not available.  get_usgs_gitlab_url() uses
    # usethis::git_remotes() to retrieve remote names.  If usethis isn't
    # installed or if not using a git repo, we can't exercise this behavior.
    testthat::skip_if_not_installed("usethis")
    testthat::skip_if_not(tryCatch(gert::git_find(), error = \(e) FALSE))
    # Provide a remote name that is extremely unlikely to exist.  We don't
    # assert on the error message itself because it may change; it's
    # sufficient to know that the function throws an error.
    testthat::expect_error(
      get_usgs_gitlab_url("INVALID_Ample_Breakfast_Earthenware"),
      "`INVALID_Ample_Breakfast_Earthenware` is not an existing remote."
    )
  }
)
