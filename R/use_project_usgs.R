#' Set up a USGS project directory
#'
#' @description Wraps the other \code{use_*_usgs} functions to initialize
#'   multiple files in one directory.
#'
#' @param home chr, root directory of targets project. To use the current
#'   working directory, use  \code{"."}.
#' @param gitignore_additions chr vector, other files/directories to be added to
#'   .gitignore or `NULL` (default) for no additions.
#' @param readme_type chr, either "md" to create README as a markdown file
#'   (README.md; default) or "rmd" to create it as an R markdown file
#'   (README.Rmd). Using an Rmd file will allow you to include R code and output
#'   in your README.md.
#' @param disclaimer_type chr, either "provisional" (default) to include the
#'   provisional software disclaimer statement or "approved" for the approved
#'   disclaimer statement. It is important to only include the approved
#'   disclaimer statement if your software is an approved software release.
#' @param repo_url chr, URL to Git repository. If `NULL` (default) a generic
#'   URL will be provided as a link to the repository in CONTRIBUTING.md.
#'   Otherwise, the issue page for `repo_url` will be used as the link.
#' @param use_mr_template lgl, Should GitLab Merge Request template be included
#'   in project repository. Default is `TRUE`.
#' @param open lgl; whether to open the files for interactive editing.
#'
#' @returns \code{NULL} invisibly
#'
#' @examples
#' tmp <- tempdir()
#' unlink(tmp, recursive = TRUE, force = TRUE)
#' dir.create(tmp)
#'
#' use_project_usgs(
#'   home = tmp,
#'   repo_url = "https://code.usgs.gov/water/computational-tools/mortar"
#' )
#' list.files(tmp)
#'
#' # start over
#' unlink(tmp, recursive = TRUE, force = TRUE)
#' dir.create(tmp)
#'
#' # creates README.Rmd and DISCLAIMER_APPROVED instead
#' use_project_usgs(
#'   home = tmp,
#'   readme_type = "rmd",
#'   disclaimer_type = "approved",
#'   repo_url = "https://code.usgs.gov/water/computational-tools/mortar"
#' )
#' list.files(tmp)
#'
#' # Clean up temp files
#' unlink(tmp, recursive = TRUE, force = TRUE)
#'
#' @export
use_project_usgs <- function(
  home,
  gitignore_additions = NULL,
  readme_type = c("md", "rmd"),
  disclaimer_type = c("provisional", "approved"),
  repo_url = get_usgs_gitlab_url("origin"),
  use_mr_template = TRUE,
  open = rlang::is_interactive()
) {
  disclaimer_type <- rlang::arg_match(disclaimer_type)
  readme_type <- rlang::arg_match(readme_type)
  if (!any(rlang::is_scalar_character(repo_url) | is.null(repo_url))) {
    cli::cli_abort(c(
      "{.arg repo_url} must be a character with length 1 or NULL.",
      x = "You provided a class {.cls {class(repo_url)}} of length {length(repo_url)}."
    ))
  }
  if (!rlang::is_scalar_logical(use_mr_template)) {
    cli::cli_abort(c(
      "{.arg use_mr_template} must be a logical with length 1.",
      x = "You provided a class {.cls {class(use_mr_template)}} of length {length(use_mr_template)}."
    ))
  }
  if (!rlang::is_scalar_logical(open)) {
    cli::cli_abort(c(
      "{.arg open} must be a logical with length 1.",
      x = "You provided a class {.cls {class(open)}} of length {length(open)}."
    ))
  }
  if (!(is.character(gitignore_additions) | is.null(gitignore_additions))) {
    cli::cli_abort(c(
      "x" = "{.arg gitignore_additions} must be a character vector or {.code NULL}, not class {.cls {class(gitignore_additions)}}."
    ))
  }

  # README file
  if (readme_type == "rmd") {
    use_readme_rmd_usgs(home = home, open = open)
  } else {
    use_readme_usgs(home = home, open = open)
  }

  # DISCLAIMER file
  if (disclaimer_type == "approved") {
    use_disclaimer_approved_usgs(home = home, open = open)
  } else {
    use_disclaimer_provisional_usgs(home = home, open = open)
  }

  if (use_mr_template) {
    use_gitlab_mr_template_usgs(home = home, open = open)
  }

  # code.json
  use_code_json_usgs(home = home, open = open)

  # CHANGELOG
  use_changelog_usgs(home = home, open = open)

  # CONTRIBUTING
  use_contributing_usgs(home = home, repo_url = repo_url, open = open)

  # CODE OF CONDUCT
  use_code_of_conduct_usgs(home = home, open = open)

  # .gitignore
  use_gitignore_usgs(
    home = home,
    additions = gitignore_additions,
    open = open
  )
}

#' Add individual USGS project files to a directory
#'
#' @description Creates common project files like .gitignore, README.md. LICENSE.md,
#'   etc.#'
#'
#' @param home chr, root directory of targets project. To use the current
#'   working directory, use  \code{"."}.
#' @param additions chr vector, other files/directories to be added to
#'   .gitignore
#' @param open lgl; whether to open the file for interactive editing
#'
#' @returns \code{NULL} invisibly
#'
#' @examples
#' tmp <- tempdir()
#'
#' use_gitignore_usgs(home = tmp,
#'                    additions = c("excluded_file.R",
#'                                  "excluded_dir",
#'                                  "*excluded_pattern*"))
#' # here are the contents of the .gitignore:
#' cat(readLines(file.path(tmp,".gitignore")), sep = "\n")
#'
#' unlink(tmp, recursive = TRUE, force = TRUE)
#'
#' @name use-file-usgs
#' @rdname use-file-usgs
#' @export
use_gitignore_usgs <- function(
  home,
  additions = NULL,
  open = rlang::is_interactive()
) {
  use_file_usgs(
    home = home,
    inst_file = "GITIGNORE",
    out_file = ".gitignore",
    additions = additions,
    open = open
  )

  return(invisible(NULL))
}

#' @rdname use-file-usgs
#' @export
use_readme_usgs <- function(home, open = rlang::is_interactive()) {
  use_file_usgs(
    home = home,
    inst_file = "README",
    out_file = "README.md",
    additions = NULL,
    open = open
  )

  return(invisible(NULL))
}

#' @rdname use-file-usgs
#' @export
use_readme_rmd_usgs <- function(home, open = rlang::is_interactive()) {
  use_file_usgs(
    home = home,
    inst_file = "README",
    out_file = "README.Rmd",
    additions = NULL,
    open = open
  )

  # add YAML heading as in usethis::use_readme_rmd
  cat(
    c(
      "---",
      "output: github_document",
      "---",
      " ",
      "<!-- README.md is generated from README.Rmd. Please edit that file -->",
      " ",
      readLines(file.path(home, "README.Rmd"))
    ),
    file = file.path(home, "README.Rmd"),
    sep = "\n",
    fill = FALSE
  )

  # add git hook that forces README.md to be updated if README.Rmd is updated.
  repo <- tryCatch(gert::git_find(usethis::proj_get()), error = function(e) {
    NULL
  })
  if (!is.null(repo)) {
    # copied + pasted from usethis:::render_template("readme-rmd-pre-commit.sh")
    readme_rmd_pre_commit_sh <-
      c(
        "#!/bin/bash",
        "README=($(git diff --cached --name-only | grep -Ei '^README\\.[R]?md$'))",
        "MSG=\"use 'git commit --no-verify' to override this check\"",
        "",
        "if [[ ${#README[@]} == 0 ]]; then",
        "  exit 0",
        "fi",
        "",
        "if [[ README.Rmd -nt README.md ]]; then",
        "  echo -e \"README.md is out of date; please re-knit README.Rmd\\n$MSG\"",
        "  exit 1",
        "elif [[ ${#README[@]} -lt 2 ]]; then",
        "  echo -e \"README.Rmd and README.md should be both staged\\n$MSG\"",
        "  exit 1",
        "fi"
      )

    usethis::use_git_hook(
      hook = "pre-commit",
      script = readme_rmd_pre_commit_sh
    )
  }

  return(invisible(NULL))
}

#' @rdname use-file-usgs
#' @export
use_license_usgs <- function(home, open = rlang::is_interactive()) {
  use_file_usgs(
    home = home,
    inst_file = "LICENSE",
    out_file = "LICENSE.md",
    additions = NULL,
    open = open
  )

  return(invisible(NULL))
}

#' @rdname use-file-usgs
#' @export
use_code_json_usgs <- function(home, open = rlang::is_interactive()) {
  use_file_usgs(
    home = home,
    inst_file = "CODE_JSON",
    out_file = "code.json",
    additions = NULL,
    open = open
  )

  return(invisible(NULL))
}

#' @rdname use-file-usgs
#' @export
use_disclaimer_provisional_usgs <- function(
  home,
  open = rlang::is_interactive()
) {
  use_file_usgs(
    home = home,
    inst_file = "DISCLAIMER_PROVISIONAL",
    out_file = "DISCLAIMER_PROVISIONAL.md",
    additions = NULL,
    open = open
  )

  return(invisible(NULL))
}

#' @rdname use-file-usgs
#' @export
use_disclaimer_approved_usgs <- function(
  home,
  open = rlang::is_interactive()
) {
  use_file_usgs(
    home = home,
    inst_file = "DISCLAIMER_APPROVED",
    out_file = "DISCLAIMER_APPROVED.md",
    additions = NULL,
    open = open
  )

  return(invisible(NULL))
}

#' @rdname use-file-usgs
#' @export
use_code_of_conduct_usgs <- function(
  home,
  open = rlang::is_interactive()
) {
  use_file_usgs(
    home = home,
    inst_file = "CODE_OF_CONDUCT",
    out_file = "CODE_OF_CONDUCT.md",
    additions = NULL,
    open = open
  )

  return(invisible(NULL))
}

#' @param repo_url chr, URL to Git repository. If `NULL` (default) a generic
#'   URL will be provided as a link to the repository in CONTRIBUTING.md.
#'   Otherwise, the issue page for `repo_url` will be used as the link.
#' @rdname use-file-usgs
#' @export
use_contributing_usgs <- function(
  home,
  repo_url = NULL,
  open = rlang::is_interactive()
) {
  use_file_usgs(
    home = home,
    inst_file = "CONTRIBUTING",
    out_file = "CONTRIBUTING.md",
    additions = NULL,
    open = FALSE
  )

  # Add repo issue link if applicable
  if (!is.null(repo_url)) {
    file_edit(
      file = file.path(home, "CONTRIBUTING.md"),
      txt = glue::glue("[1]: {repo_url}/-/issues"),
      match = ~ grepl("\\[1]: https://gitlab.com/namespace/repo/-/issues", .x),
      append = FALSE
    )
  }

  # Open file ----
  usethis::edit_file(path = file.path(home, "CONTRIBUTING.md"), open = open)

  return(invisible(NULL))
}

#' @rdname use-file-usgs
#' @export
use_changelog_usgs <- function(home, open = rlang::is_interactive()) {
  use_file_usgs(
    home = home,
    inst_file = "CHANGELOG",
    out_file = "CHANGELOG.md",
    additions = NULL,
    open = open
  )

  return(invisible(NULL))
}

#' @rdname use-file-usgs
#' @export
use_gitlab_mr_template_usgs <- function(home, open = FALSE) {
  dir_path <- file.path(".gitlab", "merge_request_templates")

  if (!dir.exists(file.path(home, dir_path))) {
    dir.create(file.path(home, dir_path), recursive = TRUE)
  }

  use_file(
    home = home,
    inst_file = "MR_TEMPLATE",
    inst_subdir = "template_files",
    out_file = file.path(dir_path, "Default.md"),
    additions = NULL,
    open = open
  )

  return(invisible(NULL))
}

#' Get the HTTPS URL for the git remote of a repo at code.usgs.gov
#'
#' @param remote_name chr; name of the git remote. The default is "origin".
#'
#' @returns a character sting of the HTTPS URL to the remote repo
#' @export
#'
#' @examples
#' \dontrun{
#' # Will fail if working directory is not in a git repo
#' get_usgs_gitlab_url("origin")
#' }
get_usgs_gitlab_url <- function(remote_name = "origin") {
  remote_url <- usethis::git_remotes()

  # Ensure remote name is valid
  if (!remote_name %in% names(remote_url)) {
    cli::cli_abort("{.arg {remote_name}} is not an existing remote.")
  }

  remote_url <- remote_url[[remote_name]]

  # Ensure remote is from code.usgs.gov
  if (!grepl(x = remote_url, pattern = "code.usgs.gov")) {
    cli::cli_abort(c(
      "x" = "The git remote was expecting a remote from {.url https://code.usgs.gov}.",
      "i" = "The remote URL is {.url {remote_url}}"
    ))
  }

  # Convert SSH URL to HTTPS URL
  if (grepl(x = remote_url, pattern = "^git@")) {
    remote_url <- gsub(x = remote_url, pattern = ".git$", replacement = "")
    remote_url <- gsub(
      x = remote_url,
      pattern = "^git@code.usgs.gov:",
      replacement = "https://code.usgs.gov/"
    )
  }

  return(remote_url)
}


#' Internal: core function used in other use_*_usgs functions, which all have
#' the same basic structure
#'
#' @param home chr, root directory of targets project. To use the current
#'   working directory, use  \code{"."}.
#' @param inst_file chr; template file to be included
#' @param out_file chr; path to location of output file
#' @param additions chr; additional lines to add to the template file
#' @param open lgl; whether to open the file for interactive editing
#'
#' @returns NULL, invisibly
#' @noRd
#'
use_file_usgs <- function(
  home,
  inst_file,
  out_file = stringr::str_remove(inst_file, "\\.txt"),
  additions = NULL,
  open = rlang::is_interactive()
) {
  # Write file ----
  use_file(
    home = home,
    inst_file,
    inst_subdir = "template_files",
    out_file = out_file,
    additions = additions,
    open = open
  )

  # Inform user of source ----
  cli::cli_inform(c(
    "i" = "Using {.file {out_file}} from mortar template."
  ))

  return(invisible(NULL))
}
