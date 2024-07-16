#' Set up a USGS project directory
#'
#' @description Wraps the other \code{use_*_usgs} functions to initialize
#'   multiple files in one directory.
#'
#' @param home chr, root directory of project. Defaults to current working
#'   directory
#' @param gitignore_additions chr vector, other files/directories to be added to
#'   .gitignore
#' @param readme_rmd lgl, should a README.Rmd file be created? If not (default),
#'   then a README.md is created.
#' @param disclaimer_approved lgl, should this project contain an approved
#'   disclaimer statement? If not (default), then a provisional disclaimer
#'   statement is created.
#' @param use_mr_template lgl, should this project contain the file necessary
#'   to have a default GitLab Merge Request template? It is included by default.
#'
#' @examples
#' tmp <- tempdir()
#' unlink(tmp, recursive = TRUE, force = TRUE)
#' dir.create(tmp)
#'
#' use_project_usgs(home = tmp)
#' list.files(tmp)
#'
#' # start over
#' unlink(tmp, recursive = TRUE, force = TRUE)
#' dir.create(tmp)
#'
#' # creates README.Rmd and DISCLAIMER_APPROVED instead
#' use_project_usgs(home = tmp, readme_rmd = TRUE, disclaimer_approved = TRUE)
#' list.files(tmp)
#'
#' @seealso \url{https://code.usgs.gov/water/IWAAs-trends/templates/standard-template-repository/}
#'
#' @export

use_project_usgs <- function(home = ".",
                             gitignore_additions = NULL,
                             readme_rmd = FALSE,
                             disclaimer_approved = FALSE,
                             use_mr_template = TRUE){
  # README file
  if(readme_rmd){
    use_readme_rmd_usgs(home)
  }
  else{
    use_readme_usgs(home)
  }

  # DISCLAIMER file
  if(disclaimer_approved){
    use_disclaimer_approved_usgs(home)
  }
  else{
    use_disclaimer_provisional_usgs(home)
  }

  # CHANGELOG
  use_changelog_usgs(home)

  # CONTRIBUTING
  use_contributing_usgs(home)

  # CODE OF CONDUCT
  use_code_of_conduct_usgs(home)

  # .gitignore
  use_gitignore_usgs(home, gitignore_additions)

  # Gitlab Merge Request template
  if(use_mr_template) {
    use_gitlab_mr_template(home)
  }
}

#' Add individual USGS project files to a directory
#'
#' @description Creates common project files like .gitignore, README.md. LICENSE.md,
#'   etc. based on the templates here:
#'   \url{https://code.usgs.gov/water/IWAAs-trends/templates/standard-template-repository/}.
#'
#'
#' @param home chr, root directory of project. Defaults to current working
#'   directory
#' @param additions chr vector, other files/directories to be added to
#'   .gitignore
#' @examples
#' tmp <- tempdir()
#'
#' use_gitignore_usgs(home = tmp,
#'                    additions = c("excluded_file.R",
#'                                  "excluded_dir",
#'                                  "*excluded_pattern*"))
#'
#' # here are the contents of the .gitignore:
#' cat(readLines(file.path(tmp,".gitignore")), sep = "\n")
#' @name use-file-usgs

#' @rdname use-file-usgs
#' @export
use_gitignore_usgs <- function(home = ".", additions = NULL){

  use_file_usgs(inst_file = "GITIGNORE",
                out_file = ".gitignore",
                home = home,
                additions = additions)

  return(invisible(NULL))

}

#' @rdname use-file-usgs
#' @export
use_readme_usgs <- function(home = "."){

  use_file_usgs(inst_file = "README",
                out_file = "README.md",
                home = home,
                additions = NULL)

  return(invisible(NULL))

}

#' @rdname use-file-usgs
#' @export
use_readme_rmd_usgs <- function(home = "."){

  use_file_usgs(inst_file = "README",
                out_file = "README.Rmd",
                home = home,
                additions = NULL)

  # add YAML heading as in usethis::use_readme_rmd
  cat(c("---",
        "output: github_document",
        "---",
        " ",
        "<!-- README.md is generated from README.Rmd. Please edit that file -->",
        " ",
        readLines(file.path(home, "README.Rmd"))),
      file = file.path(home, "README.Rmd"),
      sep = "\n",
      fill = FALSE)

  # add git hook that forces README.md to be updated if README.Rmd is updated.
  repo <- tryCatch(gert::git_find(usethis::proj_get()), error = function(e) NULL)
  if(!is.null(repo)){
    # copied + pasted from usethis:::render_template("readme-rmd-pre-commit.sh")
    readme_rmd_pre_commit_sh <-
      c("#!/bin/bash",
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
        "fi")

    usethis::use_git_hook(hook = "pre-commit",
                          script = readme_rmd_pre_commit_sh)
  }

  return(invisible(NULL))

}

#' @rdname use-file-usgs
#' @export
use_license_usgs <- function(home = "."){

  use_file_usgs(inst_file = "LICENSE",
                out_file = "LICENSE.md",
                home = home,
                additions = NULL)

  return(invisible(NULL))

}

#' @rdname use-file-usgs
#' @export
use_disclaimer_provisional_usgs <- function(home = "."){

  use_file_usgs(inst_file = "DISCLAIMER_PROVISIONAL",
                out_file = "DISCLAIMER_PROVISIONAL.md",
                home = home,
                additions = NULL)

  return(invisible(NULL))

}

#' @rdname use-file-usgs
#' @export
use_disclaimer_approved_usgs <- function(home = "."){

  use_file_usgs(inst_file = "DISCLAIMER_APPROVED",
                out_file = "DISCLAIMER_APPROVED.md",
                home = home,
                additions = NULL)

  return(invisible(NULL))

}

#' @rdname use-file-usgs
#' @export
use_code_of_conduct_usgs <- function(home = "."){

  use_file_usgs(inst_file = "CODE_OF_CONDUCT",
                out_file = "CODE_OF_CONDUCT.md",
                home = home,
                additions = NULL)

  return(invisible(NULL))

}

#' @rdname use-file-usgs
#' @export
use_contributing_usgs <- function(home = "."){

  use_file_usgs(inst_file = "CONTRIBUTING",
                out_file = "CONTRIBUTING.md",
                home = home,
                additions = NULL)

  return(invisible(NULL))

}

#' @rdname use-file-usgs
#' @export
use_changelog_usgs <- function(home = "."){

  use_file_usgs(inst_file = "CHANGELOG",
                out_file = "CHANGELOG.md",
                home = home,
                additions = NULL)

  return(invisible(NULL))

}

#' @rdname use-file-usgs
#' @export
use_gitlab_mr_template <- function(home = "."){

  if(! dir.exists(".gitlab/merge_request_templates")) {
    dir.create(".gitlab/merge_request_templates", recursive = TRUE)
  }

  use_file_usgs(inst_file = "mr_template",
                out_file = ".gitlab/merge_request_templates/Default.md",
                home = home,
                additions = NULL,
                source_message = FALSE)

  return(invisible(NULL))

}

#' Internal: core function used in other use_*_usgs functions, which all have
#' the same basic structure
#'
#' @param inst_file chr; template file to be included
#' @param out_file chr; path to location of output file
#' @param home chr; root directory of project. Defaults to current working
#'   directory
#' @param additions chr; additional lines to add to the template file
#' @param source_message lgl; should a console message be printed, informing
#'   user that the default files comes from the Trends and Drivers repo. The
#'   default is to include the message.
#'
#' @return NULL, invisibly
#' @noRd
#'
use_file_usgs <- function(inst_file,
                          out_file = stringr::str_remove(inst_file, "\\.txt"),
                          home = ".",
                          additions = NULL,
                          source_message = TRUE){
  # Check arguments ----
  rlang::arg_match(
    inst_file,
    list.files(system.file("template_files", package = "mortar"))
  )

  if(! all(rlang::is_scalar_character(home), dir.exists(home))) {
    cli::cli_abort(c(
      "x" = "{.arg home} must be a path to a directory that exists."
    ))
  }

  if(! rlang::is_scalar_character(out_file)) {
    cli::cli_abort(c(
      "x" = "{.arg out_file} must be a character vector, not class {.cls {class(out_file)}}."
    ))
  }

  if(!(is.character(additions) | is.null(additions))) {
    cli::cli_abort(c(
      "x" = "{.arg additions} must be a character vector or {.code NULL}, not class {.cls {class(additions)}}."
    ))
  }

  if(source_message) {
    cli::cli_inform(c("i" = "Using {.file {out_file}} from {.url https://code.usgs.gov/water/IWAAs-trends/templates/standard-template-repository/}"))
  }

  out_file_path <- file.path(home, out_file)

  if(!file.exists(out_file_path)) file.create(out_file_path)

  cat(
    c(
    readLines(system.file(file.path("template_files",inst_file),
                          package = "mortar")),
    additions),
      file = out_file_path,
      sep = "\n",
      fill = FALSE)

  return(invisible(NULL))
}
