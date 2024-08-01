#' Set up a USGS project directory
#'
#' @description Wraps the other \code{use_*_usgs} functions to initialize
#'   multiple files in one directory.
#'
#' @param home chr, root directory of project. Defaults to current working
#'   directory
#' @param gitignore_additions chr vector, other files/directories to be added to
#'   .gitignore
#' @param readme_rmd lgl, should a README.Rmd file be created? If not
#'   (default), then a README.md is created. Using an Rmd file
#'   will allow you to include R code and output in your README.md.
#' @param disclaimer_approved lgl, should this project contain an approved
#'   disclaimer statement? If not (default), then a provisional disclaimer
#'   statement is created.
#' @param open lgl; whether to open the files for interactive editing
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
#' @seealso
#' \url{https://code.usgs.gov/water/IWAAs-trends/templates/standard-template-repository/}
#'
#' @export

use_project_usgs <- function(home = ".",
                             gitignore_additions = NULL,
                             readme_rmd = FALSE,
                             disclaimer_approved = FALSE,
                             open = rlang::is_interactive()){
  # README file
  if(readme_rmd){
    use_readme_rmd_usgs(home, open = open)
  }
  else{
    use_readme_usgs(home, open = open)
  }

  # DISCLAIMER file
  if(disclaimer_approved){
    use_disclaimer_approved_usgs(home, open = open)
  }
  else{
    use_disclaimer_provisional_usgs(home, open = open)
  }

  # CHANGELOG
  use_changelog_usgs(home, open = open)

  # CONTRIBUTING
  use_contributing_usgs(home, open = open)

  # CODE OF CONDUCT
  use_code_of_conduct_usgs(home, open = open)

  # .gitignore
  use_gitignore_usgs(home, gitignore_additions, open = open)
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
#' @param open lgl; whether to open the file for interactive editing
#'
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
use_gitignore_usgs <- function(home = ".", additions = NULL, open = rlang::is_interactive()){

  use_file_usgs(inst_file = "GITIGNORE",
                out_file = ".gitignore",
                home = home,
                additions = additions,
                open = open)

  return(invisible(NULL))

}

#' @rdname use-file-usgs
#' @export
use_readme_usgs <- function(home = ".", open = rlang::is_interactive()){

  use_file_usgs(inst_file = "README",
                out_file = "README.md",
                home = home,
                additions = NULL,
                open = open)

  return(invisible(NULL))

}

#' @rdname use-file-usgs
#' @export
use_readme_rmd_usgs <- function(home = ".", open = rlang::is_interactive()){

  use_file_usgs(inst_file = "README",
                out_file = "README.Rmd",
                home = home,
                additions = NULL,
                open = open)

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
use_license_usgs <- function(home = ".", open = rlang::is_interactive()){

  use_file_usgs(inst_file = "LICENSE",
                out_file = "LICENSE.md",
                home = home,
                additions = NULL,
                open = open)

  return(invisible(NULL))

}

#' @rdname use-file-usgs
#' @export
use_disclaimer_provisional_usgs <- function(home = ".", open = rlang::is_interactive()){

  use_file_usgs(inst_file = "DISCLAIMER_PROVISIONAL",
                out_file = "DISCLAIMER_PROVISIONAL.md",
                home = home,
                additions = NULL,
                open = open)

  return(invisible(NULL))

}

#' @rdname use-file-usgs
#' @export
use_disclaimer_approved_usgs <- function(home = ".", open = rlang::is_interactive()){

  use_file_usgs(inst_file = "DISCLAIMER_APPROVED",
                out_file = "DISCLAIMER_APPROVED.md",
                home = home,
                additions = NULL,
                open = open)

  return(invisible(NULL))

}

#' @rdname use-file-usgs
#' @export
use_code_of_conduct_usgs <- function(home = ".", open = rlang::is_interactive()){

  use_file_usgs(inst_file = "CODE_OF_CONDUCT",
                out_file = "CODE_OF_CONDUCT.md",
                home = home,
                additions = NULL,
                open = open)

  return(invisible(NULL))

}

#' @rdname use-file-usgs
#' @export
use_contributing_usgs <- function(home = ".", open = rlang::is_interactive()){

  use_file_usgs(inst_file = "CONTRIBUTING",
                out_file = "CONTRIBUTING.md",
                home = home,
                additions = NULL,
                open = open)

  return(invisible(NULL))

}

#' @rdname use-file-usgs
#' @export
use_changelog_usgs <- function(home = ".", open = rlang::is_interactive()){

  use_file_usgs(inst_file = "CHANGELOG",
                out_file = "CHANGELOG.md",
                home = home,
                additions = NULL,
                open = open)

  return(invisible(NULL))

}

#' Internal: core function used in other use_*_usgs functions, which all have
#' the same basic structure
#'
#' @param inst_file chr; template file to be included
#' @param out_file chr; path to location of output file
#' @param home chr, root directory of project. Defaults to current working
#'   directory
#' @param additions chr; additional lines to add to the template file
#' @param open lgl; whether to open the file for interactive editing
#'
#' @return NULL, invisibly
#' @noRd
#'
use_file_usgs <- function(inst_file,
                          out_file = stringr::str_remove(inst_file, "\\.txt"),
                          home = ".",
                          additions = NULL,
                          open = rlang::is_interactive()){

  # Write file ----
  use_file(
    inst_file,
    inst_subdir = "template_files",
    out_file = out_file,
    home = home,
    additions = additions,
    open = open
  )

  # Inform user of source ----
  cli::cli_inform(c(
    "i" = "Using {.file {out_file}} from
    {.url https://code.usgs.gov/water/IWAAs-trends/templates/standard-template-repository/}"
  ))

  return(invisible(NULL))
}
