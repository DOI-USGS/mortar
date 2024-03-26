#' Create a .gitignore file with an opinionated list of ignores
#'
#' @description Uses .gitignore file from
#'   \url{https://code.usgs.gov/water/IWAAs-trends/templates/standard-template-repository/}.
#'   Specify project-specific exclusions with the \code{additions} argument,
#'   which are appended to the end of the .gitignore.
#'
#'
#' @param home chr, root directory of project. Defaults to current working
#'   directory
#' @param additions chr vector, other files/directories to be added to
#'   .gitignore
#'
#' @examples
#' tmp <- tempdir()
#' use_gitignore_usgs(home = tmp,
#'                    additions = c("excluded_file.R",
#'                                  "excluded_dir",
#'                                  "*excluded_pattern*"))
#'
#' # here are the contents of the .gitignore:
#' cat(readLines(file.path(tmp,".gitignore")), sep = "\n")
#'
#' @export

use_gitignore_usgs <- function(home = ".", additions = NULL){

  use_file_usgs(inst_file = "gitignore.txt",
                out_file = ".gitignore",
                home = home,
                additions = additions)

  return(invisible(NULL))

}

#' internal function to be used in other use_*_usgs functions, which all have
#' the same basic structure
#' @noRd
use_file_usgs <- function(inst_file,
                          out_file = stringr::str_remove(inst_file, "\\.txt"),
                          home = ".",
                          additions = NULL){
  cli::cli_inform(c("i" = "Using {.file {out_file}} from {.url https://code.usgs.gov/water/IWAAs-trends/templates/standard-template-repository/}"))

  out_file_path <- file.path(home, out_file)

  if(!file.exists(out_file_path)) file.create(out_file_path)

  cat(c(readLines(system.file(inst_file, package = "mortar")), additions),
      file = out_file_path,
      sep = "\n",
      fill = FALSE)

  return(invisible(NULL))
}
