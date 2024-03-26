#' Create a .gitignore with an opinionated list of ignores
#'
#' @param home chr, root directory of project. Defaults to current working
#'   directory
#' @param additions chr vector, other files to be added to .gitignore
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
