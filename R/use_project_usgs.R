#' Create a .gitignore with an opinionated list of ignores
#'
#' @param home chr, root directory of project. Defaults to current working
#'   directory
#' @param additions chr vector, other files to be added to .gitignore
#'
#' @export

use_gitignore_usgs <- function(home = ".", additions = NULL){

  cli::cli_inform(c("i" = "Using .gitignore from {.url https://code.usgs.gov/water/IWAAs-trends/templates/standard-template-repository/-/blob/master/.gitignore?ref_type=heads}"))

  git_ignore_path <- file.path(home,".gitignore")

  if(!file.exists(git_ignore_path)) file.create(git_ignore_path)

  cat(c(readLines(system.file("gitignore.txt",package = "mortar")), additions),
      file = git_ignore_path,
      sep = "\n",
      fill = FALSE)

  return(invisible(NULL))

}
