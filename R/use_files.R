#' Internal: core function used to use a file from the inst directory
#'
#' @param inst_file chr; path within inst directory to the template file to be
#'   included
#' @param out_file chr; path to location of output file
#' @param home chr, root directory of project. Defaults to current working
#'   directory
#' @param additions chr; additional lines to add to the template file
#'
#' @return NULL, invisibly
#' @noRd
#'
use_file <- function(inst_file,
                     inst_subdir = "",
                     out_file = stringr::str_remove(inst_file, "\\.txt"),
                     home = ".",
                     additions = NULL) {
  # Check arguments ----
  rlang::arg_match(
    inst_file,
    list.files(system.file(inst_subdir, package = "mortar"))
  )

  if (!all(rlang::is_scalar_character(home), dir.exists(home))) {
    cli::cli_abort(c(
      "x" = "{.arg home} must be a path to a directory that exists."
    ))
  }

  if (!rlang::is_scalar_character(out_file)) {
    cli::cli_abort(c(
      "x" = "{.arg out_file} must be a character vector, not class {.cls {class(out_file)}}."
    ))
  }

  if (!(is.character(additions) | is.null(additions))) {
    cli::cli_abort(c(
      "x" = "{.arg additions} must be a character vector or {.code NULL}, not class {.cls {class(additions)}}."
    ))
  }

  # Write file ----
  out_file_path <- file.path(home, out_file)

  if (!file.exists(out_file_path)) file.create(out_file_path)

  inst_file_path <- file.path(inst_subdir, inst_file)

  cat(
    c(
      readLines(system.file(inst_file_path, package = "mortar")),
      additions
    ),
    file = out_file_path,
    sep = "\n",
    fill = FALSE
  )

  return(invisible(NULL))
}
