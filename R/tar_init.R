#' Initialize a targets project
#'
#' @description Run this function in a new targets project directory. It will
#'   create R files and directories using the targets project structure we often
#'   use in the IIDD Data Science Branch (see
#'   \url{https://wma.code-pages.usgs.gov/dsp/trainings/ds-pipelines-targets-2-course/12-usgs-data-science-conventions.html}
#'   for more information on our naming conventions and
#'   \url{https://code.usgs.gov/wma/dsp/trainings/ds-pipelines-targets-2-course}
#'   for an example targets project).
#'
#' @param phase_names chr vector, names of target phases like "fetch",
#'   "process", etc.
#' @param phase_nums chr or int vector, numbers to prepend to phase_names like
#'   \code{"1_fetch"}, \code{"2_process"}, etc. Defaults to
#'   1:length(phase_names)
#' @param home chr, root directory of targets project. Defaults to current
#'   working directory "."
#' @param separate_phase_scripts lgl, should a different R script be created for
#'   each phase like "1_fetch.R", "2_process.R", etc? If FALSE, then targets
#'   lists will be initialized in _targets.R file. Defaults to TRUE
#' @param phase_subdirs chr vector, subdirectories within each phase. Defaults
#'   to "src", "out"
#' @param overwrite lgl, should the initialization overwrite files and folders
#'   that already exist? Defaults to FALSE
#'
#' @examples
#' # temporary directories in which targets project is initialized (you can skip this part if creating your own project)
#' tmp <- tempdir()
#' unlink(tmp, recursive = TRUE, force = TRUE)
#' dir.create(tmp)
#'
#' # creates 1_fetch, 2_process, 3_summarize R scripts and directories
#' tar_init(home = tmp, phase_names = c("fetch", "process", "summarize"))
#'
#' list.files(tmp, full.names = FALSE, recursive = TRUE, all.files = TRUE, pattern = "\\.(R|empty)$")
#'
#' # clean out tmp folder
#' unlink(tmp, recursive = TRUE, force = TRUE)
#' dir.create(tmp)
#'
#' # different structure starting with 0_config and including "in/" dir in each phase
#' tar_init(home = tmp,
#'          phase_names = c("config", "pull", "munge", "visualize"),
#'          phase_nums = 0:3,
#'          phase_subdirs = c("in", "src", "out"))
#'
#' list.files(tmp, full.names = FALSE, recursive = TRUE, all.files = TRUE, pattern = "\\.(R|empty)$")
#'
#' @returns \code{NULL} invisibly
#' @export
tar_init <- function(phase_names,
                     phase_nums = seq_along(phase_names),
                     home = ".",
                     separate_phase_scripts = TRUE,
                     phase_subdirs = c("src","out"),
                     overwrite = FALSE){

  # Check arguments ----
  if(!is.character(phase_names)) {
    cli::cli_abort(c(
      "x" = "{.arg phase_names} must be a character vector, not class {.cls {class(phase_names)}}."
    ))
  }

  if(any(phase_nums %% 1 != 0)) {
    cli::cli_abort(c(
      "x" = "{.arg phase_nums} must be an integer vector, not class {.cls {class(phase_nums)}}."
    ))
  }

  if(!dir.exists(home)) {
    cli::cli_abort(c(
      "x" = "{.arg home} must be a path to a directory that exists."
    ))
  }

  if(!is.logical(separate_phase_scripts)) {
    cli::cli_abort(c(
      "x" = "{.arg separate_phase_scripts} must be logical, not class {.cls {class(separate_phase_scripts)}}."
    ))
  }

  if(!is.character(phase_subdirs)) {
    cli::cli_abort(c(
      "x" = "{.arg phase_subdirs} must be a character vector not class {.cls {class(phase_subdirs)}}."
    ))
  }

  if(!is.logical(overwrite)) {
    cli::cli_abort(c(
      "x" = "{.arg overwrite} must be logical, not class {.cls {class(overwrite)}}."
    ))
  }

  if(length(phase_names) != length(phase_nums)) {
    cli::cli_abort(c(
      "x" = "{.arg phase_nums} must be the same length as {.arg phase_names}.",
      "!" = "{.arg phase_nums} has a length of {length(phase_nums)} and {.arg phase_names} has a length of {length(phase_names)}."
    ))
  }

  # Create phase R files and directories ----
  # create "#_phase" R files and directories, adding phase_subdirs and .empty
  # files in each

  ## Create subdirectories ----
  subdir_paths <- expand.grid(
    dir = glue::glue("{home}/{phase_nums}_{phase_names}"),
    subdir = phase_subdirs
  ) |>
    glue::glue_data("{dir}/{subdir}")

  purrr::walk(subdir_paths, ~ dir_setup(.x, overwrite = overwrite))

  ## Create phase scripts (if applicable) ----
  phase_files <- glue::glue("{home}/{phase_nums}_{phase_names}.R")
  if(separate_phase_scripts & (!file.exists(phase_files) | overwrite)) {
    phase_file_text <- glue::glue(
      "#source(\"{home}/{phase_nums}_{phase_names}/src/script.R\")\n",
      " p{phase_nums}_targets_list <- list()"
    )
    purrr::walk2(phase_file_text, phase_files, ~cat(.x, file = .y))
  }

  # Create targets file ----
  if(!file.exists("_targets.R") | overwrite){
    if(separate_phase_scripts) {
      phase_script_text <- glue::glue("source(\"{home}/{phase_nums}_{phase_names}.R\")") |>
        glue::glue_collapse(sep = "\n")
    } else {
      phase_script_text <- ""
    }

    if(separate_phase_scripts) {
      phase_targets <- glue::glue_collapse(glue::glue("p{phase_nums}_targets_list"), sep = ", ")
      phase_target_text <- glue::glue("c({phase_targets})")
    } else {
      phase_target_text <- "list()"
    }


    cat(
      glue::glue(
        "library(targets)",
        "#source scripts within {phase_nums[1]}_{phase_names[1]}, etc. folders",
        "#scripts <- list.files(\"{home}\",recursive = TRUE,full.names = TRUE,pattern = \"\\\\.R$\")",
        "#purrr::walk(scripts[stringr::str_detect(scripts,\"[0-9]{{1}}_\")],source)\n",
        "{phase_script_text}\n",
        "# set options here like `packages = c(\"tidyverse\",...)",
        "tar_option_set()\n\n",
        "{phase_target_text}",
        .sep = "\n"
      ),
      file = "_targets.R"
    )
  }

  return(invisible(NULL))

}

# helper function that creates a directory if it doesn't exist and writes a
# .empty to file in it
dir_setup <- function(dir_path,overwrite){
  if(!dir.exists(dir_path) | overwrite){
    unlink(dir_path,recursive = TRUE)
    dir.create(dir_path, recursive = TRUE)
  }

  file.create(paste0(dir_path,"/.empty"))
}
