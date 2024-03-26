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
#' list.files(tmp, full.names = FALSE, recursive = TRUE, all.files = TRUE)
#'
#' # clean out tmp folder
#' unlink(tmp, recursive = TRUE, force = TRUE)
#' dir.create(tmp)
#'
#' # different structure starting with 0_config and including "in/" dir in each phase, but all phases will be defined in the _targets.R file rather than in their own .R scripts
#' tar_init(home = tmp,
#'          phase_names = c("config", "pull", "munge", "visualize"),
#'          phase_nums = 0:3,
#'          phase_subdirs = c("in", "src", "out"),
#'          separate_phase_scripts = FALSE)
#'
#' list.files(tmp, full.names = FALSE, recursive = TRUE, all.files = TRUE)
#'
#' @returns \code{NULL} invisibly
#' @export
tar_init <- function(phase_names,
                     phase_nums = seq_along(phase_names),
                     home = ".",
                     separate_phase_scripts = TRUE,
                     phase_subdirs = c("src","out"),
                     overwrite = FALSE){

  # some arg checkers here.
  if(!is.character(phase_names)) cli::cli_abort(c("x" = "{.arg phase_names} is not a character vector"))
  if(any(phase_nums %% 1 != 0)) cli::cli_abort(c("x" = "{.arg phase_nums} is not an integer vector"))
  if(!dir.exists(home)) cli::cli_abort(c("x" = "{.arg home} is not a path to a directory that exists"))
  if(!is.logical(separate_phase_scripts)) cli::cli_abort(c("x" = "{.arg separate_phase_scripts} is not logical"))
  if(!is.character(phase_subdirs)) cli::cli_abort(c("x" = "{.arg phase_subdirs} is not a character vector"))
  if(!is.logical(overwrite)) cli::cli_abort(c("x" = "{.arg overwrite} is not logical"))

  if(length(phase_names) != length(phase_nums)) cli::cli_abort(c("x" = "{.arg phase_nums} is not the same length as {.arg phase_names}"))

  # create "#_phase" R files and directories, adding phase_subdirs and .empty
  # files in each
  purrr::walk2(phase_nums,phase_names,
               function(phaseNum,phaseName){

                 purrr::walk(paste0("/",c("",phase_subdirs)),
                             function(subdir){
                               dir_setup(file.path(home,paste0(phaseNum,"_",phaseName,subdir)),
                                         overwrite = overwrite)

                             })
                 if(separate_phase_scripts & (!file.exists(paste0(home,"/",phaseNum,"_",phaseName,".R")) | overwrite)){
                   file.create(paste0(home,"/",phaseNum,"_",phaseName,".R"))

                   cat(paste0("#source('",home,"/",phaseNum,"_",phaseName,"/src/script.R')\n"),
                       paste0("p",phaseNum,"_targets_list <- list()"),
                       file = paste0(home,"/",phaseNum,"_",phaseName,".R"))
                 }
               })

  if(!file.exists("_targets.R") | overwrite){
    phase_script_text <- ""
    if(separate_phase_scripts) {
      phase_script_text <- glue::glue("source(\"{home}/{phase_nums}_{phase_names}.R\")") |>
        glue::glue_collapse(sep = "\n")
    }
    else{
      phase_script_text <- glue::glue("p{phase_nums}_targets_list <- list()") |>
        glue::glue_collapse(sep = "\n")
    }

    phase_target_text <- "list()"
    # if(separate_phase_scripts) {
      phase_targets <- glue::glue_collapse(glue::glue("p{phase_nums}_targets_list"), sep = ", ")

    # }
    phase_target_text <- glue::glue("c({phase_targets})")

    cat(
      glue::glue(
        "library(targets)

# set options here like `packages = c(\"tidyverse\",...)
tar_option_set()

#source scripts within {phase_nums[1]}_{phase_names[1]}, etc. folders
{phase_script_text}

{phase_target_text}",

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
    dir.create(dir_path)
  }

  file.create(paste0(dir_path,"/.empty"))
}
