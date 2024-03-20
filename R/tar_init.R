#' Initialize a targets project
#'
#' @param home char, root directory of targets project. Defaults to current
#'   working directory
#' @param phase_names char vector, names of target phases
#' @param phase_nums int vector, numbers to prepend to phase_names like
#'   \code{"1_fetch"}, \code{"2_process"}, etc. Defaults to
#'   1:length(phase_names)
#' @param separate_phase_scripts lgl, should a different R script be created for
#'   each phase like "1_fetch.R", "2_process.R", etc? If FALSE, then targets
#'   lists will be initialized in _targets.R file
#' @param phase_subdirs char vector, subdirectories within each phase like
#'   "src", "out"
#' @param overwrite lgl, should the initialization overwrite files and folders
#'   that already exist?
#'
#' @returns \code{NULL} invisibly
tar_init <- function(home = ".",
                     phase_names = c("fetch","process","summarize"),
                     phase_nums = seq_along(phase_names),
                     separate_phase_scripts = TRUE,
                     phase_subdirs = c("src","out"),
                     overwrite = FALSE){

  gitignore_path <- list.files(path = home,
                               pattern = "\\.gitignore",
                               full.names = TRUE,all.files = TRUE)
  if(is.null(gitignore_path)){
    gitignore_path <- file.path(home,".gitignore")
    file.create(gitignore_path)
  }
  gitignore <- unlist(read.table(gitignore_path))

  purrr::walk2(phase_nums,phase_names,
               function(phaseNum,phaseName){

                 purrr::walk(paste0("/",c("",phase_subdirs)),
                             function(subdir){
                               dir_setup(file.path(paste0(phaseNum,"_",phaseName,subdir)),
                                         overwrite = overwrite)

                               gitignore <<- c(gitignore,
                                               paste0("!",phaseNum,"_",phaseName,subdir,"/.empty"))
                             })
                 if(separate_phase_scripts & (!file.exists(paste0(home,"/",phaseNum,"_",phaseName,".R")) | overwrite)){
                   file.create(paste0(home,"/",phaseNum,"_",phaseName,".R"))

                   cat(paste0("#source('",home,"/",phaseNum,"_",phaseName,"/src/script.R')\n"),
                       paste0("p",phaseNum,"_targets_list <- list()"),
                       file = paste0(home,"/",phaseNum,"_",phaseName,".R"))
                 }
               })

  if(!file.exists("_targets.R") | overwrite){
    cat(paste0(
      'library(targets)
#source scripts within ',phase_nums[1],'_',phase_names[1],', etc. folders
#scripts <- list.files(\"',home,'\",recursive = TRUE,full.names = TRUE,pattern = "\\\\.R$")
#purrr::walk(scripts[stringr::str_detect(scripts,"[0-9]{1}_")],source)
',
      ifelse(separate_phase_scripts,
             paste0(purrr::map2_chr(phase_nums,phase_names,~ paste0("source('",home,"/",.x,"_",.y,".R')")),collapse = "\n"),
             ""),'

# set options here like `packages = c("tidyverse",...)`
tar_option_set()

',
      ifelse(!separate_phase_scripts,
             paste0(purrr::map_chr(phase_nums,~ paste0("p",.x,"_targets_list <- list()")),collapse = "\n"),
             ""),'
list(',paste0(purrr::map_chr(phase_nums,~ paste0("p",.x,"_targets_list")),collapse = ", "),')'),
      file = file.path(home,"_targets.R"))
  }

  cat(paste0(
    paste(
      unique(c(gitignore,
               "*/out/*",
               "_targets")),
      sep = "\n",collapse = "\n"),
    "\n"),
    file = gitignore_path)

  message("Start editing _targets.R file.")
  rstudioapi::documentOpen(file.path(home,"_targets.R"))
  # targets::tar_make()

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
