library(targets)
#source scripts within 1_fetch, etc. folders
#scripts <- list.files("REDACTED FILE PATH\AJMART~1\AppData\Local\Temp\1\RtmpKY6qjf",recursive = TRUE,full.names = TRUE,pattern = "\\.R$")
#purrr::walk(scripts[stringr::str_detect(scripts,"[0-9]{1}_")],source)

source("REDACTED FILE PATH\AJMART~1\AppData\Local\Temp\1\RtmpKY6qjf/1_fetch.R")
source("REDACTED FILE PATH\AJMART~1\AppData\Local\Temp\1\RtmpKY6qjf/2_process.R")
source("REDACTED FILE PATH\AJMART~1\AppData\Local\Temp\1\RtmpKY6qjf/3_summarize.R")

# set options here like `packages = c("tidyverse",...)
tar_option_set()


c(p1_targets_list, p2_targets_list, p3_targets_list)