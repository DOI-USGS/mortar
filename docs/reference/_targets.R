library(targets)

# set options here like `packages = c("tidyverse",...)
tar_option_set()

#source scripts within 1_fetch, etc. folders
source("REDACTED FILE PATH\jzemmels\AppData\Local\Temp\1\RtmpgDKoDl/1_fetch.R")
source("REDACTED FILE PATH\jzemmels\AppData\Local\Temp\1\RtmpgDKoDl/2_process.R")
source("REDACTED FILE PATH\jzemmels\AppData\Local\Temp\1\RtmpgDKoDl/3_summarize.R")

c(p1_targets_list, p2_targets_list, p3_targets_list)