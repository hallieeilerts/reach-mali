################################################################################
#' @description Calculate mortality rates for Mali DHS
#' @return 
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(haven)
library(tidyverse)
library(lubridate)
library(purrr)
library(demogsurv)
#' Inputs
source("./src/utils.R")
## Mali BR modules
l_ir <- readRDS("./gen/dhs/temp/ir-ml-prep.rds")
################################################################################

## Convert to factors (a bit inefficient)
l_ir <- lapply(l_ir, haven::as_factor)

df_nat <- plyr::ldply(l_ir[-1], calc_tfr, by=~1, tips=c(0, 5), .id = "SurveyId")
df_reg <- plyr::ldply(l_ir[-1], calc_tfr, by=~v024, tips=c(0, 5), .id = "SurveyId")

# Save output(s) ----------------------------------------------------------

saveRDS(df_nat, "./gen/dhs/output/mldhs-tfr-nat.rds")
saveRDS(df_reg, "./gen/dhs/output/mldhs-tfr-reg.rds")