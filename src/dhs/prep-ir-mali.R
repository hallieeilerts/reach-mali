################################################################################
#' @description Prep Mali DHS
#' @return 
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(haven)
library(tidyverse)
library(purrr)
#' Inputs
source("./src/utils.R")
## Mali BR modules
l_ir <- readRDS("./gen/dhs/temp/ir-ml.rds")
################################################################################

l_ir_samp <- l_ir[!(names(l_ir) %in% c("ML2015MIS", "ML2021MIS"))]

# Maternal age
l_ir_samp <- map(l_ir_samp, ~ .x %>%
              mutate(
                age_resp = v012,
                agecat_resp = cut(v012, breaks = seq(14, 50, 5),
                                  labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))
              ))


# Save output(s) ----------------------------------------------------------

saveRDS(l_ir, "./gen/dhs/temp/ir-ml-prep.rds")
