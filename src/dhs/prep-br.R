################################################################################
#' @description Prepare variables in BR for quality checks
#' @return 
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(haven)
library(tidyverse)
#' Inputs
source("./src/utils.R")
## BR modules
l_br <- readRDS("./data/dhs/br.rds")
## Analytical sample
sample <- read.csv("./gen/dhs/output/surveys-ctry-regions.csv")
################################################################################

# keep only surveys in sample (>=2015 and don't have a fph)
v_samp <- subset(sample, incl_br == 1)$SurveyId
l_br_samp <- l_br[names(l_br) %in% v_samp]
rm(l_br)

# Create age at death
l_br_samp <- lapply(l_br_samp, fn_gen_aad)

# Create time prior to survey and period variables
l_br_samp <- lapply(l_br_samp, fn_gen_tips)

# Create sex of child variable
l_br_samp <- lapply(l_br_samp, fn_gen_childsex)

# Save output(s) ----------------------------------------------------------

saveRDS(l_br_samp, "./gen/dhs/temp/br-prep.rds")

