################################################################################
#' @description Prepare variables in GR for quality checks
#' @return 
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(haven)
library(tidyverse)
#' Inputs
source("./src/utils.R")
## GR modules
l_gr <- readRDS("./data/dhs/gr.rds")
## Analytical sample
sample <- read.csv("./gen/dhs/output/surveys-ctry-regions.csv")
################################################################################

# keep only surveys in sample (>=2015 and have a fph)
v_samp <- subset(sample, incl_gr == 1)$SurveyId
l_gr_samp <- l_gr[names(l_gr) %in% v_samp]
rm(l_gr)

# Create age at death
l_gr_samp <- lapply(l_gr_samp, fn_gen_aad, module = "GR")

# Create time prior to survey and period variables
l_gr_samp <- lapply(l_gr_samp, fn_gen_tips, module = "GR")

# Create sex of child variable
l_gr_samp <- lapply(l_gr_samp, fn_gen_childsex, module = "GR")

# Add additional variables for fph
l_gr_samp <- lapply(l_gr_samp, fn_gen_fph_var)

# Save output(s) ----------------------------------------------------------

saveRDS(l_gr_samp, "./gen/dhs/temp/gr-prep.rds")

