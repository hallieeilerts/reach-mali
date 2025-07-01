################################################################################
#' @description Clean level 1 with cluster information
#' @return Cleaned qsecover file
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
dat <- read.csv("./data/reach_mortalite_femme_level1.csv")
################################################################################

dat$grappe <- dat$w_grappe
dat$w_grappe <- NULL
dat <- dat[,c("level_1_id", "grappe", "w_enu", "w_cons", "w_men", "w_ind")]

# Save --------------------------------------------------------------------

saveRDS(dat, "./gen/sec01/temp/level1-clean.rds")
