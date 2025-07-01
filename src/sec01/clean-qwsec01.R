################################################################################
#' @description Clean qwsec01
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
dat <- read.csv("./data/reach_mortalite_femme_qwsec01.csv")
################################################################################

head(dat)

# Select variables of interest (that also aren't in qsecover)
dat <- dat %>%
  select(
    level_1_id,
    q110, q110m, q110y, q111, # Date de naissance (respondent)
    q112, q113, q114, q115, q122, q131, q132 # state of health, schooling level, cell phone, ethnie, marriage
  )

# Save --------------------------------------------------------------------

saveRDS(dat, "./gen/sec01/temp/qwsec01-clean.rds")