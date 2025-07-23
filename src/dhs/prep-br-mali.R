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
l_br <- readRDS("./gen/dhs/temp/br-ml.rds")
################################################################################

# Maternal age
l_br <- map(l_br, ~ .x %>%
              mutate(
                age_resp = v012,
                agecat_resp = cut(v012, breaks = seq(14, 50, 5),
                                   labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))
              ))

# SBH variables
l_br <- map(l_br, ~ .x %>%
              rowwise() %>%
              mutate(
                q203_comb = sum(v202, v203, na.rm = TRUE), # sons living at home, daughters living at home
                q205_comb = sum(v204, v205, na.rm = TRUE), # sons living elsewhere, daughters living elsewhere
                q207_comb = sum(v206, v207, na.rm = TRUE) # sons died, daughters died
              ) %>%
              ungroup())

# Add up birth total
l_br <- map(l_br, ~ .x %>%
              rowwise() %>%
              mutate(
                sum_cebcd = sum(q203_comb, q205_comb, q207_comb, na.rm = TRUE)
              ) %>%
              ungroup())
# the variable label says v208 is births in the last five years rather than total children ever born

# In the DHS, we don't have SBH data for when the person had zero children
nrow(subset(l_br$ML2001DHS, v202 == 0))
nrow(subset(l_br$ML2001DHS, v202 == 0 & v203 == 0))
nrow(subset(l_br$ML2001DHS, v202 == 0 & v203 == 0 & v204 == 0 & v205 == 0))
nrow(subset(l_br$ML2001DHS, v202 == 0 & v203 == 0 & v204 == 0 & v205 == 0 & v206 == 0 & v207 == 0)) # 0

# Save output(s) ----------------------------------------------------------

saveRDS(l_br, "./gen/dhs/temp/br-ml-prep.rds")

