################################################################################
#' @description Merge fph to intdate
#' @return None
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)
library(haven)
#' Inputs
ebm <- readRDS("./gen/combined/temp/w_ebm-clean.rds")
dat <- readRDS("./gen/combined/temp/fph-clean.rds")
################################################################################

nrow(dat) # 79976

nrow(ebm)
ebm %>%
  group_by(w_grappe, w_men, w_ind) %>%
  mutate(n = n()) %>%
  filter(n >1)
df_ebm <- ebm %>% select(w_grappe, w_enu, w_cons, w_men, w_ind, qintdate)
nrow(subset(df_ebm, is.na(qintdate))) # 51

# merge fph to visit dates
dat <- merge(dat, df_ebm, by = c("w_grappe", "w_enu", "w_cons", "w_men", "w_ind"), all.x = TRUE)

# some are not in df_ebm and don't have an intdate
nrow(subset(dat, is.na(qintdate))) # 639

# Save --------------------------------------------------------------------

saveRDS(dat, "./gen/combined/temp/fph-ebm.rds")
