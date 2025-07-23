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
dat <- readRDS("./gen/combined/temp/fph-ebm.rds")
################################################################################

# Year of birth is missing
nrow(subset(dat, is.na(q220y))) # 32
# check if it can be recovered from current age (q225)
dat %>%
  filter(is.na(q220y) & !is.na(q225)) %>% 
  select(q223, q220y, q220m, q220d, q225, q223_aug, q224, qintdate)
# yes, there is one live births (q223_aug) for which recovery is possible (still alive q224)
# Recover when possible from current age (q225)
dat <- dat %>%
  mutate(q220y = case_when(
    !is.na(q225) & is.na(q220y) & q223_aug == 1 ~ as.numeric(format(qintdate, "%Y")) - q225,
    TRUE ~ q220y
  ))
nrow(subset(dat, is.na(q220y))) # 31

# drop observations that are still missing year of birth or pregnancy outcome
nrow(subset(dat, is.na(q220y))) # 31
nrow(subset(dat, is.na(q223_aug))) # 274
dat <- subset(dat, !is.na(q220y) & !is.na(q223_aug))
nrow(dat) # 79701

# Save --------------------------------------------------------------------

saveRDS(dat, "./gen/combined/temp/fph-recover-doby.rds")