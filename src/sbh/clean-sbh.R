################################################################################
#' @description Clean SBH
#' @return
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
dat <- readRDS("./gen/sbh/temp/sbh-qsecover-qwsec01.rds")
################################################################################

# Fill in missing q208 (nombre total d'enfants nés) with kids reported as living with them, living elsewhere, and kids who died
dat_miss <- subset(dat, is.na(q208))
dat_other <- subset(dat, !is.na(q208))
nrow(dat) == nrow(dat_miss) + nrow(dat_other)
dat_miss <- dat_miss %>%
  rowwise() %>%
  mutate(q208 = sum(q202, q204, q207a, q207b, na.rm = TRUE))
dat <- rbind(dat_miss, dat_other)
dat <- dat[order(dat$qwsec2a_id),]

# Add variable for number of children surviving (living with them or elsewhere)
# Add variable for number of children died
# will be 0 if NAs in q207a and q207b
dat <- dat %>%
  rowwise() %>%
  mutate(q203_comb = sum(q203a, q203b, na.rm = TRUE), # sons living at home, daughters living at home
         q205_comb = sum(q205a, q205b, na.rm = TRUE), # sons living elsewhere, daughters living elsewhere
         q207_comb = sum(q207a, q207b, na.rm = TRUE)) # sons died, daughters died
# q207_comb = ifelse(!is.na(q206) & q206 == 1 & q207_comb == 0, NA, q207_comb)
# the above line will recode children died to NA if it is reported that a child has died (q206) but q207_comb is zero
# choosing not to do this.
nrow(subset(dat, is.na(q203_comb))) # 0
nrow(subset(dat, is.na(q205_comb))) # 0
nrow(subset(dat, is.na(q207_comb))) # 0

# Add up birth and pregnancy totals
dat <- dat %>%
  rowwise() %>%
  mutate(sum_cebcd = sum(q203_comb, q205_comb, q207_comb, na.rm = TRUE),
         sum_preg = sum(q203_comb, q205_comb, q207_comb, q211, na.rm = TRUE)) # q211 is nombre de naissance non-vivante

# Adjust q208 to match sum_cebcd
dat$q208 <- ifelse(dat$sum_cebcd != dat$q208, dat$sum_cebcd, dat$q208)
# Adjust q212 to match sum_preg
dat$q212 <- ifelse((!is.na(dat$q212) & dat$sum_preg != dat$q212) | is.na(dat$q212), dat$sum_preg, dat$q212)
dat$sum_cebcd <- dat$sum_preg <- NULL

# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/sbh/temp/sbh-clean.rds")


