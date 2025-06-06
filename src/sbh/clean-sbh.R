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
dat <- read.csv("./data/reach_mortalite_femme_qwsec2a.csv")
################################################################################

# Fill in missing q208 (nombre total d'enfants nés) with kids reported as living with them or elsewhere and kids who died

dat_miss <- subset(dat, is.na(q208))
dat_other <- subset(dat, !is.na(q208))
nrow(dat) == nrow(dat_miss) + nrow(dat_other)

dat_miss <- dat_miss %>%
  rowwise() %>%
  mutate(q208 = sum(q202, q204, q206, na.rm = TRUE))

dat2 <- rbind(dat_miss, dat_other)
dat2 <- dat2[order(dat2$qwsec2a_id),]

# Save output(s) ----------------------------------------------------------

saveRDS(dat2, "./gen/sbh/temp/sbh-clean.rds")


