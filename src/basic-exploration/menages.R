################################################################################
#' @description Basic exploration of all the files that are part of menage
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
################################################################################

# Level 1
# level_1, case id, hh_ea, hh_enumartor, hh_num
dat <- read.csv("./data/reach_menages_level1.csv")
head(dat)

# whether household is urban/rural, geocode, household size, chef telephone number, address
# total_men_select ?
dat <- read.csv("./data/reach_menages_hh_rec.csv")
head(dat)
unique(dat$total_men_select)
table(dat$total_men_select, useNA = "always")

# Gps location of households
dat <- read.csv("./data/reach_menages_hh_gps.csv")
head(dat)

