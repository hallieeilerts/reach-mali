################################################################################
#' @description Basic exploration of all the files that are part of mortalite-menage
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
library(dplyr)
library(tidyr)
################################################################################

# Level 1
# level_1, case id, hh_ea, hh_enumerator, hh_num, hh_id
dat <- read.csv("./data/reach_mortalite_menage_level1.csv")
head(dat)

# hh_gps_id, level_1_id, gps coordinates
dat <- read.csv("./data/reach_mortalite_menage_hh_gps.csv")
head(dat)

# household geocodes, hh_head name, hh size, hh_head tel, address
# number of women 15-49, interview day
# total_men_select, hh_selected
dat <- read.csv("./data/reach_mortalite_menage_hh_rec.csv")
head(dat)
table(dat$hh_urb_rur)
table(dat$hh_geo4_nom)
table(dat$total_men_select, useNA = "always")
table(dat$hh_selected, useNA = "always") # none selected...
# households that were selected per geo4? is geo4 cluster?
dat %>%
  group_by(hh_geo4_nom, total_men_select) %>%
  summarise(n())

# qh variables - household questionnaire?
# name and date
dat <- read.csv("./data/reach_mortalite_menage_qhsec01x.csv")
head(dat)

# vac variables, haz variables, moutiquaire
dat <- read.csv("./data/reach_mortalite_menage_section_ce.csv")
head(dat)

# number of deaths, sex
dat <- read.csv("./data/reach_mortalite_menage_section_de.csv")
head(dat)

