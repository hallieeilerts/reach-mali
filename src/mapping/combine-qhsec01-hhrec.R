################################################################################
#' @description clean household mortality forms
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
#' Inputs
level1 <- read.csv("./data/reach_mortalite_menage_level1.csv") # households
hhrec <- read.csv("./data/reach_mortalite_menage_hh_rec.csv")  # households
qhsec01 <- read.csv("./data/reach_mortalite_menage_qhsec01x.csv") # individuals
################################################################

length(unique(level1$level_1_id)) # 36952
length(unique(hhrec$level_1_id)) # 36952
length(unique(qhsec01$level_1_id)) #36766

# Households IDs and clusters, merge on...
# region and summary variables for household
dat <- level1 %>%
  left_join(hhrec %>% select(level_1_id, hh_geo1_nom, hh_under_5, hh_fem1549), by = "level_1_id") 
nrow(level1) # 36952
nrow(dat) # 36952

# Households IDs, clusters, region and summary variables for household, merge on...
# Individuals with sex (qh03x) and DOB (qh04)
dat_ind <- dat %>%
  left_join(qhsec01, by = "level_1_id")
length(unique(qhsec01$qhsec01x_id)) # 365566
nrow(qhsec01) # 365566
nrow(dat_ind) # 365752
# why would it increase merging household information onto individual?
# some household records are not in the individual file
sum(!(unique(dat_ind$level_1_id) %in% unique(qhsec01$level_1_id))) #  186
# none of the households in the individual file are missing from household records
sum(!(unique(qhsec01$level_1_id) %in% unique(dat_ind$level_1_id) )) #  0
sum(!(unique(qhsec01$qhsec01x_id) %in% dat_ind$qhsec01x_id)) # 0
# the number of records grew after the merge. why?
# dat_ind %>%
#   group_by(qhsec01x_id) %>%
#   mutate(n = n()) %>%
#   filter(n > 1) %>%
#   View()
# some NA individuals ids
# however they weren't there in the original qhsec01 file
# not sure why they got added, but removing
nrow(subset(qhsec01, is.na(qhsec01x_id))) # 0
nrow(subset(dat_ind, is.na(qhsec01x_id))) # 186
dat_ind <- subset(dat_ind, !is.na(qhsec01x_id))
nrow(dat_ind) # 365566

# create combined variable for age
# calculate age variable from qhageestx, qhagemoisestx, qh04xa, qh04xb, qh04xc
# if dob is available, use to calculate age from hhsurvdatex
# if agemoisest is available, use that
# if qhageestx is available, use that
# if year of dob is available, born June 15 and use that
dat_ind <- dat_ind %>%
  mutate(dob =  as.Date(paste(qh04xa, qh04xb, qh04xc, sep = "-"), format = "%Y-%m-%d"),
         hhsurvdatex = as.Date(hhsurvdatex, format = "%d/%m/%Y"),
         age_comb = as.numeric(hhsurvdatex - dob)/365.25) %>%
  mutate(age_comb = case_when(
    is.na(age_comb) & !is.na(qhagemoisx) ~ qhagemoisx/12,
    is.na(age_comb) & is.na(qhagemoisx) & !is.na(qhageestx) ~ qhageestx,
    is.na(age_comb) & is.na(qhagemoisx) & is.na(qhageestx) & !is.na(qh04xa) ~ as.numeric(hhsurvdatex - as.Date(paste0(qh04xa, "-06-15"), format = "%Y-%m-%d"))/365.25,
    is.na(age_comb) & is.na(qhagemoisx) & is.na(qhageestx) & is.na(qh04xa) & !is.na(qh04x) ~ qh04x,
    TRUE ~ age_comb
  ))
#select(qh04xa, qh04xb, qh04xc, dob, qhagemoisx, qhageestx, hhsurvdatex, age_comb) %>% # to check
# missing age
nrow(subset(dat_ind, is.na(age_comb))) # 1126
# missing sex
nrow(subset(dat_ind, is.na(qh03x))) # 1082
#View(subset(dat_ind, is.na(age_comb)))

# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/mapping/output/hhrec.rds")
saveRDS(dat_ind, "./gen/mapping/output/qhsec01-hhrec.rds")

