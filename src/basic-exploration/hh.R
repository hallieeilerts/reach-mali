################################################################################
#' @description Basic exploration of all the files that are part of hh
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
################################################################################

# Level 1
# level_1_id, case_id, hh_ea, hh_enum, hh_cons
dat <- read.csv("./data/reach_hh_level1.csv")
head(dat)
nrow(dat) # 170403

# Household address, household head name, number of women 15-49, number of girls 1-11 and 12-59, number of boys 1-11 and 12-59
# number of household children treated? hh_enf_traite
dat <- read.csv("./data/reach_hh_identification_rec.csv")
head(dat)
nrow(dat) # 170403

# household variables with prefix hh
dat <- read.csv("./data/reach_hh_liste_menage.csv")
head(dat)
nrow(dat)

# Empty
dat <- read.csv("./data/reach_hh_naiss_enfants_1_59_mois.csv")
head(dat)
nrow(dat) # 0

# Id for children who died bw 1-59m, name of child who died, date of death, age of death, n of other deaths
dat <- read.csv("./data/reach_hh_deces_enfants_1_59_mois.csv")
head(dat)
nrow(dat) # 605

# Information on child treated, date of treatment
dat <- read.csv("./data/reach_hh_traitement_surveillance_enf.csv")
head(dat)
nrow(dat) # 457949
length(unique(dat$level_1_id)) # 170403


