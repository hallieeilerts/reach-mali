################################################################################
#' @description Basic exploration of all the files that are part of mortalite-femme
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
################################################################################

# Level 1
# level_1_id, case_id, w_grappe, w_enu, w_cons, w_men, w_ind
# cluster, enumeration area, concession, household, individual?
dat <- read.csv("./data/reach_mortalite_femme_level1.csv")
head(dat)
table(dat$w_grappe, useNA = "always")
table(dat$w_enu, useNA = "always")
table(dat$w_cons, useNA = "always")
table(dat$w_men, useNA = "always")
table(dat$w_ind, useNA = "always")
length(unique(dat$w_grappe)) # 509
length(unique(dat$w_enu))    # 559
length(unique(dat$w_cons))   # 103
length(unique(dat$w_men))  # 18
length(unique(dat$w_ind))  # 105

# Interview timing, interviewer, language of interview
# number of visits, result of interview
dat <- read.csv("./data/reach_mortalite_femme_qsecover.csv")
head(dat)

# SECTION 1. CARACTÉRISTIQUES SOCIODÉMOGRAPHIQUES DE L'ENQUÊTÉE
# when did you move here, when were you born, did you go to school, what level, mobile phone, ethnicity, marital status
dat <- read.csv("./data/reach_mortalite_femme_qwsec01.csv")
head(dat)

# SECTION 2. REPRODUCTION
# summary birth history
dat <- read.csv("./data/reach_mortalite_femme_qwsec2a.csv")

# Full pregnancy history
dat <- read.csv("./data/reach_mortalite_femme_qwsec2b.csv")

# Child health questions
# Dose de vitamine A au cours des 6 derniers mois
# Diarrhée au cours des 2 dernières semaines
# A recherché conseils/traitement pour la diarrhée
# Lieu de conseils/traitement
# Liquide à partir de sachet spécial de SRO
# Quelque chose autre pour traiter la diarrhée
# Fièvre au cours des 2 dernières semaines
# Souffle rapide ou ces difficultés pour respirer
dat <- read.csv("./data/reach_mortalite_femme_qwsec6a.csv")

# Reproductive calendar
dat <- read.csv("./data/reach_mortalite_femme_qwsec2y.csv")
head(dat)

# Empty
dat <- read.csv("./data/reach_mortalite_femme_qwsec2c.csv")
# Empty
dat <- read.csv("./data/reach_mortalite_femme_qwsec11.csv")
