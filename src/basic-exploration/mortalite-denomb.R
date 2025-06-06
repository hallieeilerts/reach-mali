################################################################################
#' @description Basic exploration of all the files that are part of mortalite-denomb
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
################################################################################

# Level 1
# level_1_id, case id, qhgrappe, qhenu, qhcons, qhmen
dat <- read.csv("./data/reach_mortalite_denomb_level1.csv")
head(dat)

# not sure...
# hhd location, visit date, qhh variables, hhc variables
dat <- read.csv("./data/reach_mortalite_denomb_qhhsecover.csv")
head(dat)

# not sure
# age cal, date, age mois
dat <- read.csv("./data/reach_mortalite_denomb_qhsec01xx.csv")
head(dat)

# not sure
# age, date, vac questions
dat <- read.csv("./data/reach_mortalite_denomb_section_che.csv")
head(dat)

# section_ct_id, level_1_id, q153, q154
dat <- read.csv("./data/reach_mortalite_denomb_section_ct.csv")
head(dat)

