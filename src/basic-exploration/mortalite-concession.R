################################################################################
#' @description Basic exploration of all the files that are part of mortalite-concession
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
################################################################################

# Level 1
# level_1_id, case id, str_ea, str_enumerator_id, str_num
dat <- read.csv("./data/reach_mortalite_conc_level1.csv")
head(dat)

# gps coordinates
dat <- read.csv("./data/reach_mortalite_conc_str_gps.csv")
head(dat)

# str variables. strata?
dat <- read.csv("./data/reach_mortalite_conc_str_rec.csv")
head(dat)

# metadata on last revision, saving
dat <- read.csv("./data/reach_mortalite_conc_cases.csv")
head(dat)

