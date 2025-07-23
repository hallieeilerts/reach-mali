################################################################################
#' @description Basic exploration of all the files that are part of concessions
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
################################################################################

# Level 1
# level_1_id, case_id, str_ea, str_enumerator_id, str_num
dat <- read.csv("./data/reach_concessions_level1.csv")
head(dat)
nrow(dat) # 162418

# urban/rural/ geo codes, district, aire, localite, street, number of children under 5, men, women
dat <- read.csv("./data/reach_concessions_str_rec.csv")
head(dat)
nrow(dat) # 162417

# gps locations for level 1 id
dat <- read.csv("./data/reach_concessions_str_gps.csv")
head(dat)
nrow(dat) # 162417
