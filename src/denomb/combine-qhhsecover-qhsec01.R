################################################################################
#' @description
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
#' Inputs
level1 <- read.csv("./data/reach_mortalite_denomb_level1.csv")
qhhsecover <- read.csv("./data/reach_mortalite_denomb_qhhsecover.csv")
qhsec01 <- read.csv("./data/reach_mortalite_denomb_qhsec01xx.csv")
################################################################################

nrow(level1) # 10247
nrow(qhhsecover) # 10247
nrow(qhsec01) # 119440

length(unique(level1$level_1_id)) # 10247
length(unique(qhhsecover$level_1_id)) # 10247
length(unique(qhsec01$level_1_id)) # 10242

# Households IDs and clusters, merge on...
# region, cercle, commune, se, village, health_area, team
dat <- level1 %>%
  left_join(qhhsecover %>% select(level_1_id, qhhi1, qhhli1, qhhi2, qhhli2, 
                                  qhhdistrict, qhhli3, qhhi3, idse, qhhi4, 
                                  qhhi5, hhi7), by = "level_1_id") 
nrow(level1) # 10247
nrow(dat) # 10247

# age/sex information for individuals
df_qhsec01 <- qhsec01 %>%
  select(level_1_id, qhh01x, qhh03x, qhh04x, qhh05x) %>%
  rename(member_id = qhh01x,
         sex = qhh03x,
         age = qhh04x,
         hhd_hd_rel = qhh05x) %>%
  mutate(sex = case_when(
    sex == 1 ~ "Male",
    sex == 2 ~ "Female",
    TRUE ~ NA 
  ),
  hhd_hd_rel = case_when(
    hhd_hd_rel == 1 ~ "head",
    hhd_hd_rel == 2 ~ "wife",
    hhd_hd_rel == 3 ~ "child",
    hhd_hd_rel == 4 ~ "son or daughter in-law",
    hhd_hd_rel == 5 ~ "grandchild",
    hhd_hd_rel == 6 ~ "grandparent",
    hhd_hd_rel == 7 ~ "step-parent",
    hhd_hd_rel == 8 ~ "sibling",
    hhd_hd_rel == 9 ~ "other",
    hhd_hd_rel == 10 ~ "adopted",
    hhd_hd_rel == 11 ~ "orphan",
    hhd_hd_rel == 98 ~ "unknown",
    TRUE ~ NA
  ))

dat_ind <- merge(dat, df_qhsec01, by="level_1_id")
nrow(dat) # 119440

# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/denomb/output/hhrec-denomb.rds")
saveRDS(dat_ind, "./gen/denomb/output/qhsec01-denomb.rds")
