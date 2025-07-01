################################################################################
#' @description Compare SBH and FPH agreement
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(cowplot)
library(viridis)
library(lubridate)
#' Inputs
sbh <- readRDS("./gen/sbh/temp/sbh-clean.rds")
fph <- readRDS("./gen/fph/output/fph-tips.rds")
fph_unaltered <- read.csv("./data/reach_mortalite_femme_qwsec2b.csv")
################################################################################

# sbh
# total number of live births
#sbh$q208
# total number of pregnancies
#sbh$q212
# total number of children died
sbh$q207_comb

# in sbh...
# number of births is not zero, pregnancies is zero
nrow(subset(sbh, q208 != 0 & q212 == 0)) # 0
# number of births is zero, pregnancies is not zero
nrow(subset(sbh, q208 == 0 & q212 != 0)) # 143

# in the fph there isn't a total births or pregnancies variable
# add up pregnancies
fph_p <- fph %>%
  group_by(level_1_id) %>%
  summarise(n_preg = n())
# add up live births
fph_lb <- fph %>%
  mutate(hadlb = ifelse(!is.na(q223_aug) & q223_aug == 1, 1, 0)) %>%
  group_by(level_1_id) %>%
  summarise(n_bth = sum(hadlb))
# add up deaths
fph_dth <- fph %>%
  mutate(haddth = ifelse(!is.na(q223_aug) & q223_aug == 1 & !is.na(q224) & q224 == 2, 1, 0)) %>%
  group_by(level_1_id) %>%
  summarise(n_dth = sum(haddth)) 

fph_p_unaltered <- fph_unaltered %>%
  group_by(level_1_id) %>%
  summarise(n_preg = n())
# add up live births
fph_lb_unaltered <- fph_unaltered %>%
  mutate(hadlb = ifelse(!is.na(q223) & q223 == 1, 1, 0)) %>%
  group_by(level_1_id) %>%
  summarise(n_bth = sum(hadlb))
# add up deaths
fph_dth_unaltered <- fph_unaltered %>%
  mutate(haddth = ifelse(!is.na(q223) & q223 == 1 & !is.na(q224) & q224 == 2, 1, 0)) %>%
  group_by(level_1_id) %>%
  summarise(n_dth = sum(haddth))

# sbh ids that are not in fph
length(unique(sbh$level_1_id)[!(unique(sbh$level_1_id) %in% unique(fph$level_1_id))]) # 7027
# remember that i have dropped some observations from the fph though (missing years, preg outcomes, and imposssible aods)
# check with raw fph
length(unique(sbh$level_1_id)[!(unique(sbh$level_1_id) %in% unique(fph_unaltered$level_1_id))]) # 6991

# fph ids that are not in sbh
length(unique(fph$level_1_id)[!(unique(fph$level_1_id) %in% unique(sbh$level_1_id))]) # 0

# among those who had children
sbh_hadc <- subset(sbh, q208 != 0 | q212 != 0 )
length(unique(sbh_hadc$level_1_id)[!(unique(sbh_hadc$level_1_id) %in% unique(fph$level_1_id))]) # 46
# 46 of those in sbh are not in fph when they should be
# again though... i have dropped observations
# check with raw fph
length(unique(sbh_hadc$level_1_id)[!(unique(sbh_hadc$level_1_id) %in% unique(fph_unaltered$level_1_id))]) # 12

# birth agreement ---------------------------------------------------------

# 19 individuals don't have fph when sbh indicated they have births
datall <- merge(sbh[,c("level_1_id", "q208", "q212")], fph_p, by = "level_1_id", all = TRUE)
nrow(subset(datall, is.na(q208))) # 0
nrow(subset(datall, is.na(q212))) # 0
nrow(subset(datall, (q208 != 0 | q212 != 0) & is.na(n_preg))) # 46
# those that are not even in unaltered file
nrow(subset(datall, (q208 != 0 | q212 != 0) & is.na(n_preg) & !(level_1_id %in% fph_p_unaltered$level_1_id))) # 12

# 19 individuals don't have fph when sbh indicated they have preg
datall <- merge(sbh[,c("level_1_id", "q208", "q212")], fph_p, by = "level_1_id", all = TRUE)
datall <- merge(datall, fph_lb, by = "level_1_id", all = TRUE)
nrow(subset(datall, is.na(q208))) # 0
nrow(subset(datall, is.na(q212))) # 0
nrow(subset(datall, (q208 != 0 | q212 != 0) & is.na(n_bth))) # 46
# those that are not even in unaltered file
nrow(subset(datall, (q208 != 0 | q212 != 0) & is.na(n_bth) & !(level_1_id %in% fph_p_unaltered$level_1_id))) # 12
# number of births is zero, pregnancies is not zero, and no fph conducted
nrow(subset(datall, q208 == 0 & q212 != 0 & is.na(n_bth))) # 0
# number of births is not zero, pregnancies is zero, and no fph conducted
nrow(subset(datall, q208 != 0 & q212 == 0 & is.na(n_bth))) # 0

# most of the time individuals with no births or pregnancies don't have an FPH
nrow(subset(datall, q208 == 0 & q212 == 0 & is.na(n_bth))) # 6981
nrow(subset(datall, q208 == 0 & q212 == 0 & is.na(n_bth) & !(level_1_id %in% fph_p_unaltered$level_1_id))) # 6979
nrow(subset(datall, q208 == 0 & q212 == 0 & is.na(n_preg))) # 6981
nrow(subset(datall, q208 == 0 & q212 == 0 & is.na(n_preg) & !(level_1_id %in% fph_p_unaltered$level_1_id))) # 6979
# sometimes they do
nrow(subset(datall, q208 == 0 & q212 == 0 & !is.na(n_bth))) # 58
nrow(subset(datall, (q208 == 0 & q212 == 0 & !is.na(n_bth)) | (q208 == 0 & q212 == 0 & level_1_id %in% fph_p_unaltered$level_1_id))) # 60
nrow(subset(datall, q208 == 0 & q212 == 0 & !is.na(n_preg))) # 58
nrow(subset(datall, (q208 == 0 & q212 == 0 & !is.na(n_preg)) | (q208 == 0 & q212 == 0 & level_1_id %in% fph_p_unaltered$level_1_id))) # 60

# drop those with no births or pregnancies
dat <- subset(datall, !(q208 == 0 & q212 == 0))
# check total children born match
nrow(subset(dat, is.na(q208))) # 0 
nrow(subset(dat, is.na(n_bth))) # 46
nrow(subset(dat, q208 == n_bth)) # 18667
nrow(subset(dat, q208 != n_bth)) # 350
nrow(subset(dat, q208 == n_bth)) + nrow(subset(dat, q208 != n_bth)) + nrow(subset(dat, is.na(n_bth))) == nrow(dat)
# check total pregnancies match
nrow(subset(dat, is.na(q212))) # 0 
nrow(subset(dat, is.na(n_preg))) # 46
nrow(subset(dat, q212 == n_preg)) # 18623
nrow(subset(dat, q212 != n_preg)) # 394
nrow(subset(dat, q212 == n_preg)) + nrow(subset(dat, q212 != n_preg)) + nrow(subset(dat, is.na(n_preg))) == nrow(dat)

# if q208 == 0 & q212 == 0 and n_bth or n_preg are missing (which happens when the FPH wasn't administered), reassign fph as 0
datall$n_bth[datall$q208 == 0 & datall$q212 == 0 & is.na(datall$n_bth)] <- 0
datall$n_preg[datall$q208 == 0 & datall$q212 == 0 & is.na(datall$n_preg)] <- 0

# Also recode as 0 when the individual was dropped from FPH during cleaning
datall$n_bth[is.na(datall$n_bth) & datall$level_1_id %in% fph_unaltered$level_1_id] <- 0
datall$n_preg[is.na(datall$n_preg) & datall$level_1_id %in% fph_unaltered$level_1_id] <- 0

# check difference in reported number
datall$dif_bth <- datall$q208 - datall$n_bth
datall$dif_preg <- datall$q212 - datall$n_preg

# Denominator here is the 26051 women who were interviewed in the SBH
tab_agreebth <- datall %>%
  mutate(dif = cut(dif_bth, breaks = c(-Inf, -4, -3, -2, -1, 0, 1, 2, 3, Inf),
                          labels = c("<=-4", "-3", "-2", "-1", "0", "1", "2", "3", "4+"))) %>%
  group_by(dif) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(total = sum(n),
         per = sprintf("%0.1f",round(n/total*100, 1))) %>%
  mutate(dif = case_when(
    is.na(dif) ~ "FPH missing", 
    TRUE ~ dif)
  ) %>%
  mutate(var = "Live births")
tab_agreepreg <- datall %>%
  mutate(dif = cut(dif_preg, breaks = c(-Inf, -4, -3, -2, -1, 0, 1, 2, 3, Inf),
                          labels = c("<=-4", "-3", "-2", "-1", "0", "1", "2", "3", "4+"))) %>%
  group_by(dif) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(total = sum(n),
         per = sprintf("%0.1f",round(n/total*100, 1))) %>%
  mutate(dif = case_when(
    is.na(dif) ~ "FPH missing", 
    TRUE ~ dif)
  ) %>%
  mutate(var = "Pregnancies")
tab_agree <- rbind(tab_agreebth, tab_agreepreg)

datall %>%
  filter(dif_bth != 0) %>%
  ggplot() +
  geom_histogram(aes(x=dif_bth), fill = "#0D0887FF") +
  labs(y = "n", x = "Difference, sbh minus fph", title = "Disagreement in N live births in SBH and FPH") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        legend.box.background = element_blank(),
        legend.background = element_blank())
datall %>%
  filter(dif_preg != 0) %>%
  ggplot() +
  geom_histogram(aes(x=dif_preg), fill = "#0D0887FF") +
  labs(y = "n", x = "Difference, sbh minus fph", title = "Disagreement in N preg. in SBH and FPH") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        legend.box.background = element_blank(),
        legend.background = element_blank())

# death agreement ---------------------------------------------------------

# 19 individuals don't have fph when sbh indicated they have births
dth <- merge(sbh[,c("level_1_id", "q208", "q212","q207_comb")], fph_dth, by = "level_1_id", all = TRUE)
nrow(subset(dth, is.na(q207_comb))) # 0
# not in fph
nrow(subset(dth, is.na(n_dth))) # 7027
# among those who had children
sbh_hadc <- subset(sbh, q208 != 0 | q212 != 0 )
length(unique(sbh_hadc$level_1_id)[!(unique(sbh_hadc$level_1_id) %in% unique(dth$level_1_id))]) # 0

# Denominator here is the 26051 women who were interviewed in the SBH
# if q208 == 0 & q212 == 0 and n_bth or n_preg are missing (which happens when the FPH wasn't administered), reassign fph as 0
dth$n_dth[dth$q208 == 0 & dth$q212 == 0 & is.na(dth$n_dth)] <- 0

# Also recode as 0 when the individual was dropped from FPH during cleaning
dth$n_dth[is.na(dth$n_dth) & dth$level_1_id %in% fph_unaltered$level_1_id] <- 0

dth$dif_dth <- dth$q207_comb - dth$n_dth

tab_agreedth <- dth %>%
  mutate(dif = cut(dif_dth, breaks = c(-Inf, -4, -3, -2, -1, 0, 1, 2, 3, Inf),
                   labels = c("<=-4", "-3", "-2", "-1", "0", "1", "2", "3", "4+"))) %>%
  group_by(dif) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(total = sum(n),
         per = sprintf("%0.1f",round(n/total*100, 1))) %>%
  mutate(dif = case_when(
    is.na(dif) ~ "FPH missing", 
    TRUE ~ dif)
  ) %>%
  mutate(var = "Deaths")

dth %>%
  filter(dif_dth != 0) %>%
  ggplot() +
  geom_histogram(aes(x=dif_dth), fill = "#0D0887FF") +
  labs(y = "n", x = "Difference, sbh minus fph", title = "Disagreement in N deaths in SBH and FPH") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        legend.box.background = element_blank(),
        legend.background = element_blank())


# Save output(s) ----------------------------------------------------------

saveRDS(datall, "./gen/fph/audit/dat_aud-sbh-bth-agreement.rds")
saveRDS(dth, "./gen/fph/audit/dat_aud-sbh-dth-agreement.rds")
saveRDS(tab_agree, "./gen/fph/audit/tab_sbh-bth-agreement.rds")
saveRDS(tab_agreedth, "./gen/fph/audit/tab_sbh-dth-agreement.rds")
