################################################################################
#' @description Exploration of survey weights file
#' @return None
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(kableExtra)
library(stringr)
library(haven)
# mortalite menage
mort_men_hhrec <- readRDS("./gen/mapping/output/hhrec.rds")
mort_men_sec01 <- readRDS("./gen/mapping/output/qhsec01-hhrec.rds")
# mortalite denomb
mort_denomb_hhrec <- readRDS("./gen/denomb/output/hhrec-denomb.rds")
mort_denomb_sec01 <- readRDS("./gen/denomb/output/qhsec01-denomb.rds")
# mortalite femme
mort_fem_sec01 <- readRDS("./gen/sec01/output/qsecover-qwsec01.rds")
mort_sbh <- read.csv("./data/reach_mortalite_femme_qwsec2a.csv")
# sampling weights
wt1 <- read_excel("./data/instat-20250623/Poids_Enquete_Base_Mortalite.xlsx", sheet = "Poidsformule")
wt2 <- read_excel("./data/instat-20250623/Poids_Enquete_Base_Mortalite.xlsx", sheet = "Poidsvf")
################################################################################

# Table mapping and sampled -----------------------------------------------

# Number of households mapped
tab1 <- mort_men_hhrec %>%
    filter(!is.na(hh_geo1_nom)) %>%
    group_by(hh_geo1_nom) %>%
    summarise(n_hhd = n(), .groups = "drop") %>%
    bind_rows(summarise(., hh_geo1_nom = "Total", n_hhd = sum(n_hhd, na.rm = TRUE)))

# Number of women 15-49 eligible in mapping
tab2 <- mort_men_sec01 %>%
    group_by(hh_geo1_nom) %>%
    filter(qh03x == 2 & !is.na(age_comb) & age_comb >= 15 & age_comb <= 49) %>%
    summarise(n_wom = n(), .groups = "drop") %>%
    bind_rows(summarise(., hh_geo1_nom = "Total", n_wom = sum(n_wom, na.rm = TRUE)))


# Number of households in denombrement
tab3 <- mort_denomb_hhrec %>%
  mutate(hh_geo1_nom = qhhli1) %>%
  group_by(hh_geo1_nom) %>%
  summarise(hhd_enum = n(), .groups = "drop") %>%
  bind_rows(summarise(., hh_geo1_nom = "Total", hhd_enum = sum(hhd_enum, na.rm = TRUE)))

# Number of eligible women in denombrement
tab4 <- mort_denomb_sec01 %>%
  mutate(hh_geo1_nom = qhhli1) %>%
  filter(sex == "Female" & age >= 15 & age <= 49) %>%
  group_by(hh_geo1_nom) %>%
  summarise(women_enum = n(), .groups = "drop") %>%
  bind_rows(summarise(., hh_geo1_nom = "Total", women_enum = sum(women_enum, na.rm = TRUE)))

# Clusters visited for mortality survey
tab5 <- mort_fem_sec01 %>%
    mutate(hh_geo1_nom = qlregion) %>%
    group_by(hh_geo1_nom) %>%
    summarise(cluster = n_distinct(grappe), .groups = "drop") %>%
    bind_rows(summarise(., hh_geo1_nom = "Total", cluster = sum(cluster, na.rm = TRUE)))
# checking that i get same numbers from denomb. yes.
mort_denomb_hhrec %>%
  mutate(hh_geo1_nom = qhhli1) %>%
  group_by(hh_geo1_nom) %>%
  summarise(cluster = n_distinct(qhgrappe), .groups = "drop") %>%
  bind_rows(summarise(., hh_geo1_nom = "Total", cluster = sum(cluster, na.rm = TRUE)))


# households sampled
tab6 <- mort_fem_sec01 %>%
    mutate(hh_geo1_nom = qlregion) %>%
    group_by(hh_geo1_nom, grappe, w_enu, w_cons) %>%
    summarise(menage = n_distinct(w_men)) %>%
    group_by(hh_geo1_nom) %>%
    summarise(hhd_samp = sum(menage)) %>%
    bind_rows(summarise(., hh_geo1_nom = "Total", hhd_samp = sum(hhd_samp, na.rm = TRUE)))

# women interviewed
# The wco3 variable is incomplete
# sometimes when it is NA, the person was interviewed
# get list of ID's of women who completed SBH
v_hassbh <- subset(mort_sbh, !is.na(q201))$level_1_id
tab7 <- mort_fem_sec01 %>%
  mutate(hh_geo1_nom = qlregion) %>%
  filter(level_1_id %in% v_hassbh) %>%
  group_by(hh_geo1_nom) %>%
  summarise(womenint = n()) %>%
  bind_rows(summarise(., hh_geo1_nom = "Total", womenint = sum(womenint, na.rm = TRUE)))
        
tab1 %>% 
  full_join(tab2, by = "hh_geo1_nom") %>%
  full_join(tab3, by = "hh_geo1_nom") %>%
  full_join(tab4, by = "hh_geo1_nom") %>%
  full_join(tab5, by = "hh_geo1_nom") %>%
  full_join(tab6, by = "hh_geo1_nom") %>%
  full_join(tab7, by = "hh_geo1_nom") %>%
  #mutate(per_hhd_samp = sprintf("%0.1f",round(hhd/n_hhd*100, 1)),
  #       per_wm_int = sprintf("%0.1f",round(womenint/n_wom*100, 1))) %>%
  select(hh_geo1_nom, n_hhd, n_wom, cluster, hhd_enum, women_enum, hhd_samp, womenint) %>%
  kbl(format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
      format.args = list(big.mark = ",", scientific = FALSE),
      col.names = c("Region", "Households", "Women 15-49y",
                    "Clusters sampled", 
                    "Households enumerated",
                    "Women 15-49y enumerated",
                    "Households sampled",
                    "Women 15-49y interviewed"),
      caption = "Geographic coverage and interview completion by region.",
      label = "interviews") %>% 
  column_spec(6:9, width = "5em")


# Strata and domains ------------------------------------------------------

names(wt1) <- tolower(names(wt1))
names(wt2) <- tolower(names(wt2))
wt1 <- subset(wt1, !is.na(grappe))

nrow(wt2)
length(unique(wt2$grappe))
length(unique(wt2$idse))

# checking with sheet 1

# number of clusters per region
wt1 %>%
  group_by(lregion) %>%
  summarise(n_cluster = n_distinct(grappe))

# number of treatment and comparison zones per region
wt1 %>%
  group_by(lregion, lstatut) %>%
  summarise(n_strat = n())

# number of clusters belonging to different strata per region
wt1 %>%
  group_by(lregion, cstrate) %>%
  summarise(n_strat = n())
wt1 %>%
  group_by(lregion, lstrate) %>%
  summarise(n_strat = n()) 

# checking with sheet 2

# number of clusters per region
wt2 %>%
  group_by(lregion) %>%
  summarise(n_cluster = n_distinct(grappe))

# number of treatment and comparison zones per region
wt2 %>%
  group_by(lregion, lstatut) %>%
  summarise(n_perstrat = n())

# Clusters within strata by region
# these two do the same
wt1 %>%
  group_by(lregion, cstrate, lstrate) %>%
  summarise(n_strat = n()) 


df_strata <- wt1 %>%
  group_by(lregion, cstrate, lstrate) %>%
  summarise(n_strat = n_distinct(grappe)) %>%
  mutate(lstrate = ifelse(lstrate == "Les capitales régionales",  "Les capitales regionales", lstrate)) %>%
  mutate(cstrate = as.character(cstrate))
df_zones <- wt1 %>%
  group_by(lregion, lstatut) %>%
  summarise(n_strat = n_distinct(grappe)) %>%
  rename(cstrate = lstatut)
df_tab <- bind_rows(df_strata, df_zones)
df_tab <- df_tab %>%
  ungroup() %>%
  select(-lstrate) %>%
  pivot_wider(
    names_from = cstrate,
    values_from = n_strat
  ) 
distinct(wt1[,c("cstrate", "lstrate")])
df_tab <- rbind(df_tab, c("Total", colSums(df_tab[,-c(1)])))

kbl(df_tab, format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
    format.args = list(big.mark = ",", scientific = FALSE),
    col.names = c("Region", "Regional capitals", "Small towns", 
                  "Rural, w<40% of population living >5km from the CSCOM", 
                  "Rural, w>40% of population living >5km from the CSCOM",
                  "Comparison", "Treatment"),
    caption = "Number of clusters per strata and domain by region.",
    label = "stratazones") %>% 
  column_spec(2, width = "4em") %>%
  column_spec(3, width = "3em") %>%
  column_spec(4:5, width = "7em") %>%
  add_header_above(c(" " = 1, 
                     "Strata" = 4,
                     "Domain" = 2)) %>%
  row_spec(0 ,  bold = F, extra_css = 'vertical-align: middle !important;')

