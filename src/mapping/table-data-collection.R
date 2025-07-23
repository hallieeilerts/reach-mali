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
library(readxl)
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
  mutate(#per_hhd_samp = sprintf("%0.1f",round(hhd/n_hhd*100, 1)),
         per_wm_int = sprintf("%0.1f",round(womenint/women_enum*100, 1))) %>%
  select(hh_geo1_nom, n_hhd, n_wom, cluster, hhd_enum, women_enum, hhd_samp, womenint, per_wm_int) %>%
  kbl(format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
      format.args = list(big.mark = ",", scientific = FALSE),
      col.names = c("Region", "Households", "Women 15-49y",
                    "Clusters sampled", 
                    "Households enumerated",
                    "Women 15-49y enumerated",
                    "Households sampled",
                    "Women 15-49y interviewed",
                    "% eligible women interviewed"),
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


# Visits per individual interviewed ---------------------------------------

# number of interviews per person
# i count this by counting the visit dates. there are 60 without a visit date.
nrow(subset(mort_fem_sec01, is.na(qintdate))) # 60
nrow(subset(mort_fem_sec01, is.na(qvdate1) & is.na(qvdate2) & is.na(qvdate3))) # 60
# these are 56 C'est pas la bonne personne, and 4 missing
# count these as having 1 interview for the purposes of this table
table(subset(mort_fem_sec01, is.na(qintdate))$wco3, useNA = "always")

# women interviewed
# The wco3 variable is incomplete
# sometimes when it is NA, the person was interviewed
# get list of ID's of women who completed SBH
v_hassbh <- subset(mort_sbh, !is.na(q201))$level_1_id

tab1 <- mort_fem_sec01 %>%
  select(level_1_id, qvdate1, qvdate2, qvdate3) %>% 
  filter(level_1_id %in% v_hassbh) %>%
  mutate(nvis = case_when(
    !is.na(qvdate3) ~ 3,
    TRUE ~ NA)) %>%
  mutate(nvis = case_when(
    is.na(nvis) & !is.na(qvdate2) ~ 2,
    TRUE ~ nvis)) %>%
  mutate(nvis = case_when(
    is.na(nvis) & !is.na(qvdate1) ~ 1,
    TRUE ~ nvis)) %>%
  mutate(nvis = case_when( # those without visit date recorded are counted as 1
    is.na(nvis) ~ 1,
    TRUE ~ nvis)) %>%
  group_by(nvis) %>%
  summarise(n = n())
tab1$per <- tab1$n/sum(tab1$n)
tab1 <- rbind(tab1, data.frame(nvis = "Total", n = sum(tab1$n), per = 1))
tab1$per <- round(tab1$per*100)
kbl(tab1, format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
    format.args = list(big.mark = ",", scientific = FALSE), 
    col.names = c("Visits", "Number of sampled individuals", "%"),
    caption = "Number of visits made to conduct interview per individual sampled.",
    label = "interview-visits")

# Table basic characteristics ---------------------------------------------

# women interviewed
# The wco3 variable is incomplete
# sometimes when it is NA, the person was interviewed
# get list of ID's of women who completed SBH
v_hassbh <- subset(mort_sbh, !is.na(q201))$level_1_id

mort_fem_sec01 %>%
  filter(level_1_id %in% v_hassbh) %>%
  mutate(
    qlangi = case_when( # language of interview
      qlangi == 1 ~ "French",
      qlangi == 2 ~ "Bambara/Malinke",
      qlangi == 3 ~ "Sonraï/Djerma",
      qlangi == 4 ~ "Peuhl/Foulfouldé",
      qlangi == 5 ~ "Sénoufo",
      qlangi == 6 ~ "Marka/Soninké",
      qlangi == 7 ~ "Dogon",
      qlangi == 8 ~ "Minianka",
      qlangi == 9 ~ "Tamacheck",
      qlangi == 10 ~ "Bobo/Dafing",
      qlangi == 11 ~ "Bozo",
      qlangi == 96 ~ "Other",
      TRUE ~ NA_character_
    ),
    qlangr = case_when( # language of interviewee
      qlangr == 1 ~ "French",
      qlangr == 2 ~ "Bambara/Malinke",
      qlangr == 3 ~ "Sonraï/Djerma",
      qlangr == 4 ~ "Peuhl/Foulfouldé",
      qlangr == 5 ~ "Sénoufo",
      qlangr == 6 ~ "Marka/Soninké",
      qlangr == 7 ~ "Dogon",
      qlangr == 8 ~ "Minianka",
      qlangr == 9 ~ "Tamacheck",
      qlangr == 10 ~ "Bobo/Dafing",
      qlangr == 11 ~ "Bozo",
      qlangr == 96 ~ "Other",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(across(
    c(qlangi, qlangr),
    ~factor(.x, levels = c(
      "French",
      "Bambara/Malinke",
      "Sonraï/Djerma",
      "Peuhl/Foulfouldé",
      "Sénoufo",
      "Marka/Soninké",
      "Dogon",
      "Minianka",
      "Tamacheck",
      "Bobo/Dafing",
      "Bozo",
      "Other"
    ))
  )) %>%
  mutate(q112 = case_when( # self-rated health
    q112 == 1 ~ "Very good",
    q112 == 2 ~ "Good",
    q112 == 3 ~ "Moderately good",
    q112 == 4 ~ "Bad",
    q112 == 5 ~ "Very bad",
    TRUE ~ NA_character_),    
    q112 = factor(
      q112,
      levels = c(
        "Very good",
        "Good",
        "Moderately good",
        "Bad",
        "Very bad")
    )
  ) %>%
  mutate(q113 = case_when( # went to school
    q113 == 1 ~ "Yes",
    q113 == 2 ~ "No",
    TRUE ~ NA_character_)
  ) %>%
  mutate(q114 = case_when( # Highest level of school
    is.na(q114) & q113 == "No" ~ "None",
    q114 == 1 ~ "Lower primary school",
    q114 == 2 ~ "Upper primary school",
    q114 == 3 ~ "High school or vocational",
    q114 == 5 ~ "University",
    TRUE ~ NA_character_),    
    q114 = factor(
      q114,
      levels = c(
        "None",
        "Lower primary school",
        "Upper primary school",
        "High school or vocational",
        "University")
    )
  ) %>%
  mutate(q122 = case_when( # has a cell phone
    q122 == 1 ~ "Yes",
    q122 == 2 ~ "No",
    TRUE ~ NA_character_)
  ) %>%
  mutate(
    q131 = case_when( # ethnicity
      q131 == 1 ~ "Bambara",
      q131 == 2 ~ "Malinke",
      q131 == 3 ~ "Peuhl",
      q131 == 4 ~ "Sarakole",
      q131 == 5 ~ "Kassonke",
      q131 == 6 ~ "Sonraï",
      q131 == 7 ~ "Dogon",
      q131 == 8 ~ "Touareg",
      q131 == 9 ~ "Sénoufo",
      q131 == 10 ~ "Bobo",
      q131 == 11 ~ "Bozo",
      q131 == 12 ~ "Arab",
      q131 == 16 ~ "Other Malian ethnicity",
      q131 == 21 ~ "Other ECOWAS country",
      q131 == 22 ~ "Other African country",
      q131 == 23 ~ "Other nationalities",
      TRUE ~ NA_character_
    ),
    q131 = factor(
      q131,
      levels = c(
        "Bambara",
        "Malinke","Peuhl","Sarakole","Kassonke","Sonraï","Dogon",
        "Touareg","Sénoufo", "Bobo", "Bozo", "Arabe", "Other Malian ethnicity",
        "Other ECOWAS country", "Other African country","Other nationalities"
      )
    )
  ) %>%
  mutate(q132 = case_when( # marital status
    q132 == 1 ~ "Married or living together",
    q132 == 2 ~ "Divorced/separated",
    q132 == 3 ~ "Widowed",
    q132 == 4 ~ "Never married",
    TRUE ~ NA_character_),    
    q132 = factor(
      q132,
      levels = c(
        "Married or living together",
        "Divorced/separated",
        "Widowed",
        "Never married")
    )
  ) %>%
  select(level_1_id, qlangi, qlangr, q112, q113, q114, q122, q131, q132, agecat_resp) %>% 
  pivot_longer(
    cols = -level_1_id,
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable, value) %>%
  summarise(n = n()) %>% 
  mutate(value = case_when(
    is.na(value) ~ "Missing",
    TRUE ~ value
  )) %>% 
  mutate(variable = case_when(
    variable == "qlangi" ~ "Language of interview", 
    variable == "qlangr" ~ "Language of interviewee", 
    variable == "q112" ~ "Self-rated health",
    variable == "q113" ~ "Attended school",
    variable == "q114" ~ "Highest level of schooling",
    variable == "q122" ~ "Owns mobile phone",
    variable == "q131" ~ "Ethnicity",
    variable == "q132" ~ "Marital status",
    variable == "agecat_resp" ~ "Age of respondent",
    ),
    value_order = 1,
    value_order = case_when(
      value == "Missing" ~ 99,
      value == "None" ~ 2, 
      value == "Lower primary school" ~ 3, 
      value == "Upper primary school" ~ 4, 
      value == "High school or vocational" ~ 5,
      value == "Very bad" ~ 2,
      value == "Bad" ~ 3,
      value == "Moderately good" ~ 4,
      value == "Good" ~ 5,
      value == "Very good" ~ 6,
      TRUE ~ value_order
    )) %>%
  arrange(variable, value_order, value) %>%
  filter(!(variable %in% c("Language of interview", "Language of interviewee"))) %>%
  group_by(variable) %>%
  mutate(total = sum(n),
         per = sprintf("%.1f", round(n/total*100, 1))) %>%
  ungroup() %>%
  mutate(variable_order = row_number(),
         variable_order = case_when(
           variable == "Attended school" & value == "No" ~ 25.1,
           variable == "Attended school" & value == "Yes" ~ 25.2,
           TRUE ~ variable_order
         )) %>%
  arrange(variable_order) %>%
  filter(!(variable %in% c("Attended school"))) %>%
  bind_rows(summarise(., variable = "Total", 
                      value = "",
                      n = mort_fem_sec01 %>% filter(level_1_id %in% v_hassbh) %>% nrow(),
                      per = "100.0")) %>% 
  select(-c(value_order, total, variable_order)) %>%
  kbl(format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "", escape = TRUE, 
      format.args = list(big.mark = ",", scientific = FALSE),
      col.names = c("Variable", "Value", "N", "%"),
      caption = "Characteristics of respondents.",
      label = "interview-sample") %>% 
  collapse_rows(1) #column_spec(6:9, width = "5em")

