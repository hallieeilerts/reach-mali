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
library(flextable)
library(officer)
# HOUSEHOLD FORMS
# checking grappes in the household forms that are not in mortality femme survey
level1 <- read.csv("./data/reach_hh_level1.csv")
hhident <- read.csv("./data/reach_hh_identification_rec.csv") # exist_dece
hhrec_men <- read.csv("./data/reach_menages_hh_rec.csv")
#hhlist <- read.csv("./data/reach_hh_liste_menage.csv") # questions about age of household head and who is the mother of children in the household, but nothing mortality related
# hhdec <- read.csv("./data/reach_hh_deces_enfants_1_59_mois.csv") # information on children who died, but only 605 records...
# # mortalite menage
# mort_men_hhrec <- readRDS("./gen/mapping/output/hhrec.rds")
# mort_men_sec01 <- readRDS("./gen/mapping/output/qhsec01-hhrec.rds")
# # mortalite denomb
# mort_denomb_hhrec <- readRDS("./gen/denomb/output/hhrec-denomb.rds")
# mort_denomb_sec01 <- readRDS("./gen/denomb/output/qhsec01-denomb.rds")
# # mortalite femme
# mort_fem_sec01 <- readRDS("./gen/sec01/output/qsecover-qwsec01.rds")
# mort_sbh <- read.csv("./data/reach_mortalite_femme_qwsec2a.csv")
# sampling weights
wt1 <- read_excel("./data/instat-20250623/Poids_Enquete_Base_Mortalite.xlsx", sheet = "Poidsformule")
wt2 <- read_excel("./data/instat-20250623/Poids_Enquete_Base_Mortalite.xlsx", sheet = "Poidsvf")
# enchantillon se
ench <- read_excel("./data/instat-20250623/Echantillon_SE_Mortality.xlsx", sheet = "Echant_SE_by_Statut_Corrigé")
################################################################################

# # MORTALITE MENAGE
# # checking if there are many grappes in the mortalite menage that are not in the denomb or mortality femme survey
# # (trying to identify insecure grappes that were left out)
# mort_men_sec01$hh_ea[!(mort_men_sec01$hh_ea %in% mort_denomb_hhrec$qhgrappe)] # just one-- #234
# mort_men_sec01$hh_ea[!(mort_men_sec01$hh_ea %in% mort_fem_sec01$grappe)] # 234
# mort_men_hhrec$hh_ea[!(mort_men_hhrec$hh_ea %in% mort_fem_sec01$grappe)] # NA 234 234
# test <- hhrec
# test <- unique(test$hh_ea)
# test[!(test %in% mort_fem_sec01$grappe)]
# 
# # MENAGE
# # checking if there are many grappes in the menage files that are not in the denomb or mortality femme survey
# # yes. it appears these are are the grappes that were left out
# level1 <- read.csv("./data/reach_menages_level1.csv") # households
# nrow(level1) # 206033
# hhrec <- read.csv("./data/reach_menages_hh_rec.csv")  # households
# nrow(hhrec)  # 206033
# table(hhrec$total_men_select, useNA = "always")
# # 171314 selected it seems
# #hhgps <- read.csv("./data/reach_menages_hh_gps.csv")
# test <- merge(level1, hhrec, by = "level_1_id")
# test <- subset(test, total_men_select == 1)
# nrow(test) #  171314
# # how many clusters are not in the mortality survey?
# v_test <- unique(test$hh_ea)
# length(v_test) # 3011
# v_test[!(v_test %in% mort_fem_sec01$grappe)]
# length(v_test[(v_test %in% mort_fem_sec01$grappe)]) # 488
# length(v_test[!(v_test %in% mort_fem_sec01$grappe)]) # 2523
# length(v_test[(v_test %in% mort_denomb_sec01$qhgrappe)]) # 488
# length(v_test[(v_test %in% mort_men_hhrec$hh_ea)]) # 460
# length(v_test[!(v_test %in% mort_men_hhrec$hh_ea)]) # 2551
# # i'm looking for a number of around 94.
# # also, none of the menage files have mortality information
# # look for them in household forms


# household size, number of children under 5, respondent name, address
nrow(hhrec_men) # 206032
nrow(level1) # 170403
nrow(hhident) # 170403
length(hhident$level_1_id) # 170403

# Data dictionary file for HOUSEHOLD FORMS
# REACH1HH

# mortality related questions in hhident

# [Item]
# Label=Total d'enfants de 1 à 6 mois nés au cours des 6 derniers mois
# Name=HH_ENF06M
# Start=754
# Len=2
# DataType=Numeric
# ZeroFill=Yes

# [Item]
# Label=Total de personnes décedées au cours des 6 derniers mois
# Name=HH_ENF06M1
# Start=756
# Len=2
# DataType=Numeric
# ZeroFill=Yes

# [Item]
# Label=Est ce-qu'il y a eu au moins un cas de décès  dans le ménage au cours des 6 derniers mois
# Name=EXIST_DECE
# Start=758
# Len=1
# DataType=Numeric

# merge clusters to hhint
dat <- merge(level1, hhident, by = "level_1_id")
nrow(dat) #  170403
nrow(level1)  # 170403
nrow(hhident) # 170403

# # check if many clusters not in mortality survey
# test <- dat
# test <- unique(test$hh_ea)
# test[!(test %in% mort_fem_sec01$grappe)]
# length(test[!(test %in% mort_fem_sec01$grappe)]) # 2534
# # yes.
# 
# # add label for clusters that are not in mortality survey
# dat$excluded <- ifelse(dat$hh_ea %in% mort_fem_sec01$grappe, 0, 1)
# table(dat$excluded)
# # there are a lot.
# # but how can i tell which were sampled and excluded due to being insecure?
# 
# # look at mortality variables by whether cluster was in mortality survey
# dat %>%
#   group_by(excluded) %>%
#   mutate(hh_enf06m1 = ifelse(is.na(hh_enf06m1), 0, hh_enf06m1)) %>%
#   summarise(mean(hh_enf06m1, na.rm = TRUE))

# merge on enchantillon
df_ench <- ench
df_ench$hh_ea <- as.numeric(df_ench$GRAPPE)
length(unique(df_ench$hh_ea)) # 604 sampled. some are dropped de to insecuirty
dat <- merge(dat, df_ench, by = "hh_ea")
nrow(dat) # 32339
table(dat$Accessibilite)
# Accessible Insecurite 
# 27101       5238 

# Insecurity by survey design ---------------------------------------------

# insecure households
dat_tab <- dat %>%
  mutate(across(everything(), as.character)) %>%
  select(hh_ea, LREGION, Accessibilite, 
         LStatut, TypeStrate, CStrate, LStrate,
         hh_fem1549, hh_mere1_59, hh_fille_1_11, hh_fille_12_59,
         hh_garcon_1_11, hh_garcon_12_59, hh_tot_enf1_59, hh_enf06m, hh_enf06m1, 
         exist_dece, hh_maladie, hh_lmaladie) %>%
  # select(hh_ea, LREGION, Accessibilite, 
  #        LStatut, TypeStrate, CStrate, LStrate) %>%
  rename(Region = LREGION, Accessibility = Accessibilite, Strata = LStrate, Domain = LStatut) %>%
  mutate(Accessibility = case_when(
            Accessibility == "Insecurite" ~ "Insecure",
            TRUE ~ Accessibility),
        Strata = case_when(
          Strata == "Aires de sante ou 40 % ou moins de la population se trouvent a plus de 5 km du CSCOM" ~ "Rural w <40% living >5km from CSCOM",
          Strata == "Aires de sante ou plus de 40 % de la population vivent a plus de 5 km du CSCOM" ~ "Rural w >40% living >5km from CSCOM",
          Strata == "Autres villes urbaines (petite ville)" ~ "Small towns",
          Strata == "Les capitales régionales" ~ "Regional capitals",
          Strata == "Les capitales regionales" ~ "Regional capitals",
          TRUE ~ Strata
        ),
        Domain = case_when(
          Domain == "Zones du programme" ~ "Treatment",
          Domain == "Zones de comparaison" ~ "Comparison",
          TRUE ~ Domain
        ))
# households per region
ft1 <- dat_tab %>%
  group_by(Region) %>%
  summarise(n = n()) %>%
  pivot_wider(
    names_from = c(Region),
    values_from = n,
  ) %>%
  mutate(Total = as.integer(rowSums(.))) %>%
  flextable() %>%
  fontsize(size = 9, part = "all") %>%
  colformat_double(big.mark = ",", na_str = "N/A") %>%
  flextable::set_table_properties(layout = "autofit") %>%
  theme_booktabs() 

# insecurity broken down by region
t_all <- dat_tab %>%
  group_by(Region, Accessibility) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(Accessibility, Region, fill = list(n = 0)) %>%
  group_by(Region) %>%
  mutate(total = sum(n), per = sprintf("%.1f", round(n/total*100, 1))) %>% 
  mutate(combo = paste0(scales::comma(n), " (", per, ")")) %>%
  select(-c(n, per, total)) %>%
  pivot_wider(
    names_from = c(Region, Accessibility),
    values_from = combo,
  ) %>% 
  select(order(names(.)))  %>%
  mutate(variable = "All",
         value = "All") %>%
  select(variable, value, everything())
# insecurity broken down by domain
t_domain <- dat_tab %>%
  group_by(Region, Accessibility, Domain) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(Accessibility, Domain, Region, fill = list(n = 0)) %>%
  group_by(Region, Domain) %>%
  mutate(total = sum(n), per = sprintf("%.1f", round(n/total*100, 1))) %>% 
  mutate(combo = paste0(scales::comma(n), " (", per, ")")) %>%
  select(-c(n, per, total)) %>%
  pivot_wider(
    names_from = c(Region, Accessibility),
    values_from = combo,
  ) %>% 
  arrange(Domain) %>%
  select(order(names(.)))  %>%
  mutate(variable = "Domain") %>%
  rename(value = Domain) %>%
  select(variable, value, everything())
# insecurity broken down by strata
t_strata <- dat_tab %>%
  group_by(Region, Accessibility, Strata) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(Accessibility, Strata, Region, fill = list(n = 0)) %>%
  group_by(Region, Strata) %>%
  mutate(total = sum(n), per = sprintf("%.1f", round(n/total*100, 1))) %>% 
  mutate(combo = paste0(scales::comma(n), " (", per, ")")) %>%
  select(-c(n, per, total)) %>%
  pivot_wider(
    names_from = c(Region, Accessibility),
    values_from = combo,
  ) %>% 
  arrange(Strata) %>%
  select(order(names(.)))  %>%
  mutate(variable = "Strata") %>%
  rename(value = Strata) %>%
  select(variable, value, everything())
df <- rbind(t_all, t_domain, t_strata)
# create headers
col_names <- names(df)[-c(1,2)]  # exclude first column (Strata)
region <- gsub("_.*", "", col_names)  # get region part
access <- gsub(".*_", "", col_names)  # get access part
header_df <- data.frame(
  col_keys = col_names,
  Region = region,
  Access = access,
  stringsAsFactors = FALSE
)
# create flextable for insecurity by admin regions
ft2 <- flextable(df) %>%
  set_header_df(mapping = header_df, key = "col_keys") %>%
  merge_h(part = "header") %>%
  merge_v(j = ~variable) %>%  # vertical merge for grouping rows
  align(align = "center", part = "header") %>%
  align(j = 2, align = "left", part = "body") %>%
  fontsize(size = 9, part = "all") %>%
  flextable::set_table_properties(layout = "autofit") %>%
  theme_booktabs()

# Insecurity by household composition -------------------------------------

# insecurity by numeric variables
table(dat_tab$hh_enf06m1, useNA = "always")
table(dat_tab$exist_dece, useNA = "always") # only 100 deaths reported
table(dat_tab$hh_maladie, useNA = "always") # hospitalized in past six months
t_num <- dat_tab %>%
  select(Region, Accessibility,
         hh_fem1549, hh_mere1_59, 
         #hh_fille_1_11, hh_fille_12_59, hh_garcon_1_11, hh_garcon_12_59, 
         hh_tot_enf1_59, hh_enf06m, hh_enf06m1) %>%
  pivot_longer(
    cols = c(hh_fem1549, hh_mere1_59, 
             #hh_fille_1_11, hh_fille_12_59, hh_garcon_1_11, hh_garcon_12_59, 
             hh_tot_enf1_59, hh_enf06m, hh_enf06m1),
    names_to = "variable", values_to = "value"
  ) %>% 
  group_by(Region, Accessibility, variable) %>% 
  mutate(variable = case_when(
    variable == "hh_enf06m" ~ "N children 01-06m",
    variable == "hh_enf06m1" ~ "N people (children?) died in past 6m",
    variable == "hh_fem1549" ~ "N women 15-49y",
    variable == "hh_mere1_59" ~ "N mothers of children <5y",
    variable == "hh_tot_enf1_59" ~ "N children 01-59m",
    #variable == "hh_fille_1_11" ~ "N girls 1-11m",
    #variable == "hh_fille_12_59" ~ "N girls 12-59m",
    #variable == "hh_garcon_1_11" ~ "N boys 1-11m",
    #variable == "hh_garcon_12_59" ~ "N boys 12-59m",
    TRUE ~ variable
  )) %>%
  summarise(avg = mean(as.numeric(as.character(value)), na.rm = TRUE)) %>%
  mutate(avg = sprintf("%.2f", round(avg, 2))) %>%
  pivot_wider(
    names_from = c(Region, Accessibility),
    values_from = avg,
  ) %>% 
  rename(value = variable) %>%
  arrange(value) %>%
  select(order(names(.)))  %>%
  mutate(variable = "Household") %>%
  select(variable, value, everything())
# insecurity by categorical variables
t_cat <- dat_tab %>%
  select(Region, Accessibility, exist_dece, hh_maladie) %>%
  pivot_longer(
    cols = c(exist_dece, hh_maladie),
    names_to = "variable"
  ) %>%
  mutate(value = case_when(
    value == 2 ~ "No",
    value  == 1 ~ "Yes",
    is.na(value) ~ "Missing",
    TRUE ~ value
    ),
    variable = case_when(
      variable == "exist_dece" ~ "Household death in past 6m",
      variable == "hh_maladie" ~ "Household sickness or hospitalization",
      TRUE ~ variable
    )) %>%
  #filter(Region == "Koulikoro" & Accessibility == "In") %>% View
  group_by(Region, Accessibility, variable, value) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  complete(Accessibility, Region, variable, value, fill = list(n = 0)) %>%
  group_by(Accessibility, Region, variable) %>%
  mutate(total = sum(n), 
         per = n/total*100,
         per = ifelse(is.na(per), 0, per),
         per = sprintf("%.1f", round(per, 1))) %>% 
  mutate(combo = paste0(scales::comma(n), " (", per, ")")) %>%
  select(-c(n, per, total)) %>%
  pivot_wider(
    names_from = c(Region, Accessibility),
    values_from = combo,
  ) %>% 
  select(order(names(.)))  %>%
  select(variable, value, everything()) 
df2 <- rbind(t_cat, t_num)
df2$value_order <- 1:nrow(df2)
df2$value_order[df2$variable == "Household"] <- df2$value_order[df2$variable == "Household"] + 1
df2$value_order[df2$value == "N mothers of children <5y"] <- df2$value_order[df2$value == "N mothers of children <5y"] + 90
df2 <- df2[order(df2$value_order),]
df2[is.na(df2)] <- "0"
df2$value_order <- NULL

# create flextable for insecurity by admin regions
ft3 <- flextable(df2) %>%
  set_header_df(mapping = header_df, key = "col_keys") %>%
  merge_h(part = "header") %>%
  merge_v(j = ~variable) %>%  # vertical merge for grouping rows
  align(align = "center", part = "header") %>%
  align(j = 2, align = "left", part = "body") %>%
  fontsize(size = 9, part = "all") %>%
  width(j = 1, width = 0.75) %>%
  width(j = 2:ncol(df2), width = 0.85) %>%  # Adjust the other columns
  flextable::set_table_properties(layout = "fixed") %>%  # Optional: Use fixed layout if needed
  #flextable::set_table_properties(layout = "autofit") %>%  # Autofit other columns
  theme_booktabs()

# Export table to Word ----------------------------------------------------

# Define landscape layout
landscape_section <- prop_section(
  page_size = page_size(orient = "landscape", width = 11, height = 8.5),
  page_margins = page_mar(top = 0.5, bottom = 0.5, left = 0.5, right = 0.5),
  type = "continuous"
)

# # export all tables to Word doc
# save_as_docx(
#   "Households sampled from mapping" = ft1,
#   "Accessibility by survey region, domain, strata" = ft2,
#   "Accessibility by household characteristics" = ft3,
#   path = "./gen/mapping/audit/tables-accessibility.docx",
#   pr_section = landscape_section
# )

# export with page break
ft1 <- set_caption(ft1, caption = "Households sampled from mapping")
ft2 <- set_caption(ft2, caption = "Accessibility by survey region, domain, strata")
ft3 <- set_caption(ft3, caption = "Accessibility by household characteristics")
doc <- read_docx() %>%
  body_set_default_section(value = landscape_section) %>%
  body_add_flextable(value = ft1) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(value = ft2) %>%
  body_add_break() %>%
  body_add_flextable(value = ft3)
# Save the document
print(doc, target = "./gen/mapping/audit/tables-accessibility.docx")

