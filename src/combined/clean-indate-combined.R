################################################################################
#' @description Clean interview/visit dates
#' @return None
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)
library(haven)
#' Inputs
ebm <- read_sav("./data/instat-20250623/PartB-Womens_questionnaire_EBM.sav")
################################################################################

# lower case column names
names(ebm) <- tolower(names(ebm))

# visit dates
ebm$qvdate1 <- as.Date(paste(ebm$qvyear, ebm$qvmonth, ebm$qvday, sep = "-"), format = "%Y-%m-%d")
ebm$qvdate2 <- as.Date(paste(ebm$qvyear2, ebm$qvmonth2, ebm$qvday2, sep = "-"), format = "%Y-%m-%d")
ebm$qvdate3 <- as.Date(paste(ebm$qvyear3, ebm$qvmonth3, ebm$qvday3, sep = "-"), format = "%Y-%m-%d")
# interview dates
ebm$qintdate <- as.Date(paste(ebm$qinty, ebm$qintm, ebm$qintd, sep = "-"), format = "%Y-%m-%d")

# Fill in missing qintdate when possible
# Places have an intdate even when the interview was not complete
ebm %>%
  select(wco3, qintdate, qvdate1, qvdate2, qvdate3) %>%
  filter(!is.na(qintdate)) %>% 
  group_by(wco3)%>%
  summarise(n())
# Places without an intdate
ebm %>%
  select(wco3, qintdate, qvdate1, qvdate2, qvdate3) %>%
  filter(is.na(qintdate)) #%>% View()
# Fill in qintdate with latest qvdate
ebm <- ebm %>%
  mutate(qvdates = coalesce(qvdate3, qvdate2, qvdate1)) %>%
  mutate(qintdate = dplyr::if_else(is.na(qintdate), qvdates, qintdate))
# Sometimes qvdate1 is missing when qvdate2 is recorded
subset(ebm, is.na(qvdate1) & !is.na(qvdate2))
# Those that still have missing qintdate
nrow(subset(ebm, is.na(qintdate))) # 51

# When qcorrect is NA, recode wc03
ebm <- ebm %>%
  mutate(
    wco3 = as.character(wco3),
    wco3 = case_when(
    is.na(qcorrect) ~ "C'est pas la bonne personne",
    TRUE ~ wco3 
  )) %>%
  mutate(wco3 = factor(
    wco3,
    levels = c(
      "Complet",
      "C'est pas la bonne personne",
      "Pas de membre du ménage à la maison ou pas de répondant du ménage compétent à la maison au moment de la visite",
      "Ménage entier absent pour une longue période",
      "Refus",
      "Logement vide ou adresse n’est pas celle d’un logement",
      "Logement détruit ou Autre",
      "Logement pas trouvé",
      "Partiel")
  ))
table(ebm$wco3, useNA = "always")

# Save --------------------------------------------------------------------

saveRDS(ebm, "./gen/combined/temp/w_ebm-clean.rds")

