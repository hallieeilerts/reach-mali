################################################################################
#' @description Prepare qsecover
#' @return Cleaned qsecover file
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
dat <- read.csv("./data/reach_mortalite_femme_qsecover.csv")
################################################################################

# Add factor labels
dat <- dat %>%
  mutate(wco3 = case_when(
    wco3 == 1 ~ "Complet",
    wco3 == 2 ~ "Pas de membre du ménage à la maison ou pas de répondant du ménage compétent à la maison au moment de la visite",
    wco3 == 3 ~ "Ménage entier absent pour une longue période",
    wco3 == 4 ~ "Refus",
    wco3 == 5 ~ "Logement vide ou adresse n’est pas celle d’un logement",
    wco3 == 6 ~ "Logement détruit ou Autre",
    wco3 == 7 ~ "Logement pas trouvé",
    trimws(wco3) == "" ~ "Partiel",
    TRUE ~ NA_character_),    
  wco3 = factor(
    wco3,
    levels = c(
      "Complet",
      "Pas de membre du ménage à la maison ou pas de répondant du ménage compétent à la maison au moment de la visite",
      "Ménage entier absent pour une longue période",
      "Refus",
      "Logement vide ou adresse n’est pas celle d’un logement",
      "Logement détruit ou Autre",
      "Logement pas trouvé",
      "Partiel")
    )
  ) %>%
  mutate(qtype = case_when(
    qtype == 1 ~ "Urbain",
    qtype == 2 ~ "Rural",
    TRUE ~ NA_character_)
  ) %>%
  mutate(
    qlangq = case_when(
      qlangq == 1 ~ "Français",
      qlangq == 2 ~ "Bambara/Malinke",
      qlangq == 3 ~ "Sonraï/Djerma",
      qlangq == 4 ~ "Peuhl/Foulfouldé",
      qlangq == 5 ~ "Sénoufo",
      qlangq == 6 ~ "Marka/Soninké",
      qlangq == 7 ~ "Dogon",
      qlangq == 8 ~ "Minianka",
      qlangq == 9 ~ "Tamacheck",
      qlangq == 10 ~ "Bobo/Dafing",
      qlangq == 11 ~ "Bozo",
      qlangq == 96 ~ "Autre",
      TRUE ~ NA_character_
    ),
    qlangi = case_when(
      qlangi == 1 ~ "Français",
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
      qlangi == 96 ~ "Autre",
      TRUE ~ NA_character_
    ),
    qlangr = case_when(
      qlangr == 1 ~ "Français",
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
      qlangr == 96 ~ "Autre",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(across(
    c(qlangq, qlangi, qlangr),
    ~factor(.x, levels = c(
      "Français",
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
      "Autre"
    ))
  ))

# Create dates
dat$qvdate1 <- as.Date(paste(dat$qvyear, dat$qvmonth, dat$qvday, sep = "-"), format = "%Y-%m-%d")
dat$qvdate2 <- as.Date(paste(dat$qvyear2, dat$qvmonth2, dat$qvday2, sep = "-"), format = "%Y-%m-%d")
dat$qvdate3 <- as.Date(paste(dat$qvyear3, dat$qvmonth3, dat$qvday3, sep = "-"), format = "%Y-%m-%d")
dat$qintdate <- as.Date(paste(dat$qinty, dat$qintm, dat$qintd, sep = "-"), format = "%Y-%m-%d")

# Fill in missing qintdate when possible
# Places have an intdate even when the interview was not complete
dat %>%
  select(wco3, qintdate, qvdate1, qvdate2, qvdate3) %>%
  filter(!is.na(qintdate)) %>% 
  group_by(wco3)%>%
  summarise(n())
# Places without an intdate
dat %>%
  select(wco3, qintdate, qvdate1, qvdate2, qvdate3) %>%
  filter(is.na(qintdate)) #%>% View()
# Fill in qintdate with latest qvdate
dat <- dat %>%
  mutate(qvdates = coalesce(qvdate3, qvdate2, qvdate1)) %>%
  mutate(qintdate = dplyr::if_else(is.na(qintdate), qvdates, qintdate))
# Sometimes qvdate1 is missing when qvdate2 is recorded
subset(dat, is.na(qvdate1) & !is.na(qvdate2))
# Those that still have missing qintdate
nrow(subset(dat, is.na(qintdate))) # 60

# Create new number of interviews variable
# Not going to do right now. But number of visits doesn't always add up to qvdates

# When qcorrect is NA, recode wc03
dat <- dat %>%
  mutate(wco3 = case_when(
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
  
table(dat$wco3, useNA = "always")

# Interview result for those with missing intdate
table(subset(dat, is.na(qintdate))$wco3, useNA = "always")
# 56 C'est pas la bonne personne 
# 4 just NA

# Select variables of interest
dat <- dat %>%
  select(
    qsecover_id, level_1_id,
    qregion, qlregion, qcercle, qlcercle, qdistrict,
    wi3, wli3, wi4, wi5, # commune, village, aire de santé
    qtype, # Type de résidence (num)
    qintdate, qvdate1, qvdate2, qvdate3, 
    qlangq, qlangi, qlangr, # Langue du questionnaire, interview, de l’enquêté(e)
    qteam, qintnum, # Numero equipe, numero interviewer ménage
    wco3, qcorrect
  )


nrow(dat) == length(unique(dat$level_1_id))

# Save --------------------------------------------------------------------

saveRDS(dat, "./gen/sec01/temp/qsecover-clean.rds")
