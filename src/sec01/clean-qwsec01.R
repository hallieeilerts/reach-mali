################################################################################
#' @description Clean qwsec01
#' @return Cleaned qsecover file
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(fitdistrplus)
#' Inputs
dat <- read.csv("./data/reach_qwsec01_vue.csv")
################################################################################

# Add factor labels
dat <- dat %>%
  mutate(q112 = case_when(
    q112 == 1 ~ "Très bon",
    q112 == 2 ~ "Bon",
    q112 == 3 ~ "Moyennement bon",
    q112 == 4 ~ "Mauvais",
    q112 == 5 ~ "Très mauvais",
    TRUE ~ NA_character_),    
    q112 = factor(
      q112,
      levels = c(
        "Très bon",
        "Bon",
        "Moyennement bon",
        "Mauvais",
        "Très mauvais")
    )
  ) %>%
  mutate(q113 = case_when(
    q113 == 1 ~ "Oui",
    q113 == 2 ~ "Non",
    TRUE ~ NA_character_)
  ) %>%
  mutate(q114 = case_when(
    q114 == 1 ~ "Fondamental 1er Cycle",
    q114 == 2 ~ "Fondamental 2nd Cycle",
    q114 == 3 ~ "Secondaire",
    q114 == 4 ~ "Mauvais",
    q114 == 5 ~ "Supérieur",
    TRUE ~ NA_character_),    
    q114 = factor(
      q114,
      levels = c(
        "Fondamental 1er Cycle",
        "Fondamental 2nd Cycle",
        "Moyennement bon",
        "Secondaire",
        "Supérieur")
    )
  ) %>%
  mutate(q122 = case_when(
    q122 == 1 ~ "Oui",
    q122 == 2 ~ "Non",
    TRUE ~ NA_character_)
  ) %>%
  mutate(
    q131 = case_when(
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
      q131 == 12 ~ "Arabe",
      q131 == 16 ~ "Autre ethnie Malienne",
      q131 == 21 ~ "Pays CEDEAO",
      q131 == 22 ~ "Autres pays africains",
      q131 == 23 ~ "Autres nationalités",
      TRUE ~ NA_character_
    ),
    q131 = factor(
      q131,
      levels = c(
        "Bambara",
        "Malinke","Peuhl","Sarakole","Kassonke","Sonraï","Dogon",
        "Touareg","Sénoufo", "Bobo", "Bozo", "Arabe", "Autre ethnie Malienne",
        "Pays CEDEAO", "Autres pays africains","Autres nationalités"
      )
    )
  ) %>%
  mutate(q132 = case_when(
    q132 == 1 ~ "Marié ou vivant ensemble",
    q132 == 2 ~ "Divorcé/Séparé",
    q132 == 3 ~ "Veuve",
    q132 == 4 ~ "Jamais marié et n'a jamais vécu avec quelqu'un",
    TRUE ~ NA_character_),    
    q132 = factor(
      q132,
      levels = c(
        "Marié ou vivant ensemble",
        "Divorcé",
        "Veuve",
        "Jamais marié et n'a jamais vécu avec quelqu'un")
    )
  )

# Select variables of interest (that also aren't in qsecover)
dat <- dat %>%
  dplyr::select(
    id_data_cover, 
    grappe, w_enum, w_cons, w_men, w_ind,
    q110, q110m, q110y, q111, # Date de naissance (respondent)
    q112, q113, q114, q115, q122, q131, q132 # state of health, schooling level, portable, ethnie, marriage
  )

# Save --------------------------------------------------------------------

saveRDS(dat, "./gen/sec01/temp/qwsec01-clean.rds")
