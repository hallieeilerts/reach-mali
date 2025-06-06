################################################################################
#' @description Clean FPH
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
dat <- read.csv("./data/reach_mortalite_femme_qwsec2b.csv")
################################################################################

# Fill in missing q223 with q216 when possible
# augmented q223
dat$q223_aug <- dat$q223

# name has "FAUSSE COUCHE", q217 (moved or cried) is not yes
# if pregnancy duration reported, it was not more than 28 weeks or 7 months
dat$q223_aug[is.na(dat$q223) & dat$q218 == "FAUSSE COUCHE" &
           (is.na(dat$q217) | dat$q217 == 2) &
           ((is.na(dat$q221u) | (dat$q221u == 1 & dat$q221n < 28)) |
              (is.na(dat$q221u) | (dat$q221u == 2 & dat$q221n < 8)))] <- 3

# name has "MORTINAISSANCE", q217 (moved or cried) is not yes
# if pregnancy duration reported, it was not less than 28 weeks or 7 months
dat$q223_aug[is.na(dat$q223) & dat$q218 == "MORTINAISSANCE" &
           (is.na(dat$q217) | dat$q217 == 2) &
           ((is.na(dat$q221u) | (dat$q221u == 1 & dat$q221n >= 28)) |
              (is.na(dat$q221u) | (dat$q221u == 2 & dat$q221n >= 8)))] <- 3
dat$q223_aug[is.na(dat$q223) & dat$q218 == "MORTINAISSANCE" &
           (is.na(dat$q217) | dat$q217 == 2) &
           ((is.na(dat$q221u) | (dat$q221u == 1 & dat$q221n < 28)) |
              (is.na(dat$q221u) | (dat$q221u == 2 & dat$q221n < 8)))] <- 2

# name has "AVORTEMENT", q217 (moved or cried) is not yes
# if pregnancy duration reported, it was not more than 28 weeks or 7 months
dat$q223_aug[is.na(dat$q223) & dat$q218 == "AVORTEMENT" &
           (is.na(dat$q217) | dat$q217 == 2) &
           ((is.na(dat$q221u) | (dat$q221u == 1 & dat$q221n < 28)) |
              (is.na(dat$q221u) | (dat$q221u == 2 & dat$q221n < 8)))] <- 4


# Add factor labels
dat <- dat %>%
  mutate(q216 = case_when(
    q216 == 1 ~ "Né vivant",
    q216 == 2 ~ "Mort né",
    q216 == 3 ~ "Fausse-couche",
    q216 == 4 ~ "Avortement",
    TRUE ~ NA_character_),    
    q216 = factor(
      q216,
      levels = c(
        "Né vivant",
        "Mort né",
        "Fausse-couche",
        "Avortement")
    )
  ) %>%
  mutate(q223 = case_when(
    q223 == 1 ~ "Né vivant",
    q223 == 2 ~ "Mort né",
    q223 == 3 ~ "Fausse-couche",
    q223 == 4 ~ "Avortement",
    TRUE ~ NA_character_),    
    q223 = factor(
      q223,
      levels = c(
        "Né vivant",
        "Mort né",
        "Fausse-couche",
        "Avortement")
    )
  ) %>%
  mutate(q223_aug = case_when(
    q223_aug == 1 ~ "Né vivant",
    q223_aug == 2 ~ "Mort né",
    q223_aug == 3 ~ "Fausse-couche",
    q223_aug == 4 ~ "Avortement",
    TRUE ~ NA_character_),    
    q223_aug = factor(
      q223_aug,
      levels = c(
        "Né vivant",
        "Mort né",
        "Fausse-couche",
        "Avortement")
    )
  ) %>%
  mutate(q217 = case_when(
    q217 == 1 ~ "Oui",
    q217 == 2 ~ "Non",
    TRUE ~ NA_character_),    
    q217 = factor(
      q217,
      levels = c(
        "Oui",
        "Non")
    )
  ) %>%
  mutate(q221u = case_when(
    q221u == 1 ~ "Semaines",
    q221u == 2 ~ "Mois",
    TRUE ~ NA_character_),    
    q221u = factor(
      q221u,
      levels = c(
        "Semaines",
        "Mois")
    )
  )

table(dat$q216, useNA = "always")
table(dat$q217, useNA = "always")
table(dat$q223, useNA = "always")
table(dat$q223_aug, useNA = "always")
table(dat$q221u, useNA = "always")

# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/fph/temp/fph-clean.rds")
