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
table(dat$q223_aug, useNA = "always")
dat$q223_aug[is.na(dat$q223) & dat$q218 == "FAUSSE COUCHE" &
           (is.na(dat$q217) | dat$q217 == 2) &
           ((is.na(dat$q221u) | (dat$q221u == 1 & dat$q221n < 28)) |
              (is.na(dat$q221u) | (dat$q221u == 2 & dat$q221n < 8)))] <- 3
table(dat$q223_aug, useNA = "always")

# name has "MORTINAISSANCE", q217 (moved or cried) is not yes
# if pregnancy duration reported, it was not less than 28 weeks or 7 months
table(dat$q223_aug, useNA = "always")
dat$q223_aug[is.na(dat$q223) & dat$q218 == "MORTINAISSANCE" &
           (is.na(dat$q217) | dat$q217 == 2) &
           ((is.na(dat$q221u) | (dat$q221u == 1 & dat$q221n >= 28)) |
              (is.na(dat$q221u) | (dat$q221u == 2 & dat$q221n >= 8)))] <- 3
table(dat$q223_aug, useNA = "always")
dat$q223_aug[is.na(dat$q223) & dat$q218 == "MORTINAISSANCE" &
           (is.na(dat$q217) | dat$q217 == 2) &
           ((is.na(dat$q221u) | (dat$q221u == 1 & dat$q221n < 28)) |
              (is.na(dat$q221u) | (dat$q221u == 2 & dat$q221n < 8)))] <- 2
table(dat$q223_aug, useNA = "always")

# name has "AVORTEMENT", q217 (moved or cried) is not yes
# if pregnancy duration reported, it was not more than 28 weeks or 7 months
dat$q223_aug[is.na(dat$q223) & dat$q218 == "AVORTEMENT" &
           (is.na(dat$q217) | dat$q217 == 2) &
           ((is.na(dat$q221u) | (dat$q221u == 1 & dat$q221n < 28)) |
              (is.na(dat$q221u) | (dat$q221u == 2 & dat$q221n < 8)))] <- 4
table(dat$q223_aug, useNA = "always")

# if child is reported as still alive (q224 == 1), age at death (q228) should be NA
nrow(subset(dat, q224 == 1 & !is.na(q228))) # 3
# Recode q228 as NA if the child is still alive
dat$q228[dat$q224 == 1] <- NA
nrow(subset(dat, q224 == 1 & !is.na(q228))) # 0

# if the child is still alive, q223_aug should always be live birth
table(subset(dat, q224 == 1)$q223_aug, useNA = "always")
subset(dat, q224 == 1 & q223_aug != 1)
# in these two cases, both children listed as living with the respondent (q226 == 1) and they dont have a age at death (q228)
# recode as live birth
table(dat$q223_aug, useNA = "always")
dat$q223_aug[dat$q224 == 1] <- 1
table(dat$q223_aug, useNA = "always")

# if the child is not still alive, q223_aug should always be live birth
table(subset(dat, q224 == 2)$q223_aug, useNA = "always")
subset(dat, q224 == 2 & q223_aug != 1)
# in this one case, the child does not live with the respondent and has a date of death of 2 years
# recode as live birth
table(dat$q223_aug, useNA = "always")
dat$q223_aug[dat$q224 == 2] <- 1
table(dat$q223_aug, useNA = "always")

# check that pregnancy outcome is always live birth when there is a response for is the child still alive
nrow(subset(dat, q224 == 2 & q223_aug != 1)) # 0
nrow(subset(dat, q224 == 1 & q223_aug != 1)) # 0 
# and pregnancy outcome is always not a live birth when the child still alive question wasn't asked
table(subset(dat, is.na(q224))$q223_aug) # 0 

# check if age of death is (q228) is ever missing when units of age at death (q228u) are reported
nrow(subset(dat, is.na(q228) & !is.na(q228u))) # 2
subset(dat, is.na(q228) & !is.na(q228u))
# both are listed as still alive (q224 == 1) 
# in one case, is living with respondent (q226 == 1)
# recode q228u as missing
dat$q228u[is.na(dat$q228) & !is.na(dat$q228u)] <- NA
# check if age of death is (q228) is ever missing when values of age at death (q228n) are reported
nrow(subset(dat, is.na(q228) & !is.na(q228n))) # 3
subset(dat, is.na(q228) & !is.na(q228n))
# all are listed as still alive (q224 == 1) 
# recode q228n as missing
dat$q228n[is.na(dat$q228) & !is.na(dat$q228n)] <- NA

# Add factor labels
# Choosing not to do this now
# dat <- dat %>%
#   mutate(q216 = case_when(
#     q216 == 1 ~ "Né vivant",
#     q216 == 2 ~ "Mort né",
#     q216 == 3 ~ "Fausse-couche",
#     q216 == 4 ~ "Avortement",
#     TRUE ~ NA_character_),    
#     q216 = factor(
#       q216,
#       levels = c(
#         "Né vivant",
#         "Mort né",
#         "Fausse-couche",
#         "Avortement")
#     )
#   ) %>%
#   mutate(q223 = case_when(
#     q223 == 1 ~ "Né vivant",
#     q223 == 2 ~ "Mort né",
#     q223 == 3 ~ "Fausse-couche",
#     q223 == 4 ~ "Avortement",
#     TRUE ~ NA_character_),    
#     q223 = factor(
#       q223,
#       levels = c(
#         "Né vivant",
#         "Mort né",
#         "Fausse-couche",
#         "Avortement")
#     )
#   ) %>%
#   mutate(q223_aug = case_when(
#     q223_aug == 1 ~ "Né vivant",
#     q223_aug == 2 ~ "Mort né",
#     q223_aug == 3 ~ "Fausse-couche",
#     q223_aug == 4 ~ "Avortement",
#     TRUE ~ NA_character_),    
#     q223_aug = factor(
#       q223_aug,
#       levels = c(
#         "Né vivant",
#         "Mort né",
#         "Fausse-couche",
#         "Avortement")
#     )
#   ) %>%
#   mutate(q217 = case_when(
#     q217 == 1 ~ "Oui",
#     q217 == 2 ~ "Non",
#     TRUE ~ NA_character_),    
#     q217 = factor(
#       q217,
#       levels = c(
#         "Oui",
#         "Non")
#     )
#   ) %>%
#   mutate(q221u = case_when(
#     q221u == 1 ~ "Semaines",
#     q221u == 2 ~ "Mois",
#     TRUE ~ NA_character_),    
#     q221u = factor(
#       q221u,
#       levels = c(
#         "Semaines",
#         "Mois")
#     )
#   )

table(dat$q216, useNA = "always") # Résultat de la grossesse
table(dat$q217, useNA = "always") # Bébé a crié, a bougé ou respiré
table(dat$q223, useNA = "always") # Résultat de la grossesse (final)
table(dat$q223_aug, useNA = "always") # Résultat de la grossesse (final), with corrections from name
table(dat$q221u, useNA = "always") # Durée de grossesse (unité)

# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/fph/temp/fph-clean.rds")
