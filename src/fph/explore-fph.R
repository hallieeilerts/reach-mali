################################################################################
#' @description Explore FPH
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

# Résultat de la grossesse
table(dat$q216, useNA = "always")
# Résultat de la grossesse (final)
table(dat$q223, useNA = "always")
# some change here
# far more NA's in final result
# Look when q223 is missing and q216 is not
#View(subset(dat, is.na(q223) & !is.na(q216)))
# looks like pregnancy outcome was sometimes reported in q218 which is supposed to be babies name
dat %>% 
  group_by(q218) %>%
  summarise(n = n()) %>%
  arrange(-n) %>%
  head()

# if the fausse couche written in q18
# making sure q217 (moved or cried) is not 1
nrow(subset(dat, is.na(q223) & q218 == "FAUSSE COUCHE" & (is.na(q217) | q217 == 2))) # 16
# making sure that if pregnancy duration reported, was not more than 28 weeks or 7 months
#View(subset(dat, is.na(q223) & q218 == "FAUSSE COUCHE" & (is.na(q217) | q217 == 2)))
nrow(subset(dat, is.na(q223) & q218 == "FAUSSE COUCHE" & 
              (is.na(q217) | q217 == 2) &
              ((is.na(q221u) | (q221u == 1 & q221n < 28)) |
                 (is.na(q221u) | (q221u == 2 & q221n < 8)))
)) # 16
# ok to replace q223 in these cases

# if the mortnaissance written in q18
# making sure q217 (moved or cried) is not 1
# making sure that if pregnancy duraction reported, was not less than 28 weeks or 7 months
View(subset(dat, is.na(q223) & q218 == "MORTINAISSANCE" & (is.na(q217) | q217 == 2)))
nrow(subset(dat, is.na(q223) & q218 == "MORTINAISSANCE" & 
              (is.na(q217) | q217 == 2) &
              ((is.na(q221u) | (q221u == 1 & q221n >= 28)) |
                 (is.na(q221u) | (q221u == 2 & q221n >= 8)))
)) # 7
# ok to replace q223 as stillbirth in these cases
nrow(subset(dat, is.na(q223) & q218 == "MORTINAISSANCE" & 
              (is.na(q217) | q217 == 2) &
              ((is.na(q221u) | (q221u == 1 & q221n < 28)) |
                 (is.na(q221u) | (q221u == 2 & q221n < 8)))
)) # 2
# ok to replace q223 as miscarriage in these cases
nrow(subset(dat, is.na(q223) & q218 == "AVORTEMENT" & 
              (is.na(q217) | q217 == 2) &
              ((is.na(q221u) | (q221u == 1 & q221n < 28)) |
                 (is.na(q221u) | (q221u == 2 & q221n < 8)))
)) # 4
# ok to replace q223 as avortement in these cases

# Bébé a crié, a bougé ou respiré
table(dat$q217, useNA = "always") # 1 Oui, 2 Non

# check that if the baby cried, it's always a live birth
table(dat$q217, dat$q216, useNA = "always") # Résultat de la grossesse
# Value=1;Né vivant
# Value=2;Mort né
# Value=3;Fausse-couche
# Value=4;Avortement
# Not the case...
# sometimes when the baby is said to have cried, we have mort ne and fausse-couche
# sometimes when the baby did not cry, we have live birth
table(dat$q217, dat$q223, useNA = "always") # Résultat de la grossesse (final)
# when it goes to final result, the top row is corrected but bottom isn't
# there are still some live births where baby did not cry or move
# subset those where the baby did not cry/move, but was listed as live birth. 
# look at whether child is still alive. sometimes yes.
subset(dat, q217 == 2 & q223 == 1)$q224
# really this question should only be asked for those who answered that they had a stillbirth
# not the case, was asked for several outcomes
table(subset(dat, !is.na(q217))$q216, useNA = "always") 

# when the pregnacy outcome is missing, is the child still alive
table(subset(dat, is.na(q223))$q224)
#View(subset(dat, is.na(q223)))

# Sexe de l'enfant
table(dat$q219, useNA = "always")
# 4780 missing

# L'enfant est toujours en vie
table(dat$q224, useNA = "always")

# Age au décès
subset(dat, q224 == 1 & !is.na(q228))
# in three cases, ages at death are reported for children who are still alive

# Age at death is never missing when death reported
nrow(subset(dat, q224 == 2 & is.na(q228))) # 0

# Durée de grossesse
table(dat$q221, useNA = "always")
table(dat$q221u, useNA = "always") # 1 semaines, 2 mois
table(dat$q221n, useNA = "always") # units

# Âge actuel de l'enfant
# use this variable for missing year of birth if possible
# wont help with any issues related to AOD because only asked for surviving children
table(dat$q225, useNA = "always")
table(subset(dat, is.na(q220y) | q220y == 98)$q225, useNA = "always")
# in two cases this will help

# Imputé jour de la naissance/fin de la grossesse
unique(dat$q220di) # all NA

# Indicateur de date pour/Date flag for Q220C (jour de la naissance)
unique(dat$q228f) # all NA


