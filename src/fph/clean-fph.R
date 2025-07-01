################################################################################
#' @description Clean FPH
#' @return dataset with cleaned pregnancy outcome variable (q223_aug),
#' recoding of nonsensical values (e.g., if child is still alive and living with respondent, age of death recoded to NA)
#' (e.g., if child is still alive, pregnancy outcome should be live birth)
#' no values dropped
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
dat <- read.csv("./data/reach_mortalite_femme_qwsec2b.csv")
################################################################################

# create augmented q223
dat$q223_aug <- dat$q223

# fill in missing q223 with q216 when possible

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

# fill in q223_aug with q216 when it is missing
# make sure that the record has q224 reported (whether child is still alive) if q216 is a live birth
nrow(subset(dat, is.na(q223_aug))) # 336
nrow(subset(dat, is.na(q223_aug) & !is.na(q216))) # 301
dat$q223_aug[is.na(dat$q223) & !is.na(dat$q216) & !(dat$q216 == 1 & is.na(dat$q224))] <- dat$q216[is.na(dat$q223) & !is.na(dat$q216)& !(dat$q216 == 1 & is.na(dat$q224))]
nrow(subset(dat, is.na(q223_aug))) # 320
# this only helped fill in 336-320 = 16
# most of the time when q223 is missing and q216 is reported the record is still missing crucial values like q224
# so not worth filling these ones in
# View(subset(dat, is.na(q223_aug) & !is.na(q216)))

# The ones that still have missing pregnancy outcomes have missing in almost all other variables
# checking missingness of just one important variable -- q224 is child still alive
sum(subset(dat, is.na(q223_aug))$q224, na.rm = TRUE) # 0

# if child is reported as still alive (q224 == 1), age at death (q228) should be NA
nrow(subset(dat, q224 == 1 & !is.na(q228))) # 3
# Recode q228 as NA if the child is still alive
dat$q228[dat$q224 == 1] <- NA
nrow(subset(dat, q224 == 1 & !is.na(q228))) # 0

# if the child is still alive, q223_aug should always be live birth
table(subset(dat, q224 == 1)$q223_aug, useNA = "always")
nrow(subset(dat, q224 == 1 & q223_aug != 1)) # 2
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
nrow(subset(dat, is.na(q224) & q223_aug == 1)) # 0
# q223_aug is still missing in 320 cases
#View(subset(dat, is.na(q223_aug)))
# it is reported in q216 for many of these. 
# however it seems the value was not transferred over to q223 because of missing values in other crucial variables
nrow(subset(dat, is.na(q223_aug))) # 320

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

# check that length of pregnancy is always longer than 7 months when stillbirth
table(subset(dat, q223_aug == 2)$q221u) # reported in months in all but one case
table(subset(dat, q223_aug == 2)$q221n) # there is one case of 4 months, and one of 6 months
# recode to miscarriage
table(dat$q221u, useNA = "always")
table(dat$q221n, useNA = "always")
subset(dat, q223_aug == 2 & q221u == 2 & q221n <7)
# not sure why this returns NA rows
dat[dat$q223_aug == 2 & dat$q221u == 2 & dat$q221n < 7,]
table(dat$q223_aug, useNA = "always")
dat$q223_aug[dat$q223_aug == 2 & dat$q221u == 2 & dat$q221n < 7] <- 3
table(dat$q223_aug, useNA = "always")

table(dat$q216, useNA = "always") # Résultat de la grossesse
table(dat$q217, useNA = "always") # Bébé a crié, a bougé ou respiré
table(dat$q223, useNA = "always") # Résultat de la grossesse (final)
table(dat$q223_aug, useNA = "always") # Résultat de la grossesse (final), with corrections from name
table(dat$q221u, useNA = "always") # Durée de grossesse (unité)

# Save output(s) ----------------------------------------------------------

saveRDS(dat, "./gen/fph/temp/fph-clean.rds")
