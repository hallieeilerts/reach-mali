################################################################################
#' @description Checking that mortality calculations from combined file are similar to other files
#' yes. there are a few more missing observations in these files because didn't have interview date.
#' mortality rates at national, regional, and area level are version similar though.
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(purrr)
library(survival)
library(readxl)
#' Inputs
source("./src/utils.R")
dat <- readRDS("./gen/combined/temp/fph-impute-dob.rds")
# sampling weights
wt1 <- read_excel("./data/instat-20250623/Poids_Enquete_Base_Mortalite.xlsx", sheet = "Poidsformule")
wt2 <- read_excel("./data/instat-20250623/Poids_Enquete_Base_Mortalite.xlsx", sheet = "Poidsvf")
################################################################################

wt1 <- wt1[, c("GRAPPE", "CStrate")]
wt2 <- wt2[, c("GRAPPE", "Lstatut", "Poids normalisé des femmes de 15-49 ans")]
wt <- merge(wt1, wt2, by = "GRAPPE")
names(wt) <- c("grappe", "strate", "area", "wt")
wt$strate <- as.numeric(wt$strate)
wt$grappe <- as.numeric(wt$grappe)

# Merge on weights --------------------------------------------------------

nrow(dat) # 79052
dat <- merge(dat, wt, by.x = "w_grappe", by.y = "grappe")
nrow(dat) # 79052
# all have weights
nrow(subset(dat, is.na(w_grappe))) # 0
nrow(subset(dat, is.na(wt))) # 0

#  Set exposure -----------------------------------------------------------

# Only keep live births
dat <- subset(dat, q223_aug == 1)
nrow(dat) # 74340

# There are no NA's event
table(dat$event, useNA = "always")
# When event == 1, aad_unit and aad_value always reported
nrow(subset(dat, event == 1 & (is.na(aad_unit) | is.na(aad_value))))

# Exposure when date of birth = date of survey 
dat$select <- dat$event == 0 & dat$dob_dec == dat$v008_dec
set.seed(1)
dat$expo <- NA
dat$expo[dat$select == T] <-  0

# Exposure when date of birth != date of survey 
dat$select <- dat$event == 0 & dat$dob_dec != dat$v008_dec
dat$expo[dat$select == T] <-  dat$v008_dec[dat$select == T] - dat$dob_dec[dat$select == T] 

# Exposure for non-surviving children

# set a random number between 0 and 1 for uniform imputation of days
set.seed(777)
dat$random <- NA
dat$random[dat$event == 1] <- runif(nrow(dat[dat$event == 1,]), min = 0,  max = 1)

# Turn discreate age at death values into continuous values to reduce effects of digit preference
# adds less than 1 day to ages at death reported in days, adds somewhere within 1 month to month, places year death within the same year
# Age at death in person-years with imputation for random days
dat$expo[dat$event == 1 & dat$aad_unit == 1] <- (as.numeric(dat$aad_value[dat$event == 1 & dat$aad_unit == 1]) + dat$random[dat$event == 1 & dat$aad_unit == 1])/365.25   
dat$expo[dat$event == 1 & dat$aad_unit == 2] <- (as.numeric(dat$aad_value[dat$event == 1 & dat$aad_unit == 2]) + dat$random[dat$event == 1 & dat$aad_unit == 2])/12          
dat$expo[dat$event == 1 & dat$aad_unit == 3] <- (as.numeric(dat$aad_value[dat$event == 1 & dat$aad_unit == 3]) + dat$random[dat$event == 1 & dat$aad_unit == 3])

gapu5m_age <- c(7,14, 21, 28,
                seq(60.8750, 365.25,  30.4375),
                seq(365.25 + 365.25/4 , 365.25*2,  365.25/4),
                seq(365.25*3 , 365.25*5,  365.25))

reach_age <- c(28, 365.25*5)

# exposure is never missing
nrow(subset(dat, is.na(expo))) # 0
#hist(dat$expo)

# Entire pop --------------------------------------------------------------

# REACH age groups
reach1 <- fn_calcmort(dat, ages = c(365.25*5), tips = c(0, 5, 10, 15))
reach2 <- fn_calcmort(dat, ages = c(28, 365.25*5), tips = c(0, 5, 10, 15))
reach3 <- fn_calcmort(dat, ages = c(28, 365.25), tips = c(0, 5, 10, 15))
reach3$rates <- subset(reach3$rates, age_d == 28)
reach4 <- fn_calcmort(dat, ages = c(365.25, 365.25*5), tips = c(0, 5, 10, 15))
reach4$rates <- subset(reach4$rates, age_y == 1)
reach5 <- fn_calcmort(dat, ages = c(365.25), tips = c(0, 5, 10, 15))
reach_rates_all_tips <- rbind(reach1$rates, reach2$rates, reach3$rates, reach4$rates, reach5$rates)

# Region ------------------------------------------------------------------

groups <- dat %>% group_by(e_lregion) %>% group_split()
qlregions <- dat %>% group_by(e_lregion) %>% group_keys() %>% pull(e_lregion)

# REACH

results_byvar <- map(groups, ~ fn_calcmort(.x, ages = c(365.25*5), tips = c(0, 5, 10, 15)))
names(results_byvar) <- qlregions
reach1 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))

results_byvar <- map(groups, ~ fn_calcmort(.x, ages = c(28, 365.25*5), tips = c(0, 5, 10, 15)))
names(results_byvar) <- qlregions
reach2 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))

results_byvar <- map(groups, ~ fn_calcmort(.x, ages = c(28, 365.25), tips = c(0, 5, 10, 15)))
names(results_byvar) <- qlregions
reach3 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
reach3 <- subset(reach3, age_d == 28)

results_byvar <- map(groups, ~ fn_calcmort(.x, ages =  c(365.25, 365.25*5), tips = c(0, 5, 10, 15)))
names(results_byvar) <- qlregions
reach4 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
reach4 <- subset(reach4, age_y == 1)

results_byvar <- map(groups, ~ fn_calcmort(.x, ages =  c(365.25), tips = c(0, 5, 10, 15)))
names(results_byvar) <- qlregions
reach5 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))

reach_rates_reg_tips <- rbind(reach1, reach2, reach3, reach4, reach5)

subset(reach1, cut_time == "0-4")


# area --------------------------------------------------------------------

groups <- dat %>% group_by(area) %>% group_split()
qtypes <- dat %>% group_by(area) %>% group_keys() %>% pull(area)
# Apply the function by qtype

# REACH

results_byvar <- map(groups, ~ fn_calcmort(.x, ages = c(365.25*5), tips = c(0, 5, 10, 15)))
names(results_byvar) <- qtypes
reach1 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))

results_byvar <- map(groups, ~ fn_calcmort(.x, ages = c(28, 365.25*5), tips = c(0, 5, 10, 15)))
names(results_byvar) <- qtypes
reach2 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))

results_byvar <- map(groups, ~ fn_calcmort(.x, ages = c(28, 365.25), tips = c(0, 5, 10, 15)))
names(results_byvar) <- qtypes
reach3 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
reach3 <- subset(reach3, age_d == 28)

results_byvar <- map(groups, ~ fn_calcmort(.x, ages =  c(365.25, 365.25*5), tips = c(0, 5, 10, 15)))
names(results_byvar) <- qtypes
reach4 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
reach4 <- subset(reach4, age_y == 1)

results_byvar <- map(groups, ~ fn_calcmort(.x, ages =  c(365.25), tips = c(0, 5, 10, 15)))
names(results_byvar) <- qtypes
reach5 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))

reach_rates_area_tips <- rbind(reach1, reach2, reach3, reach4, reach5)

subset(reach1, cut_time == "0-4")

