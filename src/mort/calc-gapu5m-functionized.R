################################################################################
#' @description Mortality calculations using gapu5m age groups without confidence intervals
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
dat <- readRDS("./gen/fph/output/fph-tips.rds")
# sampling weights
wt <- read_excel("./data/instat-20250623/Poids_Enquete_Base_Mortalite.xlsx", sheet = "Poidsvf")
################################################################################

wt <- wt[, c("GRAPPE", "Poids normalisé des femmes de 15-49 ans")]
names(wt) <- c("grappe", "wt")
wt$grappe <- as.numeric(wt$grappe)

# Merge on weights --------------------------------------------------------

nrow(dat) # 79763
dat <- merge(dat, wt, by = "grappe")
nrow(dat) # 79763
# all have weights
nrow(subset(dat, is.na(grappe))) # 0
nrow(subset(dat, is.na(wt))) # 0

#  Set exposure -----------------------------------------------------------

# Only keep live births
dat <- subset(dat, q223_aug == 1)
nrow(dat) # 75006

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


# Test function -----------------------------------------------------------

tips = c(0, 5, 10, 15)

dat$cut_time <- tcut(dat$dob_dec - dat$v008_dec, -rev(tips), labels = rev(.epis_labels(tips)))

# "startage" sets the age at entry time (that is birth), so it is 0
dat$startage <- 0

# define age groups in weeks and months (but expressed in years) and labels (expressed in days)
agecut <-  c(0,7,14, 21, 28, seq(60.8750, 365.25*5,  30.4375))/365.25
agecutlab <- as.character(agecut[1:(length(agecut)-1)]*365.25)
dat$cut_age <- tcut(dat$startage, agecut, labels = agecutlab)

options(scipen=999)
#head(dat[,c("dob_dec", "dod_dec", "event", "v008_dec", "expo", "cut_time", "cut_age")])

# function that computes deaths and person-years for the defined periods and age group (use weight variable)
# pyears(surv ~ year + age, weights = tmp$v005/1000000, scale = 1, data.frame = TRUE)
calcpyears <- pyears(Surv(time = expo, event = event, type = "right") ~ cut_time + cut_age, weights = wt, scale = 1, dat, data.frame = TRUE)
PY <- calcpyears[[2]]
PY <- PY[order(PY$cut_time, PY$cut_age),]

# This won't be exact
# Because I added the tips variable just based on date of birth in years prior to survey
# Whereas the pyears function in placing dates of death in tips periods
# But just a gut check that no deaths being lost.
sum(subset(PY, cut_time == "0-4")$event) # 1332.666
sum(subset(dat, tips == "0-4")$event) # 1009
# dat %>% 
#   filter(event == 1) %>%
#   mutate(yod = floor(dob_dec)) %>%
#   filter(yod %in% 2020:2025 & aadd < 30) %>%
#   nrow()

dat <- dat %>%
  select(-c(cut_time, startage, cut_age))


# Entire pop --------------------------------------------------------------

# Tips

res <- fn_calcmort(dat, ages = gapu5m_age, tips = c(0, 5, 10, 15))
gapu5m_rates_all_tips <- res$rates
gapu5m_plot_all_tips <- res$plot

# as.data.frame(subset(gapu5m_rates_all_tips, cut_time == "0-4"))
# sum(subset(gapu5m_rates_all_tips, cut_time == "0-4")$pyears)
# sum(subset(gapu5m_rates_all_tips, cut_time == "0-4")$events)
# subset(gapu5m_rates_all_tips, age_y_up == 5)
# as.data.frame(subset(gapu5m_rates_all_tips, cut_time == "0-4" & age_y_up == 5))

# REACH age groups
reach1 <- fn_calcmort(dat, ages = c(365.25*5), tips = c(0, 5, 10, 15))
reach2 <- fn_calcmort(dat, ages = c(28, 365.25*5), tips = c(0, 5, 10, 15))
reach3 <- fn_calcmort(dat, ages = c(28, 365.25), tips = c(0, 5, 10, 15))
reach3$rates <- subset(reach3$rates, age_d == 28)
reach4 <- fn_calcmort(dat, ages = c(365.25, 365.25*5), tips = c(0, 5, 10, 15))
reach4$rates <- subset(reach4$rates, age_y == 1)
reach5 <- fn_calcmort(dat, ages = c(365.25), tips = c(0, 5, 10, 15))
reach_rates_all_tips <- rbind(reach1$rates, reach2$rates, reach3$rates, reach4$rates, reach5$rates)

#as.data.frame(subset(reach1$rates, cut_time == "0-4"))

# Residence ---------------------------------------------------------------

groups <- dat %>% group_by(qtype) %>% group_split()
qtypes <- dat %>% group_by(qtype) %>% group_keys() %>% pull(qtype)

# gapu5m

# Apply the function by qtype
results_byvar <- map(groups, ~ fn_calcmort(.x, ages = gapu5m_age, tips = c(0, 5, 10, 15)))
names(results_byvar) <- qtypes
# Extract and label
gapu5m_rates_res_tips <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
gapu5m_plot_res_tips <- imap_dfr(results_byvar, ~ mutate(.x$plot, byvar = .y))

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

reach_rates_res_tips <- rbind(reach1, reach2, reach3, reach4, reach5)

# Region ------------------------------------------------------------------

groups <- dat %>% group_by(qlregion) %>% group_split()
qlregions <- dat %>% group_by(qlregion) %>% group_keys() %>% pull(qlregion)

# gapu5m

# Apply the function by qlregion
results_byvar <- map(groups, ~ fn_calcmort(.x, ages = gapu5m_age, tips = c(0, 5, 10, 15)))
names(results_byvar) <- qlregions
# Extract and label
gapu5m_rates_reg_tips <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
gapu5m_plot_reg_tips <- imap_dfr(results_byvar, ~ mutate(.x$plot, byvar = .y))

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

# Combine -----------------------------------------------------------------

# Combine all gapu5m rates
gapu5m_rates_all_tips$byvar <- "All"
gapu5m_rates_all_tips$type <- "All"
gapu5m_rates_res_tips$type <- "Residence"
gapu5m_rates_reg_tips$type <- "Region"
gapu5m_rates <- rbind(gapu5m_rates_all_tips,
                      gapu5m_rates_res_tips,
                      gapu5m_rates_reg_tips)
gapu5m_rates <- gapu5m_rates %>%
  arrange(byvar, cut_time, age_d)


# Combine all gapu5m plot rates
gapu5m_plot_all_tips$byvar <- "All"
gapu5m_plot_all_tips$type <- "All"
gapu5m_plot_res_tips$type <- "Residence"
gapu5m_plot_reg_tips$type <- "Region"
gapu5m_plot <- rbind(gapu5m_plot_all_tips,  
                     gapu5m_plot_res_tips,
                      gapu5m_plot_reg_tips)
gapu5m_plot <- gapu5m_plot %>%
  arrange(byvar, cut_time, age_d)

# Combine all reach rates
reach_rates_all_tips$byvar <- "All"
reach_rates_all_tips$type <- "All"
reach_rates_res_tips$type <- "Residence"
reach_rates_reg_tips$type <- "Region"
reach_rates <- rbind(reach_rates_all_tips,
                     reach_rates_res_tips, 
                     reach_rates_reg_tips)
reach_rates$agegrp <- "q0to5y"
reach_rates$agegrp[reach_rates$age_y == 0 & reach_rates$n_d == 28] <- "q0to28d"
reach_rates$agegrp[reach_rates$age_d == 28 & reach_rates$age_y_up == 5] <- "q1to59m"
reach_rates$agegrp[reach_rates$age_d == 28 & reach_rates$age_y_up == 1] <- "q28dto1y"
reach_rates$agegrp[reach_rates$age_y == 1 & reach_rates$age_y_up == 5] <- "q1to5y"
reach_rates$agegrp[reach_rates$age_d == 0 & reach_rates$age_y_up == 1] <- "q0to1y"



reach_rates <- reach_rates %>%
  arrange(byvar, cut_time, age_y, age_y_up)

# Save output(s) ----------------------------------------------------------

saveRDS(gapu5m_rates, "./gen/mort/output/gapu5m-rates.rds")
saveRDS(gapu5m_plot, "./gen/mort/output/gapu5m-for-plots.rds")
saveRDS(reach_rates, "./gen/mort/output/reach-rates.rds")
