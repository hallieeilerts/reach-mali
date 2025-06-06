################################################################################
#' @description Prepare DOB for respondent
#' @return Cleaned qsecover file
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#library(fitdistrplus) # not loading because loads MASS and masks dplyr select()
#' Inputs
dat <- readRDS("./gen/sec01/temp/qwsec01-clean.rds")
################################################################################

# ages vector
ages <- dat %>%
  filter(!is.na(q111)) %>%
  mutate(age = q111) %>%
  dplyr::select(age) %>% pull

# fit gamma to age distribution to age and check fit
fit_gamma <- fitdistrplus::fitdist(ages, "gamma")
summary(fit_gamma)
#plot(fit_gamma)
hist(ages, probability = TRUE, breaks = 30, main = "gamma Fit", xlab = "Age")
curve(dgamma(x+3, shape = fit_gamma$estimate["shape"], rate  = fit_gamma$estimate["rate"]),
      col = "blue", lwd = 2, add = TRUE)

# Extract estimated parameters from fit
shape <- fit_gamma$estimate["shape"]
rate  <- fit_gamma$estimate["rate"]
shift <- -3  # equivalent to x + 3 in dgamma means shift left by 3

# identify those with missing age
to_impute <- which((is.na(dat$q111)))

# create columns
dat$q111_imp_ind <- 0
dat$q111_imp <- NA_integer_

# set seed
set.seed(123)

# sample random values from gamma
for (i in to_impute) {
  
  repeat {
    # Sample a single age from the shifted gamma
    age <- rgamma(1, shape = shape, rate = rate) + shift
    age <- round(age)
    
    # check that age is in the desired range
    if (age >= 15 & age < 50) {
      dat$q111_imp[i] <- age
      break
    }
  }
}
# check
dat$q111_imp_ind[to_impute] <- 1

# create combined variable of real and imputed ages
dat$q111_comb <- dat$q111
dat$q111_comb[to_impute] <- dat$q111_imp[to_impute]

# create age categorization
dat$agecat_resp <- cut(dat$q111_comb, breaks = seq(14, 50, 5),
                    labels = c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49"))

nrow(subset(dat, is.na(q111_comb))) # 0 
nrow(subset(dat, is.na(q111_imp_ind))) # 0
nrow(subset(dat, is.na(agecat_resp))) # 0 

# Save --------------------------------------------------------------------

saveRDS(dat, "./gen/sec01/temp/qwsec01-age.rds")



