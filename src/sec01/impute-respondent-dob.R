################################################################################
#' @description Prepare DOB for respondent
#' @return No longer using this. Use imputed age instead.
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(fitdistrplus)
#' Inputs
dat <- readRDS("./gen/sec01/output/qwsec01-clean.rds")
################################################################################

# Recode dob --------------------------------------------------------------

# recode missing years and months of birth
sort(unique(dat$q110y))
dat$q110y[dat$q110y %in% c(16, 40, 9998)] <- NA
sort(unique(dat$q110m))
dat$q110m[dat$q110m %in% c(98)] <- NA

# Fit gamma distribution to age -------------------------------------------

# calculate age for those with reported year
ages <- dat %>%
  filter(!is.na(q110y)) %>%
  mutate(age = 2025 - q110y) %>%
  dplyr::select(age) %>% pull

# fit gamma to age distribution and check fit
fit_gamma <- fitdist(ages, "gamma")
summary(fit_gamma)
#plot(fit_gamma)
hist(ages, probability = TRUE, breaks = 30, main = "gamma Fit", xlab = "Age")
curve(dgamma(x+3, shape = fit_gamma$estimate["shape"], rate  = fit_gamma$estimate["rate"]),
      col = "blue", lwd = 2, add = TRUE)

# Extract estimated parameters from fit
shape <- fit_gamma$estimate["shape"]
rate  <- fit_gamma$estimate["rate"]
shift <- -3  # equivalent to x + 3 in dgamma means shift left by 3

# Draw many samples from the fitted gamma
set.seed(123)
#samples <- rgamma(10000, shape = shape, rate = rate) + shift

# Impute year -------------------------------------------------------------

# subset those with missing year or month of birth
dat_unk <- subset(dat, is.na(q110y) | is.na(q110m))
# non-missing
dat_knw <- subset(dat, !(id_data_cover %in% dat_unk$id_data_cover))
nrow(dat_unk) + nrow(dat_knw) == nrow(dat)

dat_knw$q110y_imp_ind <- 0
dat_knw$q110m_imp_ind <- 0
dat_knw$q110y_comb <- dat_knw$q110y
dat_knw$q110m_comb <- dat_knw$q110m

dat_unk$q110y_imp_ind <- 0
dat_unk$q110m_imp_ind <- 0
dat_unk$q110y_imp <- NA_integer_
dat_unk$q110m_imp <- NA_integer_
dat_unk$q110y_comb <- dat_unk$q110y
dat_unk$q110m_comb <- dat_unk$q110m

to_impute <- which(
  (is.na(dat_unk$q110y))
)

for (i in to_impute) {
  
  repeat {
    # Sample a single age from the shifted gamma
    age <- rgamma(1, shape = shape, rate = rate) + shift
    
    # age is in the desired range
    # need to be less than 49 by 2026
    if (age >= 15 & age < 49) {
      dat_unk$q110y_imp[i] <- round(2025 - age)
      break
    }
  }
}

dat_unk$q110y_imp_ind[to_impute] <- 1
dat_unk$q110y_comb[to_impute] <- dat_unk$q110y_imp[to_impute]

#head(subset(dat_unk, is.na(q110y))[,c("q110y","q110y_imp", "q110y_imp_ind")])
#range(2025-subset(dat_unk, is.na(q110y))$q110y_imp) # 15-49

# Impute month ------------------------------------------------------------

to_impute <- which(
  (is.na(dat_unk$q110m))
)
no_impute <- which(
  (!is.na(dat_unk$q110m))
)

dat_unk <- dat_unk %>%
  rowwise() %>%
  mutate(q110m_imp =  sample(1:12, 1)) %>%
  ungroup()

dat_unk$q110m_imp_ind[to_impute] <- 1
dat_unk$q110m_comb[to_impute] <- dat_unk$q110m_imp[to_impute]

#head(subset(dat_unk, is.na(q110m))[,c("q110m","q110m_imp", "q110m_imp_ind")])

# Combine known and unknown -----------------------------------------------

dat2 <- bind_rows(dat_knw, dat_unk)

# Impute days
dat2 <- dat2 %>%
  mutate(q110_date_stub = ymd(sprintf("%04d-%02d-01", q110y_comb, q110m_comb)),
         max_day = days_in_month(q110_date_stub)
  ) %>%
  rowwise() %>%
  mutate(q110d_imp =  sample(1:max_day, 1)) %>%
  ungroup()

nrow(subset(dat2, is.na(q110y_comb))) # 0 
nrow(subset(dat2, is.na(q110m_comb))) # 0
nrow(subset(dat2, is.na(q110d_imp))) # 0

# Create dob
dat2 <- dat2 %>%
  mutate(dob_resp = as.Date(paste(q110y_comb, q110m_comb, q110d_imp, sep = "-"), format = "%Y-%m-%d")) %>%
  dplyr::select(-c(q110y_comb, q110m_comb, q110y_imp, q110m_imp, q110_date_stub, max_day, q110d_imp))


# Save --------------------------------------------------------------------

#saveRDS(dat2, "./gen/sec01/temp/qwsec01-dob.rds")
