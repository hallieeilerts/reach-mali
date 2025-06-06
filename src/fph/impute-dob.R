################################################################################
#' @description Impute missing values for DOB
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
dat <- readRDS("./gen/fph/temp/fph-clean-aod.rds")
################################################################################

# DOB/end of pregnancy
unique(dat$q220d)
unique(dat$q220m)
unique(dat$q220y)

# Have already dropped those with missing year

# Subset unknown month or day
dat_unk <- subset(dat, is.na(q220d) | q220d == 98 | is.na(q220m) | q220m == 98)

# Subset known year, month, and day
dat_knw <- subset(dat, !(qwsec2b_id %in% dat_unk$qwsec2b_id))
nrow(dat) == nrow(dat_unk) + nrow(dat_knw)

# Create variable for end of pregnancy/dob when date is known
dat_knw$dob <- as.Date(paste(dat_knw$q220y, dat_knw$q220m, dat_knw$q220d, sep = "-"), format = "%Y-%m-%d")

dat_knw$q220m_imp_ind <- 0
dat_knw$q220d_imp_ind <- 0
dat_knw$q220m_comb <- dat_knw$q220m
dat_knw$q220d_comb <- dat_knw$q220d

# Missing DOB month, reported day -----------------------------------------

# imputation
set.seed(123)

# For individuals with a missing month but a reported day
# Impute month and...
# (i) make sure the date is possible (e.g., not February 31)
# (ii) make sure date with imputed month is not after interview
# (iii) new dob + age at death is not after interview

# Subset the cases that have missing months but reported days
to_impute <- which(
  (is.na(dat_unk$q220m) | dat_unk$q220m == 98) &
    !is.na(dat_unk$q220d) & dat_unk$q220d != 98
)
dat_unk$q220m_imp <- NA_integer_

for (i in to_impute) {

  y <- dat_unk$q220y[i]
  d <- dat_unk$q220d[i]
  qint_y <- dat_unk$qintdate_y[i]
  qint_m <- dat_unk$qintdate_m[i]
  qint_d <- dat_unk$qintdate_d[i]
  event <- dat_unk$event[i]
  aadd <- dat_unk$aadd[i]
  intdate <- dat_unk$qintdate[i]
  
  repeat {
    
    # if year is same as interview year, set new max month
    if(qint_y == y) {
      upper_month <- qint_m
    }else{
      upper_month <- 12
    }
    # sample month
    m <- sample(1:upper_month, 1)
    
    valid_date <- tryCatch({
      # Check...
      # (i) day is valid within month
      day_check <- d <= days_in_month(ymd(sprintf("%04d-%02d-01", y, m)))
      # (ii) dob year is same as interview, that month is not after
      # (iii) if dob year is same as interview, month and day are not after the interview
      limit_check <- !(y == qint_y & m == qint_m & d >= qint_d)
      # (iv) if individual has died, dod is not after interview date
      dod_check <- if(event == 0){
        TRUE
      }else{
          dob <- as.Date(paste(y, m, d, sep = "-"), format = "%Y-%m-%d")
          dod <- dob + aadd
          dod <= intdate
        }
      
      day_check & limit_check & dod_check
    }, error = function(e) FALSE)
    
    if (valid_date) {
      dat_unk$q220m_imp[i] <- m
      break
    }
  }
}

dat_unk$q220m_comb <- dat_unk$q220m
dat_unk$q220m_comb[to_impute] <- dat_unk$q220m_imp[to_impute]
dat_unk$q220m_imp_ind <- 0
dat_unk$q220m_imp_ind[to_impute] <- 1

# Combine q220m and q220_imp
#dat_unk$q220m_comb <- ifelse(is.na(dat_unk$q220m) | dat_unk$q220m == 98, dat_unk$q220m_imp, dat_unk$q220m)
# Indicator for month imputation
#dat_unk$q220m_imp_ind <- ifelse(is.na(dat_unk$q220m) | dat_unk$q220m == 98, 1, 0)

# Quality checks
check <- dat_unk[to_impute,]
# Check that none of the newly imputed dob are > intdate
check %>%
  mutate(dob =  as.Date(paste(q220y, q220m_comb, q220d, sep = "-"), format = "%Y-%m-%d")) %>%
  filter(dob > qintdate) %>% nrow # 0
# Check that none of the newly imputed dob + aad are > intdate
check %>%
  mutate(dob =  as.Date(paste(q220y, q220m_comb, q220d, sep = "-"), format = "%Y-%m-%d"),
         dod = dob + aadd) %>%
  filter(dod > qintdate) %>% nrow # 0

# Missing DOB month and day -----------------------------------------------

# For individuals with a missing month and day, 
# Impute month and day..
# (i) make sure the date is possible (e.g., not February 31)
# (ii) make sure date is not after interview
# (iii) new dob + age at death is not after interview

# Subset cases that have missing months and days
to_impute <- which(
  (is.na(dat_unk$q220m) | dat_unk$q220m == 98) & (is.na(dat_unk$q220d) | dat_unk$q220d == 98)
)

dat_unk$q220m_imp <- NA_integer_
dat_unk$q220d_imp <- NA_integer_

# Loop over only the relevant rows
for (i in to_impute) {
  
  y <- dat_unk$q220y[i]
  qint_y <- dat_unk$qintdate_y[i]
  qint_m <- dat_unk$qintdate_m[i]
  qint_d <- dat_unk$qintdate_d[i]
  event <- dat_unk$event[i]
  aadd <- dat_unk$aadd[i]
  intdate <- dat_unk$qintdate[i]
  
  # if year is same as interview year, set new max month
  if(qint_y == y) {
    upper_month <- qint_m
  }else{
    upper_month <- 12
  }
  
  repeat {
    
    # sample month
    m <- sample(1:upper_month, 1)
    # get max days for that month based on year/month
    date_stub <- ymd(sprintf("%04d-%02d-01", y, m))
    max_day <- days_in_month(date_stub)
    # if year and month are same as interview year/month, set new max day
    if(qint_y == y & qint_m == m) {
      upper_day <- min(qint_d, max_day)
    }else{
      upper_day <- max_day
    }
    
    # try to sample day 10 times before sampling new month
    tries <- 0
    success <- FALSE
    while (tries < 10) {
      d <- sample(1:upper_day, 1)
      
      valid_date <- tryCatch({
        dod_check <- if (event == 0) {
          TRUE
        } else {
          dob <- as.Date(paste(y, m, d, sep = "-"), format = "%Y-%m-%d")
          dod <- dob + aadd
          dod <= intdate
        }
        
        dod_check
      }, error = function(e) FALSE)
      
      if (valid_date) {
        dat_unk$q220m_imp[i] <- m
        dat_unk$q220d_imp[i] <- d
        success <- TRUE
        break
      }
      
      tries <- tries + 1
    }
    
    if (success) break
  }
}

dat_unk$q220m_comb[to_impute] <- dat_unk$q220m_imp[to_impute]
dat_unk$q220m_imp_ind[to_impute] <- 1

dat_unk$q220d_comb <- dat_unk$q220d
dat_unk$q220d_comb[to_impute] <- dat_unk$q220d_imp[to_impute]
dat_unk$q220d_imp_ind <- 0
dat_unk$q220d_imp_ind[to_impute] <- 1

# # Combine q220m and new imputation (already have imputed month once, so no need to create new column)
# dat_unk$q220m_comb[to_impute] <- dat_unk$q220m_imp[to_impute]
# # Combine q220d and first imputation (only for those we just imputed that have both month and day missing)
# dat_unk$q220d_comb <- ifelse((is.na(dat_unk$q220m) | dat_unk$q220m == 98) & (is.na(dat_unk$q220d) | dat_unk$q220d == 98),
#                              dat_unk$q220d_imp, dat_unk$q220d)
# 
# # Indicator for month imputation
# dat_unk$q220m_imp_ind[to_impute] <- 1
# # Indicator for day imputation
# dat_unk$q220d_imp_ind <- ifelse((is.na(dat_unk$q220m) | dat_unk$q220m == 98) & (is.na(dat_unk$q220d) | dat_unk$q220d == 98),
#                                 1, 0)

# Quality checks
check <- dat_unk[to_impute,]
# Check that none of the newly imputed dob are > intdate
check %>%
  mutate(dob =  as.Date(paste(q220y, q220m_comb, q220d_comb, sep = "-"), format = "%Y-%m-%d")) %>%
  filter(dob > qintdate) %>% nrow # 0
# Check that none of the newly imputed dob + aad are > intdate
check %>%
  mutate(dob =  as.Date(paste(q220y, q220m_comb, q220d_comb, sep = "-"), format = "%Y-%m-%d"),
         dod = dob + aadd) %>%
  filter(event == 1 & dod > qintdate) %>% nrow # 0

# Missing DOB day, reported month -----------------------------------------

# For individuals with a missing day but reported month, 
# Impute day
# (i) make sure the date is possible (e.g., not February 31)
# (ii) make sure date is not after interview
# Subset problems and check this
# (iii) new dob + age at death is not after interview

# Subset the cases that have missing day only
dat_imp_d <- subset(dat_unk,  !is.na(q220m) & q220m != 98 & (is.na(q220d) | q220d == 98))
dat_unk_other <- subset(dat_unk, !(qwsec2b_id %in% dat_imp_d$qwsec2b_id))

# Impute day following criteria i and ii
dat_imp_d <- dat_imp_d  %>%
  mutate(q220_date_stub = ymd(sprintf("%04d-%02d-01", q220y, q220m)),
         max_day = days_in_month(q220_date_stub)
  ) %>%
  rowwise() %>%
  mutate(
    q220d_imp = if(qintdate_y == q220y & qintdate_m == q220m) {
      upper_day <- min(qintdate_d, max_day)
      sample(1:upper_day, 1)
    } else {
      sample(1:max_day, 1)
    }
  ) %>%
  ungroup()
dat_imp_d$q220d_comb <- dat_imp_d$q220d_imp
dat_imp_d$q220d_imp_ind <- 1

# Identify problem cases
# Check that none of the newly imputed dob are > intdate
dat_imp_d %>%
  mutate(dob =  as.Date(paste(q220y, q220m_comb, q220d_comb, sep = "-"), format = "%Y-%m-%d")) %>%
  filter(dob > qintdate) %>% nrow # 0
# Check that none of the newly imputed dob + aad are > intdate
dat_imp_d %>%
  mutate(dob =  as.Date(paste(q220y, q220m_comb, q220d_comb, sep = "-"), format = "%Y-%m-%d"),
         dod = dob + aadd) %>%
  filter(event == 1 & dod > qintdate) %>% nrow # 2
# dat_imp_d %>%
#   mutate(dob =  as.Date(paste(q220y, q220m_comb, q220d_comb, sep = "-"), format = "%Y-%m-%d"),
#          dod = dob + aadd) %>%
#   filter(event == 1 & dod > qintdate) %>% 
#   select(q220y, q220m_comb, q220d_comb, dob, dod, qintdate)

# When aad is after the interview, impute day again
to_impute <- dat_imp_d %>%
  mutate(row_id = row_number(),
         dob =  as.Date(paste(q220y, q220m_comb, q220d_comb, sep = "-"), format = "%Y-%m-%d"),
         dod = dob + aadd) %>%
  filter(event == 1 & dod > qintdate) %>%
  pull(row_id)
for (i in to_impute) {
  
  y <- dat_imp_d$q220y[i]
  m <- dat_imp_d$q220m[i]
  qint_y <- dat_imp_d$qintdate_y[i]
  qint_m <- dat_imp_d$qintdate_m[i]
  qint_d <- dat_imp_d$qintdate_d[i]
  event <- dat_imp_d$event[i]
  aadd <- dat_imp_d$aadd[i]
  intdate <- dat_imp_d$qintdate[i]
  
  # get max days for that month based on year/month
  date_stub <- ymd(sprintf("%04d-%02d-01", y, m))
  max_day <- days_in_month(date_stub)
  # if year and month are same as interview year/month, set new max day
  if(qint_y == y & qint_m == m) {
    upper_day <- min(qint_d, max_day)
  }else{
    upper_day <- max_day
  }
  
  repeat {
    
    # sample day
    d <- sample(1:upper_day, 1)
    
    valid_date <- tryCatch({
      # Check...
      # (i) if individual has died, dod is not after interview date
      dod_check <- if(event == 0){
        TRUE
      }else{
        dob <- as.Date(paste(y, m, d, sep = "-"), format = "%Y-%m-%d")
        dod <- dob + aadd
        dod <= intdate
      }
      
      dod_check
    }, error = function(e) FALSE)
    
    if (valid_date) {
      dat_imp_d$q220d_imp[i] <- d
      break
    }
  }
}
dat_imp_d$q220d_comb[to_impute] <- dat_imp_d$q220d_imp[to_impute]
dat_imp_d$q220d_imp_ind[to_impute] <- dat_imp_d$q220d_imp_ind[to_impute]

# Identify problem cases
# Check that none of the newly imputed dob are > intdate
dat_imp_d %>%
  mutate(dob =  as.Date(paste(q220y, q220m_comb, q220d_comb, sep = "-"), format = "%Y-%m-%d")) %>%
  filter(dob > qintdate) %>% nrow # 0
# Check that none of the newly imputed dob + aad are > intdate
dat_imp_d %>%
  mutate(dob =  as.Date(paste(q220y, q220m_comb, q220d_comb, sep = "-"), format = "%Y-%m-%d"),
         dod = dob + aadd) %>%
  filter(event == 1 & dod > qintdate) %>% nrow # 0

# Recombine
dat_unk2 <- bind_rows(dat_unk_other, dat_imp_d)
nrow(dat_unk) == nrow(dat_unk2)

# Combine known dob with unknown ------------------------------------------

dat2 <- bind_rows(dat_knw, dat_unk2)
nrow(dat) == nrow(dat2)

nrow(subset(dat2, is.na(q220y))) # 0 
nrow(subset(dat2, is.na(q220m_comb))) # 0
nrow(subset(dat2, is.na(q220d_comb))) # 0

# Create dob and dod
dat2 <- dat2 %>%
  mutate(dob = as.Date(paste(q220y, q220m_comb, q220d_comb, sep = "-"), format = "%Y-%m-%d"),
         dod = dob + aadd)

nrow(subset(dat2, dod > qintdate)) # 0

nrow(dat2) # 79785

# Add decimal dates
dat2$dob_dec <- decimal_date(dat2$dob) # b3
dat2$dod_dec <- dat2$dob_dec + dat2$aady
dat2$v008_dec <- decimal_date(dat2$qintdate) # v008

# Save --------------------------------------------------------------------

saveRDS(dat2, "./gen/fph/temp/fph-impute-dob.rds")


