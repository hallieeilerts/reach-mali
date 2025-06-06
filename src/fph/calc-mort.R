################################################################################
#' @description
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
secover <- read.csv("./data/reach_femme_qsecover.csv")
sec01 <- read.csv("./data/reach_femme_qwsec01.csv")
sec2a <- read.csv("./data/reach_femme_qwsec2a.csv")
sec2b <- read.csv("./data/reach_femme_qwsec2b.csv")
sec2c <- read.csv("./data/reach_femme_qwsec2c.csv")
sec6a <- read.csv("./data/reach_femme_qwsec6a.csv")
################################################################################

# preliminary data recode on simple fbh

# fph <- sec2b
# fph$q220d[fph$q220d ==98] <- 15
# fph$q220m[fph$q220m ==98] <- 6
# fph$dob <- as.Date(paste(fph$q220y, fph$q220m, fph$q220d, sep = "-"), format = "%Y-%m-%d")
# fph$died <- 0
# fph$died[!is.na(fph$q224) & fph$q224 == 2] <- 1
# dead <- which(fph$died == 1)
# 
# fph$aad <- fph$q228
# fph$aad <- gsub(" ", "0", fph$aad)
# fph$aad <- as.numeric(as.character(fph$aad))
# # separate unit from value
# fph$aad_unit <- trunc(fph$aad/100)
# fph$aad_value <- fph$aad - fph$aad_unit * 100
# 
# # Transform DAILY scale into MONTHLY scale
# fph$aad[which(fph$aad_unit == 1)] <- fph$aad[which(fph$aad_unit == 1)] / 31
# # Transform YEARLY scale into MONTHLY scale
# fph$aad[which(fph$aad_unit == 3)] <- fph$aad[which(fph$aad_unit == 3)]*12
# # If AAD was in months (aad_unit == 2), no need to rescale
# fph$dod <- NA
# fph$dod[dead] <- fph$dob[dead] + fph$aad[dead]


# Assess sample -----------------------------------------------------------

nrow(secover) # 25309
length(unique(secover$qsecover_id)) #  25309
length(unique(secover$level_1_id)) #  25309

# sbh
nrow(sec2a) # 25006
length(unique(sec2a$level_1_id)) # 25,006 women interviewed?

# fph
nrow(sec2b) # 76652
length(unique(sec2b$qwsec2b_id)) # 76652
length(unique(sec2b$level_1_id)) # 18289 women who did fph?

# Calculate mortality rate ------------------------------------------------

# replace once i find interview date in date
int_date <- as.Date("2025-05-09", format = "%Y-%m-%d")

fph <- sec2b
fph$q220d[fph$q220d ==98] <- 1
fph$q220m[fph$q220m ==98] <- 1
fph$dob <- as.Date(paste(fph$q220y, fph$q220m, fph$q220d, sep = "-"), format = "%Y-%m-%d")
fph$event <- !is.na(fph$q228)
fph$random <- NA

# estimation of the exposure for alive children (in person-YEARS)
set.seed(111)
# day of interview days
# !!! need to find interview dates
fph$v008_d   <- int_date - runif(nrow(fph), min = 0, max = 1)
set.seed(777)
# dob days
# !!!make this a random date day when not reported
fph$b3_d  <- fph$dob
nrow(subset(fph, is.na(dob)))
# !!! need to impute
fph <- subset(fph, !is.na(dob))

# Exposure when date of birth = date of survey 
# !!! need to find interview dates
fph$select <- fph$event == FALSE & int_date  == fph$b3_d 
set.seed(1)
fph$expo <- NULL
# !!! this used to be a random day since al measures in days
fph$expo[fph$select == T] <-  0

# Exposure when date of birth != date of survey 
fph$select <- fph$event == FALSE & int_date != fph$b3_d
fph$expo[fph$select == T] <-  int_date - fph$b3_d[fph$select == T] 

# estimation of the exposition for dead children  (in person-YEARS)

# set a random number between 0 and 1 for uniform imputation of days
set.seed(777)
fph$random[fph$event == TRUE ] <- 15

fph$b6 <- fph$q228
fph$b6 <- gsub(" ", "0", fph$b6)
fph$b6 <- as.numeric(as.character(fph$b6))

# use variable b6 to get age age at death (in person-years -> /365.25 or /12)
fph$b6[is.na(fph$b6)] <- 0

#redistribute missing values
fph$b6[fph$b6 >= 190 & fph$b6 < 200] <- sample(100:130, 1)
fph$b6[fph$b6 >= 290 & fph$b6 < 300] <- sample(200:223, 1)
fph$b6[fph$b6 >= 390 & fph$b6 < 400] <- sample(300:334, 1)
fph$b6[fph$b6 >= 900] <- sample(c(100:130, 200:223, 300:334), 1)

fph$expo[fph$b6 >= 100  & fph$b6 < 200] <-  (as.numeric(fph$b6[fph$b6 >= 100 & fph$b6 < 200]) - 100 + fph$random[fph$b6 >= 100 & fph$b6 < 200])/365.25                                                        
fph$expo[fph$b6 >= 200  & fph$b6 < 300] <-  (as.numeric(fph$b6[fph$b6 >= 200 & fph$b6 < 300]) - 200 + fph$random[fph$b6 >= 200 & fph$b6 < 300])/12                                                    
fph$expo[fph$b6 >= 300] <-(as.numeric(fph$b6[fph$b6 >= 300]) - 300 + fph$random[fph$b6 >=  300])

####################################################
#### ESTIMATING MORTALITY with SURVIVAL PACKAGE ####
####################################################

library(survival)

subset(fph, expo < 0)

# provide the exposition and the life status at the moment of right truncation
surv <- with(fph, Surv(time = fph$expo, event = fph$event, type = "right"))

# define year cuts (and labels)
yearcut <- c(2020, 2025)
yearcutlab <- c("[2020, 2025)")
# give also date of birth
fph$dob_dec <- lubridate::decimal_date(fph$dob)
year <- tcut(fph$dob_dec, yearcut, labels = yearcutlab)        

# "startage" sets the age at entry time (that is birth), so it is 0
fph$startage = 0
#define age groups in weeks and months (but expressed in years) and labels (expressed in days)
agecut <-  c(0,7,14, 21, 28,
             seq(60.8750, 365.25*5,  30.4375))/365.25
agecutlab <- as.character(agecut[1:(length(agecut)-1)]*365.25)
age <- tcut(fph$startage, agecut, labels = agecutlab)

#function that computes deaths and person-years for the defined periods and age group (use weight variable)
# !!!! weights?
pyears <- pyears(surv ~ year + age, scale = 1, data.frame = TRUE)
PY <- pyears[[2]]
PY

#################################
#### FORMAT MORTALITY OUTPUT ####
#################################


# Estimate mx (PY$event/PY$pyears)
PY$mx     <- PY$event/PY$pyears
PY$age_d_low  <- as.numeric(paste(PY$age))
PY$n_d    <- c(rep(7,4), 32.8750, rep(30.4375,58))
PY$age_d_up  <- PY$age_d_low + PY$n_d 


# Estimate q(x) for gapu5m age groups 
gapu5m_age <- c(7,14, 21, 28,
                seq(60.8750, 365.25,  30.4375),
                seq(365.25 + 365.25/4 , 365.25*2,  365.25/4),
                seq(365.25*3 , 365.25*5,  365.25))

e <- exp(1)  
tmp2 <-lapply(gapu5m_age, FUN = function(x){
  i  <- which(PY$age_d_up == x) 
  PY$ID <- c(rep(1,i), rep(NA, nrow(PY)-i))
  qx  <-  1-e^-sum(PY$mx*PY$n_d/365.25*PY$ID,  na.rm = T)
  qx
})
rates        <-  do.call(rbind.data.frame, tmp2)
names(rates) <- "qx"


# Estimate mx for gapu5m age groups 
rates$age_d <- c(0, gapu5m_age [-length(gapu5m_age )]) 
rates$n_d   <- c(rep(7,4), 32.8750,
                 rep(30.4375, 10),
                 rep(91.3125, 4),
                 rep(365.25, 3))
rates$mx    <- -log(1-c(rates$qx[1],1-(1-rates$qx[-1])/(1- rates$qx[-22])))/(rates$n_d/365.25)


rates$age_d_mid <- rates$age_d + rates$n_d/2
rates$age_d_up <- rates$age_d + rates$n_d
rates$age_y <- rates$age_d/365.25
rates$age_y_up <- rates$age_d_up/365.25
rates$year <- unique(PY$year)
#rates <- rates[,c("year", "age_d", "n_d", "age_d_mid", "age_d_up",  "mx", "qx")]
rates


ggplot(rates) +
  geom_step(aes(x = age_y_up, y = mx)) +
  scale_y_log10()
ggplot(rates) +
  geom_step(aes(x = age_y_up, y = qx))
