################################################################################
#' @description 
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(survival)
#' Inputs
dat <- readRDS("./gen/fph/output/fph-tips.rds")
################################################################################

# Exposure when date of birth = date of survey 
dat$select <- dat$event == FALSE & dat$dob_dec == dat$v008_dec
set.seed(1)
dat$expo <- NA
dat$expo[dat$select == T] <-  0

# Exposure when date of birth != date of survey 
dat$select <- dat$event == FALSE & dat$dob_dec != dat$v008_dec
dat$expo[dat$select == T] <-  dat$v008_dec[dat$select == T] - dat$dob_dec[dat$select == T] 

# Exposure for non-surviving children

# set a random number between 0 and 1 for uniform imputation of days
set.seed(777)
dat$random <- NA
dat$random[dat$event == 1] <- runif(nrow(dat[dat$event == 1,]), min = 0,  max = 1)

# Age at death in person-years
#dat$expo[dat$event == 1] <- dat$aady[dat$event == 1]
# Age at death in person-years with imputation for random days
dat$expo[dat$event == 1 & dat$aad_unit == 1] <- (as.numeric(dat$aad_value[dat$event == 1 & dat$aad_unit == 1]) + dat$random[dat$event == 1 & dat$aad_unit == 1])/365.25   
dat$expo[dat$event == 1 & dat$aad_unit == 2] <- (as.numeric(dat$aad_value[dat$event == 1 & dat$aad_unit == 2]) + dat$random[dat$event == 1 & dat$aad_unit == 2])/12          
dat$expo[dat$event == 1 & dat$aad_unit == 3] <- (as.numeric(dat$aad_value[dat$event == 1 & dat$aad_unit == 3]) + dat$random[dat$event == 1 & dat$aad_unit == 3])

# Provide the exposition and the life status at the moment of right truncation
surv <- with(dat, Surv(time = dat$expo, event = dat$event, type = "right"))

# define year cuts (and labels)
#yearcut <- c(2020, 2025)
#yearcutlab <- c("[2020, 2025)")
yearcut <- c(2010, 2015, 2020, 2025)
yearcutlab <- c("[2010, 2015)", "[2015, 2020)", "[2020, 2025)")

# give also date of birth
year <- tcut(dat$dob_dec, yearcut, labels = yearcutlab)     

# "startage" sets the age at entry time (that is birth), so it is 0
dat$startage = 0

# define age groups in weeks and months (but expressed in years) and labels (expressed in days)
agecut <-  c(0,7,14, 21, 28, seq(60.8750, 365.25*5,  30.4375))/365.25
#agecut <-  c(0, 7, 14, 21, 28, seq(60.8750, 365.25*2,  30.4375), 365.25*3, 365.25*4, 365.25*5)/365.25
agecutlab <- as.character(agecut[1:(length(agecut)-1)]*365.25)
age <- tcut(dat$startage, agecut, labels = agecutlab)

#function that computes deaths and person-years for the defined periods and age group (use weight variable)
# !!!! weights?
pyears <- pyears(surv ~ year + age, scale = 1, data.frame = TRUE)
PY <- pyears[[2]]
PY <- PY[order(PY$year, PY$age),]

# Estimate mx (PY$event/PY$pyears)
PY$mx     <- PY$event/PY$pyears
PY$age_d_low  <- as.numeric(paste(PY$age))
PY$age <- as.numeric(as.character(PY$age))
#PY$n_d <- c(PY$age[-1] - PY$age[-length(PY$age)], 365.25)
PY$n_d    <- rep(c(rep(7,4), 32.8750, rep(30.4375,58)), length(yearcutlab))
PY$age_d_up  <- PY$age_d_low + PY$n_d 


# Estimate q(x) for gapu5m age groups 
gapu5m_age <- c(7,14, 21, 28,
                seq(60.8750, 365.25,  30.4375),
                seq(365.25 + 365.25/4 , 365.25*2,  365.25/4),
                seq(365.25*3 , 365.25*5,  365.25))
#gapu5m_age <- c(7, 14, 21, 28, seq(60.8750, 365.25*2,  30.4375), 365.25*3, 365.25*4, 365.25*5)

e <- exp(1)  
l_tmp <- list()
for(i in 1:length(yearcutlab)){
  tmp <- subset(PY, year == yearcutlab[i])
  l_tmp[[i]] <- lapply(gapu5m_age, FUN = function(x){
    i  <- which(tmp$age_d_up == x) 
    tmp$ID <- c(rep(1,i), rep(NA, nrow(tmp)-i))
    qx  <-  1-e^-sum(tmp$mx*tmp$n_d/365.25*tmp$ID,  na.rm = T)
    qx
  })
}
l_rates <- lapply(l_tmp, function(x) do.call(rbind.data.frame, x))
l_rates <- lapply(l_rates, function(x){ names(x) <- "qx"; return(x)})
names(l_rates) <- yearcutlab

l_rates <- lapply(l_rates, function(x){x$age_d <- c(0, gapu5m_age [-length(gapu5m_age )])  ; return(x)})
l_rates <- lapply(l_rates, function(x){x$n_d <- c(rep(7,4), 32.8750,rep(30.4375, 10),rep(91.3125, 4), rep(365.25, 3)) ; return(x)})
l_rates <- lapply(l_rates, function(x){x$mx <- -log(1-c(x$qx[1],1-(1-x$qx[-1])/(1- x$qx[-22])))/(x$n_d/365.25) ; return(x)})

l_rates <- lapply(l_rates, function(x){
  x$age_d_mid <- x$age_d + x$n_d/2
  x$age_d_up <- x$age_d + x$n_d
  x$age_y <- x$age_d/365.25
  x$age_y_up <- x$age_d_up/365.25 ; return(x)})
df_rates <- plyr::ldply(l_rates,  .id = "year")

# Add plot rows
fn_addrows <- function(x){
  firstrow <- x[1, ]
  firstrow$qx <- 0
  firstrow$age_y <- 0
  lastrow <- x[nrow(x),]
  lastrow$age_y <- 5
  x <- rbind(firstrow, x, lastrow)
  return(x)
}
l_plot <- lapply(l_rates, fn_addrows)
df_plot <- plyr::ldply(l_plot, .id = "year")

ggplot(df_plot) +
  geom_step(aes(x = age_y, y = mx)) +
  scale_y_log10() +
  facet_wrap(~year)

ggplot(df_plot) +
  geom_step(aes(x = age_y, y = qx*1000)) +
  facet_wrap(~year)

subset(df_rates, age_y_up == 5)


# FOR SINGLE YEAR

# tmp2 <-lapply(gapu5m_age, FUN = function(x){
#   i  <- which(PY$age_d_up == x) 
#   PY$ID <- c(rep(1,i), rep(NA, nrow(PY)-i))
#   qx  <-  1-e^-sum(PY$mx*PY$n_d/365.25*PY$ID,  na.rm = T)
#   qx
# })
# rates        <-  do.call(rbind.data.frame, tmp2)
# names(rates) <- "qx"

# Estimate mx for gapu5m age groups 
# rates$age_d <- c(0, gapu5m_age [-length(gapu5m_age )]) 
# rates$n_d   <- c(rep(7,4), 32.8750, rep(30.4375, 10), rep(91.3125, 4), rep(365.25, 3))
# #rates$n_d <- c(rates$age_d[-1] - rates$age_d[-length(rates$age_d)], 365.25)
# rates$mx    <- -log(1-c(rates$qx[1],1-(1-rates$qx[-1])/(1- rates$qx[-length(rates$qx)])))/(rates$n_d/365.25)

# # add years
# rates$age_d_mid <- rates$age_d + rates$n_d/2
# rates$age_d_up <- rates$age_d + rates$n_d
# rates$age_y <- rates$age_d/365.25
# rates$age_y_up <- rates$age_d_up/365.25
# rates$year <- unique(PY$year)
# 
# # add first and last rows for plotting
# plot <- rates
# firstrow <- rates[1, ]
# firstrow$qx <- 0
# firstrow$age_y <- 0
# lastrow <- rates[nrow(rates),]
# lastrow$age_y <- 5
# plot <- rbind(firstrow, rates, lastrow)
# 
# ggplot(plot) +
#   geom_step(aes(x = age_y, y = mx)) +
#   scale_y_log10()
# ggplot(plot) +
#   geom_step(aes(x = age_y, y = qx))

