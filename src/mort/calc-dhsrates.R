################################################################################
#' @description Calculate demogsurv rates for comparison
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(lubridate)
library(DHS.rates)
#' Inputs
source("./src/utils.R")
dat <- readRDS("./gen/fph/output/fph-tips.rds")
################################################################################

# subset live births
dat <- subset(dat, q223_aug == 1)

# strata
dat$v022 <- 1
# cluster
dat$v021 <- c(rep(1, nrow(dat)/2), rep(2, nrow(dat)/2 + 1))
# weight
dat$v005 <- 1
# date of interview in cmc
dat$v008 <- 12 * (year(dat$qintdate) - 1900) + month(dat$qintdate)
# date of birth in cmc
dat$b3 <- 12 * (year(dat$dob) - 1900) + month(dat$dob)
# age at death in months
dat$b7 <- dat$aadm

res0to4 <- chmort(dat, Period = 60, JK = "No")
res0to4
# R    SE     N WN  DEFT  RSE   LCI   UCI iterations
# NNMR  22.70  7.32 22052  0  6.99 0.32  8.35 37.05          2
# PNNMR 13.70  2.71 21490  0  3.66 0.20  8.39 19.01          2
# IMR   36.40 10.03 21955  0  7.67 0.28 16.74 56.06          2
# CMR   24.02  6.92 19812  0  6.87 0.29 10.46 37.58          2
# U5MR  59.54 16.43 20678  0 13.39 0.28 27.34 91.74          2
res5to9 <- chmort(dat, Period = 60, PeriodEnd = "2020-05", JK = "No")
res10to14 <- chmort(dat, Period = 60, PeriodEnd = "2015-05", JK = "No")

resp <- chmortp(dat, Period = 60)
resp
# PROBABILITY W.DEATHS W.EXPOSURE DEATHS EXPOSURE
# 0          0.0224        0       0.04  992.5  44300.0
# 1-2        0.0043        0       0.04  187.0  43223.0
# 3-5        0.0037        0       0.04  157.0  42794.0
# 6-11       0.0070        0       0.04  295.5  42029.0
# 12-23      0.0073        0       0.04  296.0  40490.0
# 24-35      0.0088        0       0.04  339.0  38642.0
# 36-47      0.0056        0       0.04  205.5  36476.5
# 48-59      0.0033        0       0.03  112.5  34234.0
sum(resp$DEATHS) # 1293.5
sum(resp$EXPOSURE) # 166528

res0to4 <- as.data.frame(res0to4)
res5to9 <- as.data.frame(res5to9)
res10to14 <- as.data.frame(res10to14)
res0to4$tips<- "0-4"
res5to9$tips <- "5-9"
res10to14$tips <- "10-14"
res0to4$agegrp <- row.names(res0to4)
res5to9$agegrp <- row.names(res5to9)
res10to14$agegrp <- row.names(res10to14)
rates_dhs <- rbind(res0to4, res5to9, res10to14)
rates_dhs$agegrp[rates_dhs$agegrp == "NNMR"] <- "Neonatal"
rates_dhs$agegrp[rates_dhs$agegrp == "U5MR"] <- "Under5"
rates_dhs$est <- rates_dhs$R/1000

rates_dhs$type <- "All"
rates_dhs$byvar <- "All"

# Save output(s) ----------------------------------------------------------

saveRDS(rates_dhs, "./gen/mort/audit/dhs-rates.rds")

