################################################################################
#' @description Back of envelope
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(lubridate)
#' Inputs
source("./src/utils.R")
dat <- read.csv("./data/reach_mortalite_femme_qwsec2b.csv")
qsecover <- readRDS("./gen/sec01/output/qsecover-qwsec01.rds")
#########################################################################

# dob
# drop unknown dob
nrow(dat) #  79843
dat <- subset(dat, !(is.na(q220d) | q220d == 98 | is.na(q220m) | q220m == 98 | q220y == 98 | is.na(q220y)))
nrow(dat) # 55001
dat$dob <- as.Date(paste(dat$q220y, dat$q220m, dat$q220d, sep = "-"), format = "%Y-%m-%d")
dat$dob_dec <- decimal_date(dat$dob)

# event
dat$event <- ifelse(dat$q224 == 2, 1, 0)
table(dat$event, useNA = "always")

# interview date
dat <- merge(qsecover[,c("level_1_id", "qintdate")], dat, by = "level_1_id")
dat$v008_dec <- decimal_date(dat$qintdate)
nrow(subset(dat, is.na(v008_dec))) # 0

# calculate dod
# Replace blank pace with 0
dat$q228_clean <- gsub(" ", "0", dat$q228)
dat$q228_clean <- as.numeric(as.character(dat$q228_clean))
dat$aad_unit <- trunc(dat$q228_clean/100)
dat$aad_value <- dat$q228_clean - dat$aad_unit * 100
# Transform into months
dat$aadm <- dat$aad_value
dat$aadm[which(dat$aad_unit == 1)] <- dat$aadm[which(dat$aad_unit == 1)] / 30.5
dat$aadm[which(dat$aad_unit == 3)] <- dat$aadm[which(dat$aad_unit == 3)]*12
# Transform into days
dat$aadd <- dat$aad_value
dat$aadd[which(dat$aad_unit == 2)] <- dat$aadd[which(dat$aad_unit == 2)] * 30.5
dat$aadd[which(dat$aad_unit == 3)] <- dat$aadd[which(dat$aad_unit == 3)] * 365.25
# Transform into years
dat$aady <- dat$aad_value
dat$aady[which(dat$aad_unit == 1)] <- dat$aady[which(dat$aad_unit == 1)] / 365.25
dat$aady[which(dat$aad_unit == 2)] <- dat$aady[which(dat$aad_unit == 2)] / 12
dat$dod_dec <- dat$dob_dec + dat$aady
nrow(subset(dat, !is.na(q228_clean) & is.na(aady))) # 0

# drop any where dod is after v008_dec
dat <- subset(dat, is.na(dod_dec) | (!is.na(dod_dec) & dod_dec <= v008_dec))
nrow(dat) # 54948

#View(dat[,c("q223","q224","dob_dec","v008_dec","event","aad_unit", "aad_value","aadd", "dod_dec")])
# Limit to live births
dat <- subset(dat, q223 == 1)

# create vars for demogsurv
dat$v005 <- 1
dat$death <- !is.na(dat$event) & dat$event == 1
# Clusters
dat$v021 <- 1
# strata
dat$v024 <- 1
dat$v025 <- 1

# calc mortality
my_calc_nqx(dat, tips = c(0, 5, 10, 15), agegr = c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12)

# tips        est
# byf10-14 10-14 0.06804787
# byf5-9     5-9 0.05152054
# byf0-4     0-4 0.04037265
