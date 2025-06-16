################################################################################
#' @description Calculate demogsurv rates for comparison
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(demogsurv)
library(survival)
#' Inputs
source("./src/utils.R")
dat <- readRDS("./gen/fph/output/fph-tips.rds")
################################################################################

# subset live births
dat <- subset(dat, q223_aug == 1)

# weights
dat$v005 <- 1
dat$death <- dat$event == 1
# Clusters
dat$v021 <- 1
# strata
dat$v024 <- 1
dat$v025 <- 1
dat$qlregion <- factor(dat$qlregion)
dat$qtype <- factor(dat$qtype)

nmr_all <- my_calc_nqx(dat, tips = c(0, 5, 10, 15), agegr = c(0, 1)/12)
nmr_all$byvar <- "All" 
nmr_all$agegrp <- "Neonatal"
pnmr_all <- my_calc_nqx(dat, tips = c(0, 5, 10, 15), agegr = c(1, 3, 5, 12, 24, 36, 48, 60)/12)
pnmr_all$byvar <- "All" 
pnmr_all$agegrp <- "1to59m"
u5m_all <- my_calc_nqx(dat, tips = c(0, 5, 10, 15), agegr = c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12)
u5m_all$byvar <- "All" 
u5m_all$agegrp <- "Under5"
all <- rbind(nmr_all, pnmr_all, u5m_all)
all$type <- "All"

nmr_res <- my_calc_nqx(dat, ~qtype, tips = c(0, 5, 10, 15), agegr = c(0, 1)/12)
names(nmr_res)[which(names(nmr_res) == "qtype")] <- "byvar"
nmr_res$agegrp <- "Neonatal"
pnmr_res <- my_calc_nqx(dat, ~qtype, tips = c(0, 5, 10, 15), agegr = c(1, 3, 5, 12, 24, 36, 48, 60)/12)
names(pnmr_res)[which(names(pnmr_res) == "qtype")] <- "byvar"
pnmr_res$agegrp <- "1to59m"
u5m_res <- my_calc_nqx(dat, ~qtype, tips = c(0, 5, 10, 15), agegr = c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12)
names(u5m_res)[which(names(u5m_res) == "qtype")] <- "byvar"
u5m_res$agegrp <- "Under5"
res <- rbind(nmr_res, pnmr_res, u5m_res)
res$type <- "Residence"

nmr_reg <- my_calc_nqx(dat, ~qlregion, tips = c(0, 5, 10, 15), agegr = c(0, 1)/12)
names(nmr_reg)[which(names(nmr_reg) == "qlregion")] <- "byvar"
nmr_reg$agegrp <- "Neonatal"
pnmr_reg <- my_calc_nqx(dat, ~qlregion, tips = c(0, 5, 10, 15), agegr = c(1, 3, 5, 12, 24, 36, 48, 60)/12)
names(pnmr_reg)[which(names(pnmr_reg) == "qlregion")] <- "byvar"
pnmr_reg$agegrp <- "1to59m"
u5m_reg <- my_calc_nqx(dat, ~qlregion, tips = c(0, 5, 10, 15), agegr = c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12)
names(u5m_reg)[which(names(u5m_reg) == "qlregion")] <- "byvar"
u5m_reg$agegrp <- "Under5"
reg <- rbind(nmr_reg, pnmr_reg, u5m_reg)
reg$type <- "Region"

rates_demog <- rbind(all, res, reg)

# Save output(s) ----------------------------------------------------------

saveRDS(rates_demog, "./gen/mort/audit/demog-rates.rds")


