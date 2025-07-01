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
fph <- readRDS("./gen/fph/output/fph-tips.rds")
# sampling weights
wt1 <- read_excel("./data/instat-20250623/Poids_Enquete_Base_Mortalite.xlsx", sheet = "Poidsformule")
wt2 <- read_excel("./data/instat-20250623/Poids_Enquete_Base_Mortalite.xlsx", sheet = "Poidsvf")
################################################################################

# Before I had weights and sample design ----------------------------------

# subset live births
dat <- subset(fph, q223_aug == 1)

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
pnmr_all$agegrp <- "Postneonatal"
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
pnmr_res$agegrp <- "Postneonatal"
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
pnmr_reg$agegrp <- "Postneonatal"
u5m_reg <- my_calc_nqx(dat, ~qlregion, tips = c(0, 5, 10, 15), agegr = c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12)
names(u5m_reg)[which(names(u5m_reg) == "qlregion")] <- "byvar"
u5m_reg$agegrp <- "Under5"
reg <- rbind(nmr_reg, pnmr_reg, u5m_reg)
reg$type <- "Region"

rates_demog_nosurvdes <- rbind(all, res, reg)

# Weights and sample design  ----------------------------------------------

wt1 <- wt1[, c("GRAPPE", "CStrate")]
wt2 <- wt2[, c("GRAPPE", "Lstatut", "Poids normalisé des femmes de 15-49 ans")]
wt <- merge(wt1, wt2, by = "GRAPPE")
names(wt) <- c("grappe", "strate", "area", "wt")
wt$strate <- as.numeric(wt$strate)
wt$grappe <- as.numeric(wt$grappe)

# subset live births
dat <- subset(fph, q223_aug == 1)

# merge on weights
dat <- merge(dat, wt, by = "grappe")

# dhs vars
dat$b3 <- dat$dob_dec
dat$dod <- dat$dod_dec
dat$v008 <- dat$v008_dec
# weights
dat$v005 <- dat$wt
dat$death <- dat$event == 1
# Clusters
dat$v021 <- dat$grappe
# strata
dat$v024 <- dat$strate
dat$v025 <- 1
# byvar
dat$qtype <- factor(dat$qtype)
dat$strate <- factor(dat$strate)
dat$area <- factor(dat$area)

nmr_all <- calc_nqx(dat, tips = c(0, 5, 10, 15), agegr = c(0, 1)/12, varmethod = "jkn", scale = 1, origin = 0)
nmr_all$agegrp <- "Neonatal"
pnmr_all <- calc_nqx(dat, tips = c(0, 5, 10, 15), agegr = c(1, 3, 5, 12, 24, 36, 48, 60)/12, scale = 1, origin = 0)
pnmr_all$agegrp <- "Postneonatal"
u5m_all <- calc_nqx(dat, tips = c(0, 5, 10, 15), agegr = c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12, scale = 1, origin = 0)
u5m_all$agegrp <- "Under5"
all <- rbind(nmr_all, pnmr_all, u5m_all)
all$byvar <- "All"
all$type <- "All"

nmr_res <- calc_nqx(dat, ~qtype, tips = c(0, 5, 10, 15), agegr = c(0, 1)/12, scale = 1, origin = 0)
nmr_res$agegrp <- "Neonatal"
pnmr_res <- calc_nqx(dat, ~qtype, tips = c(0, 5, 10, 15), agegr = c(1, 3, 5, 12, 24, 36, 48, 60)/12, scale = 1, origin = 0)
pnmr_res$agegrp <- "Postneonatal"
u5m_res <- calc_nqx(dat, ~qtype, tips = c(0, 5, 10, 15), agegr = c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12, scale = 1, origin = 0)
u5m_res$agegrp <- "Under5"
res <- rbind(nmr_res, pnmr_res, u5m_res)
names(res)[which(names(res) == "qtype")] <- "byvar"
res$type <- "Residence"

nmr_strata <- calc_nqx(dat, ~strate, tips = c(0, 5, 10, 15), agegr = c(0, 1)/12, scale = 1, origin = 0)
nmr_strata$agegrp <- "Neonatal"
pnmr_strata <- calc_nqx(dat, ~strate, tips = c(0, 5, 10, 15), agegr = c(1, 3, 5, 12, 24, 36, 48, 60)/12, scale = 1, origin = 0)
pnmr_strata$agegrp <- "Postneonatal"
u5m_strata <- calc_nqx(dat, ~strate, tips = c(0, 5, 10, 15), agegr = c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12, scale = 1, origin = 0)
u5m_strata$agegrp <- "Under5"
strata <- rbind(nmr_strata, pnmr_strata, u5m_strata)
names(strata)[which(names(strata) == "strate")] <- "byvar"
strata$type <- "Strata"

nmr_area <- calc_nqx(dat, ~area, tips = c(0, 5, 10, 15), agegr = c(0, 1)/12, scale = 1, origin = 0)
nmr_area$agegrp <- "Neonatal"
pnmr_area <- calc_nqx(dat, ~area, tips = c(0, 5, 10, 15), agegr = c(1, 3, 5, 12, 24, 36, 48, 60)/12, scale = 1, origin = 0)
pnmr_area$agegrp <- "Postneonatal"
u5m_area <- calc_nqx(dat, ~area, tips = c(0, 5, 10, 15), agegr = c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12, scale = 1, origin = 0)
u5m_area$agegrp <- "Under5"
area <- rbind(nmr_area, pnmr_area, u5m_area)
names(area)[which(names(area) == "area")] <- "byvar"
area$type <- "Area"

rates_demog <- rbind(all, res, strata, area)

# Save output(s) ----------------------------------------------------------

saveRDS(rates_demog_nosurvdes, "./gen/mort/audit/demog-rates-nosurvdes.rds")
saveRDS(rates_demog, "./gen/mort/audit/demog-rates.rds")

