################################################################################
#' @description 
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
qsecover <- readRDS("./gen/sec01/output/qsecover-qwsec01.rds")
# sampling weights
wt1 <- read_excel("./data/instat-20250623/Poids_Enquete_Base_Mortalite.xlsx", sheet = "Poidsformule")
wt2 <- read_excel("./data/instat-20250623/Poids_Enquete_Base_Mortalite.xlsx", sheet = "Poidsvf")
################################################################################

wt1 <- wt1[, c("GRAPPE", "CStrate")]
wt2 <- wt2[, c("GRAPPE", "Lstatut", "Poids normalisé des femmes de 15-49 ans")]
wt <- merge(wt1, wt2, by = "GRAPPE")
names(wt) <- c("grappe", "strate", "area", "wt")
wt$strate <- as.numeric(wt$strate)
wt$grappe <- as.numeric(wt$grappe)

# Merge on weights --------------------------------------------------------

# dhs vars
dat$caseid <- dat$level_1_id
dat$b3 <- dat$dob_dec
dat$v008 <- dat$v008_dec
# mother's dob
dat$v011 <- dat$v008 - dat$q111_comb

# Only keep live births
dat <- subset(dat, q223_aug == 1)
nrow(dat) # 75006

# append parity0 women
df_parity0 <- qsecover %>%
  filter(!(level_1_id %in% dat$level_1_id)) %>%
  filter(!is.na(qintdate) & !is.na(q111_comb)) %>%
  mutate(caseid = level_1_id,
         v008 = decimal_date(qintdate), 
         v011 = v008 - q111_comb) 

# combine
dat <- bind_rows(df_parity0, dat)
nrow(dat) # 82462

# merge on weights
nrow(dat)
dat <- merge(dat, wt, by = "grappe")
nrow(dat)

# weights
dat$v005 <- dat$wt
# clusters
dat$v021 <- dat$grappe
# byvar
dat$qtype <- factor(dat$qtype)
dat$qlregion <- factor(dat$qlregion)
dat$strate <- factor(dat$strate)
dat$area <- factor(dat$area)
# strata
dat$v024 <- dat$strate
dat$v025 <- 1

# make births wide format, starting with b3_01
datWide <- dat %>%
  select(caseid, b3, v008, v005, v011, v021, v024, v025, qlregion, wt) %>%
  arrange(caseid, b3) %>%
  group_by(caseid) %>%
  mutate(bcount = paste0("b3_", str_pad(1:n(), 2, pad = "0"))) %>%
  pivot_wider(
    names_from = bcount,
    values_from = b3
  )
nrow(datWide)
length(unique(datWide$caseid))

# Calculate fertility -----------------------------------------------------

df_nat <- calc_tfr(datWide, tips=c(0, 5), scale = 1, origin = 0)
df_reg <- calc_tfr(datWide, by=~qlregion, tips=c(0, 5), scale = 1, origin = 0)

# Save output(s) ----------------------------------------------------------

saveRDS(df_nat, "./gen/mort/output/demog-tfr-nat.rds")
saveRDS(df_reg, "./gen/mort/output/demog-tfr-reg.rds")


