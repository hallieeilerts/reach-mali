################################################################################
#' @description Comparison of hh deaths in last 6m with fph. so far cant link these at household level
#' @return None
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(kableExtra)
library(stringr)
library(haven)
library(readxl)
# household forms
hh_level1 <- read.csv("./data/reach_hh_level1.csv")
hhident <- read.csv("./data/reach_hh_identification_rec.csv") # exist_dece
#hhlist <- read.csv("./data/reach_hh_liste_menage.csv") # liste_menage_id
# sampling weights
wt1 <- read_excel("./data/instat-20250623/Poids_Enquete_Base_Mortalite.xlsx", sheet = "Poidsformule")
wt2 <- read_excel("./data/instat-20250623/Poids_Enquete_Base_Mortalite.xlsx", sheet = "Poidsvf")
# FPH
#denomb_level1 <- read.csv("./data/reach_mortalite_denomb_level1.csv")
#sec01 <- readRDS("./gen/sec01/output/qsecover-qwsec01.rds")
fph <- readRDS("./gen/fph/output/fph-tips.rds")
################################################################################

nrow(hh_level1) # 170403
nrow(hhlist) # 931534
nrow(hhident) # 170403
hh <- merge(hh_level1[,c("level_1_id", "hh_ea")], hhident, by = "level_1_id")
hh$myhhdate <- as.Date(paste(hh$hh_anne, hh$hh_mois, hh$hh_jour, sep = "-"), format = "%Y-%m-%d")
# df_hhlist <- hhlist[,c("level_1_id", "liste_menage_id")]
# df_hhlist <- df_hhlist[!duplicated(df_hhlist),]
# nrow(df_hhlist)
# hh <- merge(hh, df_hhlist, by = "level_1_id")
#nrow(subset(hh, is.na(myhhdate))) # 0



dat <- merge(fph, hh, by.x = c("grappe", "qlregion",  "qdistrict", "qlcercle", "w_cons", "w_men"),
                      by.y = c("hh_ea",  "hh_lregion","hhl_district", "hhcercle", "h_")

