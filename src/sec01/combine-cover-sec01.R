################################################################################
#' @description Combine qsecover and qwsec01
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
level1 <- readRDS("./gen/sec01/temp/level1-clean.rds")
qsecover <- readRDS("./gen/sec01/temp/qsecover-clean.rds")
qwsec01 <- readRDS("./gen/sec01/temp/qwsec01-age.rds")
################################################################################

head(level1)
head(qsecover)
head(qwsec01)
nrow(level1) #  26452
nrow(qsecover) # 26452
nrow(qwsec01) # 26393

dat <- merge(level1, qsecover, by = "level_1_id")
nrow(dat) # 26452

dat <- merge(dat, qwsec01, by = "level_1_id", all.x = TRUE)
nrow(dat) # 26452

# Save --------------------------------------------------------------------

saveRDS(dat, "./gen/sec01/output/qsecover-qwsec01.rds")
