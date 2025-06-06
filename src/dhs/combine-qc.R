################################################################################
#' @description Combine br and gr quality checks
#' @return 
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(haven)
library(tidyverse)
#' Inputs
source("./src/utils.R")
dob_br <- read.csv("./gen/dhs/temp/dhs-br-dob-heaping.csv")
dob_gr <- read.csv("./gen/dhs/temp/dhs-gr-dob-heaping.csv")
aad_br <- read.csv("./gen/dhs/temp/dhs-br-aad-heaping.csv")
aad_gr <- read.csv("./gen/dhs/temp/dhs-gr-aad-heaping.csv")
sr_br <- read.csv("./gen/dhs/temp/dhs-br-sex-ratios.csv")
sr_gr <- read.csv("./gen/dhs/temp/dhs-gr-sex-ratios.csv")
flag_br <- read.csv("./gen/dhs/temp/dhs-br-flags.csv")
flag_gr <-read.csv("./gen/dhs/temp/dhs-gr-flags.csv")
################################################################################

dob <- rbind(dob_br, dob_gr)
dob <- dob[order(dob$SurveyId),]

aad <- rbind(aad_br, aad_gr)
aad <- aad[order(aad$SurveyId),]

sr <- rbind(sr_br, sr_gr)
sr <- sr[order(sr$SurveyId),]

flag <- rbind(flag_br, flag_gr)
flag <- flag[order(flag$SurveyId),]

# Save output(s) ----------------------------------------------------------

write.csv(dob, "./gen/dhs/output/dhs-dob-heaping.csv", row.names = FALSE)
write.csv(aad, "./gen/dhs/output/dhs-aad-heaping.csv", row.names = FALSE)
write.csv(sr, "./gen/dhs/output/dhs-sex-ratios.csv", row.names = FALSE)
write.csv(flag, "./gen/dhs/output/dhs-flags.csv", row.names = FALSE)

