################################################################################
#' @description Check reporting of variables in birth (dhs7 and dhs8) and pregnancy recode modules
#' @return
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(data.table)
library(haven)
library(tidyverse)
#' Inputs
## BR modules
l_br <- readRDS("./data/dhs/br.rds")
## GR modules
l_gr <- readRDS("./data/dhs/gr.rds")
################################################################################

# BR -----------------------------------------------------------------------

# Get phase
l_phase <- lapply(l_br, function(x) substr(as.character(x$v000[1]),3,3))
df_phase <- plyr::ldply(l_phase, .id = "SurveyId")
df_phase$phase <- df_phase$V1
df_phase$V1 <- NULL
df_phase$phase[df_phase$phase == ""] <- 1

# Get survey year
l_surveyyear <- lapply(l_br, function(x) unique(x$SurveyYear))
df_surveyyear <- plyr::ldply(l_surveyyear, .id = "SurveyId")
df_surveyyear$SurveyYear <- df_surveyyear$V1
df_surveyyear$V1 <- NULL

# Has day of birth
l_b17 <- lapply(l_br, function(x) sum(!is.na(x$b17)) > 0 )
df_b17 <- plyr::ldply(l_b17, .id = "SurveyId")
df_b17$hasdayofb <- df_b17$V1
df_b17$V1 <- NULL

df_br_var <- merge(df_phase, df_surveyyear, by = "SurveyId", all = TRUE)
df_br_var <- merge(df_br_var, df_b17, by = "SurveyId", all = TRUE)
df_br_var$SurveyId <- as.character(df_br_var$SurveyId)

# GR ----------------------------------------------------------------------

# Check if dataset has p32 (FPH pregnancy outcome question)
l_p32 <- lapply(l_gr, function(x) "p32" %in% names(x))
df_p32 <- plyr::ldply(l_p32, .id = "SurveyId")
df_p32 <- subset(df_p32, V1 == TRUE)
df_p32$p32 <- df_p32$V1
df_p32$V1 <- NULL

# Merge -------------------------------------------------------------------

df_data <- merge(df_br_var, df_p32, by = c("SurveyId"), all =TRUE)
df_data$p32[is.na(df_data$p32)] <- FALSE
df_data <- df_data[order(df_data$SurveyId),]

df_data$incl_gr <- ifelse(df_data$p32 == TRUE, 1, 0)
df_data$incl_br <- ifelse(df_data$incl_gr == 0 & df_data$SurveyYear >= 2015, 1, 0)

sum(df_data$incl_br, na.rm  = TRUE) # 55
sum(df_data$incl_gr, na.rm  = TRUE) # 6


# Save output(s) ----------------------------------------------------------

write.csv(df_data, "./gen/dhs/temp/surveys.csv", row.names = FALSE)

