################################################################################
#' @description Quality check BR
#' @return 
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(haven)
library(tidyverse)
#' Inputs
source("./src/utils.R")
## GR modules
l_br <- readRDS("./gen/dhs/temp/br-prep.rds")
## Analytical sample
sample <- read.csv("./gen/dhs/output/surveys-ctry-regions.csv")
################################################################################

# heaping of day of birth measured by relative difference of number of 
# births on first day of month compared to expected given number of times that day appears in year
l_reldifdob <- lapply(l_br, fn_reldifdob)
df_reldifdob <- plyr::ldply(l_reldifdob, .id = "SurveyId")
df_reldifdob$variable <- "reldifdob"

# heaping of age at death at 7 days, measured by the relative
# difference of the number of deaths at 7 days from the average for days 5-9, multiplied by 100.
l_reldif7d <- lapply(l_br, fn_reldif7d)
df_reldif7d <- plyr::ldply(l_reldif7d, .id = "SurveyId")
df_reldif7d$variable <- "reldif7d"
names(df_reldif7d)[which(names(df_reldif7d) == "aadd")] <- "aad"
df_reldif7d$aad_unit <- "d"

# heaping of age at death at 12 months, measured by the relative
# difference of the number of deaths at 12 months from the average for months 10-14, multiplied by 100.
l_reldif12m <- lapply(l_br, fn_reldif12m)
df_reldif12m <- plyr::ldply(l_reldif12m, .id = "SurveyId")
df_reldif12m$variable <- "reldif12m"
names(df_reldif12m)[which(names(df_reldif12m) == "aadm")] <- "aad"
df_reldif12m$aad_unit <- "m"

# combine
df_dev <- rbind(df_reldif7d, df_reldif12m)

# sex ratio at birth
l_srb <- lapply(l_br, fn_srb)
df_srb <- plyr::ldply(l_srb, .id = "SurveyId")
# merge on region
df_srb <- merge(df_srb, sample[,c("SurveyId", "UNICEFReportRegion1")], by = "SurveyId", all.x = TRUE)
df_srb$expected <- ifelse(df_srb$UNICEFReportRegion1 == "Sub-Saharan Africa", 103/100, 105/100)
df_srb$dev <- (df_srb$srb - df_srb$expected)/(df_srb$expected)*100
df_srb$variable <- "SRB"
names(df_srb)[which(names(df_srb) == "srb")] <- "value"

# sex ratio of neonatal deaths
l_srd <- lapply(l_br, fn_srd)
df_srd <- plyr::ldply(l_srd, .id = "SurveyId")
# merge on region
df_srd <- merge(df_srd, sample[,c("SurveyId", "UNICEFReportRegion1")], by = "SurveyId", all.x = TRUE)
df_srd$expected <- 150/100
df_srd$dev <- (df_srd$srd - df_srd$expected)/(df_srd$expected)*100
df_srd$variable <- "SRD"
names(df_srd)[which(names(df_srd) == "srd")] <- "value"

# combine
df_sr <- rbind(df_srb, df_srd)

# percent of infant deaths that are neonatal
l_pneo <- lapply(l_br, fn_pneo)
df_pneo <- plyr::ldply(l_pneo, .id = "SurveyId")

# imputation of dob (b10)
l_dob_flag <- lapply(l_br, fn_dob_flag)
df_dob_flag <- plyr::ldply(l_dob_flag, .id = "SurveyId")
df_dob_flag <- df_dob_flag %>%
  group_by(SurveyId, tips) %>%
  mutate(total = sum(n),
         per = n/total)
df_dob_flag$flag_val <- as.character(df_dob_flag$b10)
df_dob_flag$b10 <- NULL
df_dob_flag$variable <- "dob flag"

# imputation of aad (b13)
l_aad_flag <- lapply(l_br, fn_aad_flag)
df_aad_flag <- plyr::ldply(l_aad_flag, .id = "SurveyId")
df_aad_flag <- df_aad_flag %>%
  group_by(SurveyId, tips) %>%
  mutate(total = sum(n),
         per = n/total)
df_aad_flag$flag_val <- as.character(df_aad_flag$b13)
df_aad_flag$b13 <- NULL
df_aad_flag$variable <- "aad flag"

df_flag <- rbind(df_dob_flag, df_aad_flag)

# day of birth (b17, introduced in DHS 7)


# Save output(s) ----------------------------------------------------------

write.csv(df_reldifdob, "./gen/dhs/temp/dhs-br-dob-heaping.csv", row.names = FALSE)
write.csv(df_dev, "./gen/dhs/temp/dhs-br-aad-heaping.csv", row.names = FALSE)
write.csv(df_sr, "./gen/dhs/temp/dhs-br-sex-ratios.csv", row.names = FALSE)
write.csv(df_flag, "./gen/dhs/temp/dhs-br-flags.csv", row.names = FALSE)

