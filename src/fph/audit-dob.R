################################################################################
#' @description Audit completeness of DOB variable
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
dat <- readRDS("./gen/fph/temp/fph-clean.rds")
################################################################################

# All the variables which should contain information about imputations are missing
# Date de naissance/fin de grossesse (CMC)
unique(dat$q220c)  # all NA
# Imputé Jour de naissance/fin de grossesse
unique(dat$q220di) # all NA
# Indicateur de date pour/Date flag for Q220C
unique(dat$q220f)  # all NA
# Âge au décès mois (imputed)
unique(dat$q228c) # all NA
# Indicateur de date pour Q228C
unique(dat$q228f) # all NA

# Audit of dob - imputed month or day
n_total <- nrow(dat)
no_missing <- nrow(subset(dat, !is.na(q220y) & q220y != 98 & !is.na(q220m) & q220m != 98 & !is.na(q220d) & q220d != 98))
miss_ymd <-  nrow(subset(dat, (is.na(q220y) | q220y == 98) & (is.na(q220m) | q220m == 98) & (is.na(q220d) | q220d == 98)))
miss_ym <-  nrow(subset(dat, (is.na(q220y) | q220y == 98) & (is.na(q220m) | q220m == 98) & (!is.na(q220d) & q220d != 98)))
miss_yd <-  nrow(subset(dat, (is.na(q220y) | q220y == 98) & (!is.na(q220m) & q220m != 98) & (is.na(q220d) | q220d == 98)))
miss_md <-  nrow(subset(dat, (!is.na(q220y) & q220y != 98) & (is.na(q220m) | q220m == 98) & (is.na(q220d) | q220d == 98)))
miss_y <-  nrow(subset(dat, (is.na(q220y) | q220y == 98) & (!is.na(q220m) & q220m != 98) & (!is.na(q220d) & q220d != 98)))
miss_m <-  nrow(subset(dat, (!is.na(q220y) & q220y != 98) & (is.na(q220m) | q220m == 98) & (!is.na(q220d) & q220d != 98)))
miss_d <-  nrow(subset(dat, (!is.na(q220y) & q220y != 98) & (!is.na(q220m) & q220m != 98) & (is.na(q220d) | q220d == 98)))
no_missing + miss_ymd + miss_ym + miss_yd + miss_md + miss_y + miss_m + miss_d == nrow(dat)

dat_aud1 <- data.frame(variable = c("total",
                                    "dob complete", 
                                    "dob missing ymd",
                                    "dob missing ym",
                                    "dob missing yd",
                                    "dob missing md",
                                    "dob missing y",
                                    "dob missing m",
                                    "dob missing d"),
                       n = c(n_total, no_missing, miss_ymd, miss_ym, miss_yd, miss_md, miss_y, miss_m, miss_d))
sum(subset(dat_aud1, variable != "total")$n) == nrow(dat)
subset(dat_aud1, variable == "total")$n == nrow(dat)
write.csv(dat_aud1, paste0("./gen/fph/audit/dat_aud-dob1_", format(Sys.Date(), "%Y%m%d"), ".csv"), row.names = FALSE)

# check if current age of child is reported when dob year is missing
# miss_ymd
subset(dat, (is.na(q220y) | q220y == 98) & (is.na(q220m) | q220m == 98) & (is.na(q220d) | q220d == 98))$q225
sum(!is.na(subset(dat, (is.na(q220y) | q220y == 98) & (is.na(q220m) | q220m == 98) & (is.na(q220d) | q220d == 98))$q225)) # 2
# miss_y
subset(dat, (is.na(q220y) | q220y == 98) & (!is.na(q220m) & q220m != 98) & (!is.na(q220d) & q220d != 98))$q225
# none

# Audit of dob at individual-level to get missing per year
dat_aud2 <- dat %>%
  mutate(
    dob_type = case_when(
      !is.na(q220y) & q220y != 98 &
        !is.na(q220m) & q220m != 98 &
        !is.na(q220d) & q220d != 98 ~ "complete",
      
      (is.na(q220y) | q220y == 98) &
        (is.na(q220m) | q220m == 98) &
        (is.na(q220d) | q220d == 98) ~ "missing_ymd",
      
      (is.na(q220y) | q220y == 98) &
        (is.na(q220m) | q220m == 98) &
        (!is.na(q220d) & q220d != 98) ~ "missing_ym",
      
      (is.na(q220y) | q220y == 98) &
        (!is.na(q220m) & q220m != 98) &
        (is.na(q220d) | q220d == 98) ~ "missing_yd",
      
      (!is.na(q220y) & q220y != 98) &
        (is.na(q220m) | q220m == 98) &
        (is.na(q220d) | q220d == 98) ~ "missing_md",
      
      (is.na(q220y) | q220y == 98) &
        (!is.na(q220m) & q220m != 98) &
        (!is.na(q220d) & q220d != 98) ~ "missing_y",
      
      (!is.na(q220y) & q220y != 98) &
        (is.na(q220m) | q220m == 98) &
        (!is.na(q220d) & q220d != 98) ~ "missing_m",
      
      (!is.na(q220y) & q220y != 98) &
        (!is.na(q220m) & q220m != 98) &
        (is.na(q220d) | q220d == 98) ~ "missing_d",
      
      TRUE ~ "other"
    )
  ) %>%
  select(qwsec2b_id, q219, q220y, q220m, q220d, q228, dob_type, q216, q223)
table(dat_aud2$dob_type, useNA = "always")
write.csv(dat_aud2, paste0("./gen/fph/audit/dat_aud-dob2_", format(Sys.Date(), "%Y%m%d"), ".csv"), row.names = FALSE)

