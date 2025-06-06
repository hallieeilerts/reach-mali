################################################################################
#' @description Remove unwanted variables and save
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
dat <- readRDS("./gen/fph/temp/fph-tips.rds")
################################################################################

# Remove unnecessary variables
dat <- dat %>%
  select(-c(q228_clean, qintdate_y, qintdate_m, qintdate_d, 
            dif_d, dif_m, dif_y, earliestdob, q220m_imp,
            q220d_imp, q220_date_stub, max_day))

# Save --------------------------------------------------------------------

saveRDS(dat, "./gen/fph/output/fph-formatted.rds")





