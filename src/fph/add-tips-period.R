################################################################################
#' @description Add tips and period labels (make sure this follows methods in function fn_gen_tips)
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(lubridate)
#' Inputs
dat <- readRDS("./gen/fph/temp/fph-impute-dob.rds")
################################################################################

nrow(subset(dat, is.na(dob_dec))) # 0 
nrow(subset(dat, is.na(v008_dec))) # 0

# create time prior to survey for dob
# rowwise for different breaks by row
dat <- dat %>%
  rowwise() %>%
  mutate(
    tips = {
        # Create custom breaks for this row
        breaks <- c(-Inf, v008_dec - 15, v008_dec - 10, v008_dec - 5, Inf)
        as.character(cut(dob_dec, breaks = breaks, labels = c(">15", "10-14", "5-9", "0-4"), include.lowest = TRUE))
      }
  ) %>%
  ungroup()
nrow(subset(dat, is.na(tips))) # 0

# create period for dob
surveyyear <- 2025
surveyyear_breaks <- c(-Inf, surveyyear+1 - 15, surveyyear+1 - 10, surveyyear+1 - 5, surveyyear+1)
surveyyear_labs <- c( paste0("<",surveyyear_breaks[2]),
                      paste0("[", surveyyear_breaks[2], ", ", surveyyear_breaks[3], ")"),
                      paste0("[", surveyyear_breaks[3], ", ", surveyyear_breaks[4], ")"),
                      paste0("[", surveyyear_breaks[4], ", ", surveyyear+1, ")") )
dat$period <- cut(dat$dob_dec, breaks = surveyyear_breaks, labels = surveyyear_labs)


# Save --------------------------------------------------------------------

saveRDS(dat, "./gen/fph/output/fph-tips.rds")

