################################################################################
#' @description Prep Mali DHS
#' @return 
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(haven)
library(tidyverse)
library(lubridate)
library(purrr)
#' Inputs
source("./src/utils.R")
## Mali BR modules
l_br <- readRDS("./gen/dhs/temp/br-ml-prep.rds")
################################################################################

# calculate average children ever born, children surviving, children died
dat_sbh <- imap_dfr(l_br, ~ {
  .x %>%
    group_by(agecat_resp) %>%
    summarise(
      avg_total = sprintf("%0.2f", round(mean(sum_cebcd, na.rm = TRUE), 2)),
      avg_surv = sprintf("%0.2f", round(mean(q203_comb + q205_comb, na.rm = TRUE), 2)),
      avg_died = sprintf("%0.2f", round(mean(q207_comb, na.rm = TRUE), 2)),
      .groups = "drop"
    ) %>%
    mutate(source = .y)  # .y is the name of the list element
})

# births and deaths by years prior to survey
# create variables for yob, yod, years prior to survey
# dob_dec, dod, tips?
# imap for not combining into one dataframe
dat_bd_tips <- imap_dfr(l_br, ~ {
  .x %>%
    mutate(aadd = as.numeric(aadd)) %>%
    mutate(dob_dec = b3_dec,
           dod = b3_dec + aadd/365.2,
           v008_dec = intdate_dec) %>%
    mutate(yob = floor(dob_dec),
           yod = floor(dod),
           yoint = floor(v008_dec)) %>%
    mutate(tips_yob = -(yoint - yob),
           tips_yod = -(yoint - yod)) %>%
    select(SurveyId, dob_dec, dod, aadd, v008_dec, yob, yod, yoint, tips_yob, tips_yod)
})


# Save output(s) ----------------------------------------------------------

saveRDS(dat_sbh, "./gen/dhs/output/mldhs-sbh.rds")
saveRDS(dat_bd_tips, "./gen/dhs/output/mldhs-bd-tips.rds")
