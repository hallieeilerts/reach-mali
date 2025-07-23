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
library(fuzzyjoin)
#' Inputs
source("./src/utils.R")
## Mali BR modules
l_br <- readRDS("./gen/dhs/temp/br-ml-prep.rds")
l_ir <- readRDS("./gen/dhs/temp/ir-ml-prep.rds")
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

# calculate average children ever born, children surviving, children died by region
dat_sbh_reg <- imap_dfr(l_br[-1], ~ {
  .x %>%
    group_by(v024, agecat_resp) %>%
    mutate(v024 = as.character(as_factor(v024))) %>%
    summarise(
      avg_total = sprintf("%0.2f", round(mean(sum_cebcd, na.rm = TRUE), 2)),
      avg_surv = sprintf("%0.2f", round(mean(q203_comb + q205_comb, na.rm = TRUE), 2)),
      avg_died = sprintf("%0.2f", round(mean(q207_comb, na.rm = TRUE), 2)),
      .groups = "drop"
    ) %>%
    mutate(source = .y)  # .y is the name of the list element
})

# average parity by region (not by age)
# does not include women with zero births
dat_sbh_parity <- imap_dfr(l_br[-1], ~ {
  .x %>%
    group_by(v024) %>%
    mutate(v024 = as.character(as_factor(v024))) %>%
    summarise(
      avg_total = mean(sum_cebcd, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(source = .y)  # .y is the name of the list element
})

# proportion of children ever born by age of mother
# using IR because BR only includes those that have any births
dat_sbh_prop_reg <- imap_dfr(l_ir[-1], ~ {
  .x %>%
    group_by(v024, agecat_resp) %>%
    mutate(v024 = as.character(as_factor(v024)),
           v201 = as.character(as_factor(v201))) %>%
    mutate(v201_cat = cut(as.numeric(v201), breaks = c(-Inf, 0,1,2,3,4,5,6,Inf),
                          labels = c("0","1","2","3","4","5","6", "7+"))) %>%
    group_by(v024, agecat_resp, v201_cat) %>% 
    summarise(n = n()) %>%
    group_by(v024, agecat_resp) %>%
    mutate(total = sum(n),
              per = n/total)  %>%
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


dat_ir_tips <- imap_dfr(l_ir, ~ {
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

# Cohort comparison
# Respondent ages in previous surveys with reference to 2025
v_agegrp <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49")
v_survyear <- as.numeric(substr(names(l_br),3, 6))
# age with reference to 2018
age_df <- tibble(age_group2025 = v_agegrp) %>%
  separate(age_group2025, into = c("age_low", "age_high"), sep = "-", convert = TRUE) %>%
  mutate(birth_year_start = 2025 - age_high,
         birth_year_end   = 2025 - age_low,
         age_group2025 = v_agegrp)
cohort_ages <- expand_grid(
  age_df,
  survey_year = v_survyear
) %>%
  mutate(
    age_start = survey_year - birth_year_end,
    age_end   = survey_year - birth_year_start
  ) %>%
  filter(age_start >= 15) %>%
  select(age_group2025, survey_year, age_start, age_end)
mysurvs <- unique(cohort_ages$survey_year)
l_res <- list()
for(i in 1:length(mysurvs)){
  
  mysurv <- mysurvs[i]
  myages <- subset(cohort_ages, survey_year %in% mysurv)
  myages_expanded <- myages %>%
    rowwise() %>%
    mutate(age_vals = list(age_start:age_end)) %>%
    unnest(age_vals) %>%
    rename(age_resp = age_vals)
  df_surv <- l_br[grepl(mysurv, names(l_br))][[1]]
  df_sbh_cohort <- df_surv %>%
    mutate(age_resp = v012) %>%
    left_join(myages_expanded, by = "age_resp") %>%
    filter(age_resp >= age_start & age_resp <= age_end) %>%
    mutate(agecat_resp = paste0(age_start, "-", age_end)) %>%
    group_by(age_group2025, agecat_resp, v024) %>%
    mutate(v024 = as.character(as_factor(v024))) %>%
    summarise(
      avg_total = sprintf("%0.2f", round(mean(sum_cebcd, na.rm = TRUE), 2)),
      avg_surv = sprintf("%0.2f", round(mean(q203_comb + q205_comb, na.rm = TRUE), 2)),
      avg_died = sprintf("%0.2f", round(mean(q207_comb, na.rm = TRUE), 2)),
      .groups = "drop"
    ) %>%
    mutate(SurveyYear = mysurv)
  
  l_res[[i]] <- df_sbh_cohort
  
}
dat_sbh_cohort <- do.call(rbind, l_res)
dat_sbh_cohort$source <- paste0("ML", dat_sbh_cohort$SurveyYear, "DHS")



# Save output(s) ----------------------------------------------------------

saveRDS(dat_sbh, "./gen/dhs/output/mldhs-sbh.rds")
saveRDS(dat_sbh_reg, "./gen/dhs/output/mldhs-sbh-reg.rds")
saveRDS(dat_bd_tips, "./gen/dhs/output/mldhs-bd-tips.rds")
saveRDS(dat_sbh_prop_reg, "./gen/dhs/output/mldhs-sbh-prop-reg.rds")
saveRDS(dat_sbh_cohort, "./gen/dhs/output/mldhs-sbh-cohort-reg.rds")
saveRDS(dat_sbh_parity, "./gen/dhs/output/mldhs-sbh-parity-reg.rds")
