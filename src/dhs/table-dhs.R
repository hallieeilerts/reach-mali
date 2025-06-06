################################################################################
#' @description DHS sample table
#' @return 
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(haven)
library(tidyverse)
library(kableExtra)
#' Inputs
source("./src/utils.R")
## Analytical sample
sample <- read.csv("./gen/dhs/output/surveys-ctry-regions.csv")
################################################################################

range(sample$SurveyYear)
range(subset(sample, incl_gr == 1 | incl_br == 1)$SurveyYear) # 2015-2023
nrow(subset(sample, incl_gr == 1 | incl_br == 1))


sample %>%
  filter(incl_gr == 1 | incl_br == 1) %>%
  arrange(country_name, SurveyYear) %>%
  mutate(FPH = ifelse(incl_gr == 1, "Yes", "")) %>%
  select(country_name, SurveyId, SurveyYear, FPH) %>%
  kbl(
    format = "latex",
    booktabs = TRUE,
    row.names = FALSE,
    linesep = "",
    col.names = c("Country", "Survey ID", "Survey Year", "FPH"),
    longtable = TRUE,
    threeparttable = TRUE, 
    caption = "DHS surveys included in comparison sample."
  ) %>%
  kable_styling(latex_options = c("repeat_header")) %>%
  footnote(
    general = "FPH = Survey includes Full Pregnancy History",
    footnote_as_chunk = TRUE
  )
