################################################################################
#' @description Calculate aad heaping
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
l_br <- readRDS("./data/dhs/br.rds")
l_gr <- readRDS("./data/dhs/gr.rds")
## Analytical sample
sample <- read.csv("./gen/dhs/output/surveys-ctry-regions.csv")
################################################################################

#v_samp <- subset(sample, incl_gr == 1)$SurveyId
#l_gr_samp <- l_gr[names(l_gr) %in% v_samp]

v_samp <- subset(sample, incl_br == 1)$SurveyId
l_br_samp <- l_br[names(l_br) %in% v_samp]
rm(l_br)

# Clean br ----------------------------------------------------------------

# Create age at death
l_br_samp <- lapply(l_br_samp, fn_gen_aad)

# Create time prior to survey and period variables
l_br_samp <- lapply(l_br_samp, fn_gen_tips)


# Save output(s) ----------------------------------------------------------

saveRDS(l_br_samp, "./gen/dhs/temp/br-prep.rds")





#View(x[,c("b6", "b6_num", "aad_unit", "aad_value", "aadm", "aadd")])

### b1 month of birth
### b2 year of birth
### b5 child is alive
### b6 age at death
### b7 age at death (months imputed)
### b10 flag for date of birth
# 0 Month, year and day
# 1 Month and year - information complete
# 2 Month and age - year imputed
# 3 Year and age - month imputed
# 4 Year and age - year ignored
# 5 Year - age/month imputed
# 6 Age - year/month imputed
# 7 Month - age/year imputed
# 8 None - all imputed

### b13 flag for age at death
# 7 Age at death was imputed, however the units were given
# 8 Age at death was imputed, no units were given
### b17 day of birth (introduced in DHS 7)

