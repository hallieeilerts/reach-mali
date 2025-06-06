################################################################################
#' @description Merge FPH with information from cover and section 1
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
fph <- readRDS("./gen/fph/temp/fph-clean.rds")
qsecover <- readRDS("./gen/sec01/output/qsecover-qwsec01.rds")
################################################################################

# head(fph)
# qwsec2b_id is sequential
# level_1_id must be the linkage

# head(qsecover)
# there are two options in qsecover: qsecover_id and level_1_id
# i will go with level_1_id since this has the same name

# Only keep variables of interest from qsecover-qwsec01
qsec <- qsecover %>%
  select(level_1_id, qlregion, qlcercle, qdistrict, qtype, grappe, w_cons, w_men, w_ind,
         qintdate, q111, q111_imp, q111_comb, agecat_resp,
         q113, q114, q115, q131, q132)

dat <- merge(qsec, fph, by = "level_1_id")
nrow(fph) == nrow(dat)

# Save --------------------------------------------------------------------

saveRDS(dat, "./gen/fph/temp/fph-qsecover-qwsec01.rds")
