################################################################################
#' @description Merge SBH with information from cover and section 1
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
sbh <- readRDS("./gen/sbh/temp/sbh-clean.rds")
qsecover <- readRDS("./gen/sec01/output/qsecover-qwsec01.rds")
################################################################################

head(sbh)
# qwsec2a_id is sequential
# level_1_id is sequential as well

# head(qsecover)
# there are two options in qsecover: qsecover_id and level_1_id
# i will go with level_1_id since this has the same name

# Only keep variables of interest from qsecover-qwsec01
qsec <- qsecover %>%
  select(level_1_id, qlregion, qlcercle, qdistrict, qtype, grappe, w_cons, w_men, w_ind,
         qintdate, q111, q111_imp, q111_comb, agecat_resp,
         q113, q114, q115, q131, q132)

dat <- merge(qsec, sbh, by = "level_1_id")
nrow(sbh) == nrow(dat)

# Save --------------------------------------------------------------------

saveRDS(dat, "./gen/sbh/temp/sbh-qsecover-qwsec01.rds")

