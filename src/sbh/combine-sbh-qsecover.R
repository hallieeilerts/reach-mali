################################################################################
#' @description Merge SBH with information from cover and section 1
#' @return Drop those where interview wasn't completed
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
sbh <- read.csv("./data/reach_mortalite_femme_qwsec2a.csv")
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
         q113, q114, q115, q131, q132, wco3)

dat <- merge(qsec, sbh, by = "level_1_id")
nrow(sbh) == nrow(dat)

# check if interview complete
table(dat$wco3, useNA = "always")
# for those with "Logement détruit ou Autre" or "C'est pas la bonne personne", it looks like the SBH questions weren't asked
#View(subset(dat, wco3 == "Logement détruit ou Autre"))
#View(subset(dat, wco3 == "C'est pas la bonne personne"))
# for those with wco3 missing, it's a mix. sometimes questions asked, sometimes no.
#View(subset(dat, is.na(wco3)))

# when the first SBH question is missing, looks like interview wasn't completed or there is an NA value for wco3 (indicating lack of completion)
#View(subset(dat, is.na(q201)))
table(subset(dat, is.na(q201))$wco3, useNA = "always")

# Drop these ones
dat <- subset(dat, !is.na(q201))
nrow(dat) # 26102
# now wco3 is always either "complet" or NA
table(dat$wco3, useNA = "always")

# Save --------------------------------------------------------------------

saveRDS(dat, "./gen/sbh/temp/sbh-qsecover-qwsec01.rds")

