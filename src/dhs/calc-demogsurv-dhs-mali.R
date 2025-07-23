################################################################################
#' @description Calculate mortality rates for Mali DHS
#' @return 
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(haven)
library(tidyverse)
library(lubridate)
library(purrr)
library(demogsurv)
#' Inputs
source("./src/utils.R")
## Mali BR modules
l_br <- readRDS("./gen/dhs/temp/br-ml-prep.rds")
################################################################################


## Convert to factors (a bit inefficient)
l_br <- lapply(l_br, haven::as_factor)

l_br <- lapply(l_br, function(x){x$death <- x$b5 == "no"; x})
l_br <- lapply(l_br, function(x){x$dod <- as.numeric(as.character(x$b3)) + as.numeric(as.character(x$b7)) + 0.5; x})

# putting strata as NULL because ML1987DHS doesn't have region variable
# also, not using uncertainty calculations at the moment, so not a problem to not include strata survey design
nat1 <- plyr::ldply(l_br, calc_nqx, by=~1, agegr=c(0, (28*12/365.25) )/12, strata=NULL, cluster=~v001, .id = "SurveyId") # 0-28d
nat2 <- plyr::ldply(l_br, calc_nqx, by=~1, agegr=c((28*12/365.25), 3, 5, 12)/12, strata=NULL, cluster=~v001, .id = "SurveyId")  # 28d-12m
nat3 <- plyr::ldply(l_br, calc_nqx, by=~1, agegr=c(0, (28*12/365.25), 3, 5, 12)/12, strata=NULL, cluster=~v001, .id = "SurveyId") # 0-12m
nat4 <- plyr::ldply(l_br, calc_nqx, by=~1, agegr=c(12, 24, 36, 48, 60)/12, strata=NULL, cluster=~v001, .id = "SurveyId") # 1-5y
nat5 <- plyr::ldply(l_br, calc_nqx, by=~1, agegr=c(0, (28*12/365.25), 3, 5, 12, 24, 36, 48, 60)/12, strata=NULL, cluster=~v001, .id = "SurveyId") # under-5
nat6 <- plyr::ldply(l_br, calc_nqx, by=~1, agegr=c((28*12/365.25), 3, 5, 12, 24, 36, 48, 60)/12, strata=NULL, cluster=~v001, .id = "SurveyId") # 1-59m
nat1$agegrp <- "q0to28d"
nat2$agegrp <- "q28dto1y"
nat3$agegrp <- "q0to1y"
nat4$agegrp <- "q1to5y"
nat5$agegrp <- "q0to5y"
nat6$agegrp <- "q1to59m"

# drop early survey that is missing v025
l_brshort <- l_br[-1]

res1 <- plyr::ldply(l_brshort, calc_nqx, by=~v025, agegr=c(0, (28*12/365.25) )/12, strata=NULL, cluster=~v001, .id = "SurveyId") # 0-28d
res2 <- plyr::ldply(l_brshort, calc_nqx, by=~v025, agegr=c((28*12/365.25), 3, 5, 12)/12, strata=NULL, cluster=~v001, .id = "SurveyId")  # 28d-12m
res3 <- plyr::ldply(l_brshort, calc_nqx, by=~v025, agegr=c(0, (28*12/365.25), 3, 5, 12)/12, strata=NULL, cluster=~v001, .id = "SurveyId") # 0-12m
res4 <- plyr::ldply(l_brshort, calc_nqx, by=~v025, agegr=c(12, 24, 36, 48, 60)/12, strata=NULL, cluster=~v001, .id = "SurveyId") # 1-5y
res5 <- plyr::ldply(l_brshort, calc_nqx, by=~v025, agegr=c(0, (28*12/365.25), 3, 5, 12, 24, 36, 48, 60)/12, strata=NULL, cluster=~v001, .id = "SurveyId") # under-5
res6 <- plyr::ldply(l_brshort, calc_nqx, by=~v025, agegr=c((28*12/365.25), 3, 5, 12, 24, 36, 48, 60)/12, strata=NULL, cluster=~v001, .id = "SurveyId") # 1-59m
res1$agegrp <- "q0to28d"
res2$agegrp <- "q28dto1y"
res3$agegrp <- "q0to1y"
res4$agegrp <- "q1to5y"
res5$agegrp <- "q0to5y"
res6$agegrp <- "q1to59m"

reg1 <- plyr::ldply(l_brshort, calc_nqx, by=~v024, agegr=c(0, (28*12/365.25) )/12, strata=NULL, cluster=~v001, .id = "SurveyId") # 0-28d
reg2 <- plyr::ldply(l_brshort, calc_nqx, by=~v024, agegr=c((28*12/365.25), 3, 5, 12)/12, strata=NULL, cluster=~v001, .id = "SurveyId")  # 28d-12m
reg3 <- plyr::ldply(l_brshort, calc_nqx, by=~v024, agegr=c(0, (28*12/365.25), 3, 5, 12)/12, strata=NULL, cluster=~v001, .id = "SurveyId") # 0-12m
reg4 <- plyr::ldply(l_brshort, calc_nqx, by=~v024, agegr=c(12, 24, 36, 48, 60)/12, strata=NULL, cluster=~v001, .id = "SurveyId") # 1-5y
reg5 <- plyr::ldply(l_brshort, calc_nqx, by=~v024, agegr=c(0, (28*12/365.25), 3, 5, 12, 24, 36, 48, 60)/12, strata=NULL, cluster=~v001, .id = "SurveyId") # under-5
reg6 <- plyr::ldply(l_brshort, calc_nqx, by=~v024, agegr=c((28*12/365.25), 3, 5, 12, 24, 36, 48, 60)/12, strata=NULL, cluster=~v001, .id = "SurveyId") # 1-59m
reg1$agegrp <- "q0to28d"
reg2$agegrp <- "q28dto1y"
reg3$agegrp <- "q0to1y"
reg4$agegrp <- "q1to5y"
reg5$agegrp <- "q0to5y"
reg6$agegrp <- "q1to59m"

df_nat <- rbind(nat1, nat2, nat3, nat4, nat5, nat6)
df_reg <- rbind(reg1, reg2, reg3, reg4, reg5, reg6)
df_res <- rbind(res1, res2, res3, res4, res5, res6)

# Save output(s) ----------------------------------------------------------

saveRDS(df_nat, "./gen/dhs/output/mldhs-rates-nat.rds")
saveRDS(df_reg, "./gen/dhs/output/mldhs-rates-reg.rds")
saveRDS(df_res, "./gen/dhs/output/mldhs-rates-res.rds")

