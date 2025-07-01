################################################################################
#' @description Recover missing year of birth where possible from current age of child
#' Drop those missing year of birth and pregnancy outcomes
#' @return (i) file with no missing pregnancy outcomes or years of birth
#' (ii) dat_aud-pregdoby: number of dropped observations due to missing preg outcome/missing year of birth
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(lubridate)
#' Inputs
dat <- readRDS("./gen/fph/temp/fph-qsecover-qwsec01.rds")
################################################################################

nrow(dat) # 80094
nrow(dat) == length(unique(dat$qwsec2b_id)) # unique identifier
length(unique(dat$level_1_id)) # should be mother identifier

# Indicator for event (death)
table(dat$q224, useNA = "always")
dat$event <- ifelse(dat$q224 == 2, 1, 0)
table(dat$event, useNA = "always")
# check that when event is missing, it was not a live birth (q223_aug != 1)
table(subset(dat, is.na(event))$q223_aug, useNA =  "always")

# Year of birth is missing
nrow(subset(dat, is.na(q220y))) # 50
# check if it can be recovered from current age (q225)
dat %>%
  filter(is.na(q220y) & !is.na(q225)) %>%
  select(q223, q220y, q225, q223_aug)
# yes, there are two live births (q223_aug) for which recovery is possible
# Recover when possible from current age (q225)
dat <- dat %>%
  mutate(q220y = case_when(
    !is.na(q225) & is.na(q220y) & q223_aug == 1 ~ as.numeric(format(qintdate, "%Y")) - q225,
    TRUE ~ q220y
  ))
nrow(subset(dat, is.na(q220y))) # 48

# audit
# missing year of birth
n_missingyob <- nrow(subset(dat, is.na(q220y)))
# missing pregnancy outcome
n_missingpreg <- nrow(subset(dat, is.na(q223_aug)))
# missing both
n_missingpregyob <- nrow(subset(dat, is.na(q220y) & is.na(q223_aug)))
# missing pregnancy outcome and not year of birth
n_missingpreg_notyob <- nrow(subset(dat, is.na(q223_aug) & !is.na(q220y)))
# missing year of birth and not pregnancy outcome
n_missingyob_notpreg <- nrow(subset(dat, !is.na(q223_aug) & is.na(q220y)))
# missing either
n_missingyoborpreg <- nrow(subset(dat, is.na(q223_aug) | is.na(q220y)))

# drop observations that are still missing year of birth or pregnancy outcome
dat <- subset(dat, !is.na(q220y) & !is.na(q223_aug))
nrow(dat) # 79773

# Save audit --------------------------------------------------------------

dat_aud_pregyob <- data.frame(variable = c("missing yob", "missing preg outcome", "missing yob and preg outcome", 
                                            "missing preg, not yob", "missing yob, not preg",
                                           "missing either - dropped"),
                               n = c(n_missingyob, n_missingpreg, n_missingpregyob, n_missingpreg_notyob, n_missingyob_notpreg, n_missingyoborpreg))
write.csv(dat_aud_pregyob, paste0("./gen/fph/audit/dat_aud-pregyob_", format(Sys.Date(), "%Y%m%d"), ".csv"), row.names = FALSE)

# Save --------------------------------------------------------------------

saveRDS(dat, "./gen/fph/temp/fph-recover-doby.rds")

