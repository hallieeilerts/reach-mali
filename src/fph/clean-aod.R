################################################################################
#' @description Adjust values for impossible ages of death according to rules,
#' Drop those with impossible AOD
#' @return (i) file with no impossible ages of death
#' (ii) dat_aud-aad: number of dropped observations due to impossible AOD
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(lubridate)
#' Inputs
dat <- readRDS("./gen/fph/temp/fph-recover-doby.rds")
################################################################################

# q228 Age au décès
# Replace blank pace with 0
dat$q228_clean <- gsub(" ", "0", dat$q228)
dat$q228_clean <- as.numeric(as.character(dat$q228_clean))
unique(dat$q228)
unique(dat$q228_clean)
nrow(subset(dat, !is.na(q228))) # 6921
nrow(subset(dat, !is.na(q228_clean))) # 6921

# Redistribute explicitly missing values in q228
nrow(subset(dat, !is.na(dat$q228_clean) & q228_clean >= 190 & q228_clean < 200)) # 0
nrow(subset(dat, !is.na(dat$q228_clean) & q228_clean >= 290 & q228_clean < 300)) # 0
nrow(subset(dat, !is.na(dat$q228_clean) & q228_clean >= 390 & q228_clean < 400)) # 0
nrow(subset(dat, !is.na(dat$q228_clean) & q228_clean >= 900)) # 0
# None are coded like this. So no need.
#dat$q228_clean[!is.na(dat$q228_clean) & dat$q228_clean >= 190 & dat$q228_clean < 200] <- sample(100:130, 1)
#dat$q228_clean[!is.na(dat$q228_clean) & dat$q228_clean >= 290 & dat$q228_clean < 300] <- sample(200:223, 1)
#dat$q228_clean[!is.na(dat$q228_clean) & dat$q228_clean >= 390 & dat$q228_clean < 400] <- sample(300:334, 1)
#dat$q228_clean[!is.na(dat$q228_clean) & dat$q228_clean >= 900] <- sample(c(100:130, 200:223, 300:334), 1)

# Check if q228 is ever missing for those that died
nrow(subset(dat, event == 1 & is.na(q228_clean))) # 0

# Deconstruct q228 into unit and value
dat$aad_unit <- trunc(dat$q228_clean/100)
dat$aad_value <- dat$q228_clean - dat$aad_unit * 100
# Transform into months
dat$aadm <- dat$aad_value
dat$aadm[which(dat$aad_unit == 1)] <- dat$aadm[which(dat$aad_unit == 1)] / 30.5
dat$aadm[which(dat$aad_unit == 3)] <- dat$aadm[which(dat$aad_unit == 3)]*12
# Transform into days
dat$aadd <- dat$aad_value
dat$aadd[which(dat$aad_unit == 2)] <- dat$aadd[which(dat$aad_unit == 2)] * 30.5
dat$aadd[which(dat$aad_unit == 3)] <- dat$aadd[which(dat$aad_unit == 3)] * 365.25
# Transform into years
dat$aady <- dat$aad_value
dat$aady[which(dat$aad_unit == 1)] <- dat$aady[which(dat$aad_unit == 1)] / 365.25
dat$aady[which(dat$aad_unit == 2)] <- dat$aady[which(dat$aad_unit == 2)] / 12

#View(dat[,c("event", "q228", "q228_clean","aad_unit", "aad_value", "aady", "aadm", "aadd")])

# Format interview date
dat$qintdate_y <- lubridate::year(dat$qintdate)
dat$qintdate_m <- lubridate::month(dat$qintdate)
dat$qintdate_d <- lubridate::day(dat$qintdate)

nrow(subset(dat, !is.na(aad_unit))) # 6921
nrow(subset(dat, !is.na(aad_value))) # 6921
nrow(subset(dat, !is.na(aadd))) # 6921
nrow(subset(dat, !is.na(aadm))) # 6921
nrow(subset(dat, !is.na(aady))) # 6921

# check years of death
# For those with complete DOBs (where the dob string successfully parses),
# the distribution looks pretty normal with tapering in the latest year
dat %>%
  filter(!is.na(aadd)) %>% 
  mutate(dob = as.Date(paste(q220y, q220m, q220d, sep = "-"), format = "%Y-%m-%d"),
         dod = dob + aadd) %>%
  mutate(yod_fl = floor(decimal_date(dod))) %>%
  group_by(yod_fl) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=yod_fl, y=n), stat = "identity")
# However, when I include all (just by using year of birth) there are a lot of DODs in 2025
# This looks a bit problematic and my corrections to AOD have a minimal impact on this. More likely a data collection issue.
dat %>%
  filter(!is.na(aadd)) %>% 
  mutate(dod = q220y + aady) %>%
  mutate(yod_fl = floor(dod)) %>%
  group_by(yod_fl) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=yod_fl, y=n), stat = "identity")

# Count impossible aad ----------------------------------------------------

# ymd
n_impossible_ymd <- dat %>%
  filter(!is.na(q220m) & q220m != 98 & !is.na(q220d) & q220d != 98) %>%
  mutate(dob = as.Date(paste(q220y, q220m, q220d, sep = "-"), format = "%Y-%m-%d"),
         dod = dob + aadd) %>%
  filter(dod > qintdate) %>% nrow # 53

# ym 
n_impossible_ym <- dat %>%
  filter(!is.na(q220m) & q220m != 98 & (is.na(q220d) | q220d == 98)) %>%
  mutate(earliestdob = ymd(sprintf("%04d-%02d-01", q220y, q220m)),
         dod = earliestdob + aadd) %>%
  filter(dod > qintdate) %>% nrow() # 6

# y
n_impossible_y <- dat %>%
  filter(is.na(q220m) | q220m == 98) %>%
  mutate(earliestdob = ymd(sprintf("%04d-%02d-01", q220y, 1)),
         dod = earliestdob + aadd) %>%
  filter(dod > qintdate) %>% 
  nrow() # 9

# Not counting those with just m or just d as I already dropped those where y was missing
# Also not counting those with yd because that combination shouldn't be impossible. The month could always be before the interview in 2025.

n_impossible_aad_orig <- n_impossible_ymd + n_impossible_ym + n_impossible_y 
n_impossible_aad_orig # 68

# Impossible aad - based on ymd -------------------------------------------

dat %>%
  filter(!is.na(q220m) & q220m != 98 & !is.na(q220d) & q220d != 98) %>%
  mutate(dob = as.Date(paste(q220y, q220m, q220d, sep = "-"), format = "%Y-%m-%d"),
         dod = dob + aadd,
         dif_d = as.numeric((dod - qintdate)),
         dif_m = as.numeric((dod - qintdate)/30.5),
         dif_y = as.numeric((dod - qintdate)/365.25)) %>%
  select(q220y, q220m, q220d, dob, aad_unit, aad_value, aadm, aadd, dod, qintdate, dif_d, dif_m, dif_y) %>% 
  filter(dod > qintdate) %>% 
  nrow() # 53

# Apply rules
# If aad_unit == 2 and dif_m < 1, reduce aad_value by 1
# If aad_unit == 2 and dif_m >=1 and <2, reduce aad_value by 2
# If aad_unit == 3 and dif_y < 1, reduce aad_value by 1
# If aad_unit == 3 and dif_y > 2, reduce aad_unit by 1
# If aad_unit == 1 and dif_d < 7, recode aad_value as aad_value - dif_d
dat <- dat %>%
  mutate(aad_value_orig = aad_value, aad_unit_orig = aad_unit) %>%
  mutate(dob = as.Date(paste(q220y, q220m, q220d, sep = "-"), format = "%Y-%m-%d"),
         dod = dob + aadd,
         dif_d = as.numeric((dod - qintdate)),
         dif_m = as.numeric((dod - qintdate)/30.5),
         dif_y = as.numeric((dod - qintdate)/365.25),
         aad_value = case_when(
           !is.na(q220m) & q220m != 98 & !is.na(q220d) & q220d != 98 & dod > qintdate &
             aad_unit == 2 & dif_m < 1 ~ aad_value - 1,
           !is.na(q220m) & q220m != 98 & !is.na(q220d) & q220d != 98 & dod > qintdate &
             aad_unit == 2 & dif_m >= 1 & dif_m < 2 ~ aad_value - 2,
           !is.na(q220m) & q220m != 98 & !is.na(q220d) & q220d != 98 & dod > qintdate &
             aad_unit == 3 & dif_y < 1 ~ aad_value - 1,
           !is.na(q220m) & q220m != 98 & !is.na(q220d) & q220d != 98 & dod > qintdate &
             aad_unit == 1 & dif_d < 7 ~ aad_value - dif_d,
           TRUE ~ aad_value
         ),
         aad_unit = case_when(
           !is.na(q220m) & q220m != 98 & !is.na(q220d) & q220d != 98 & dod > qintdate &
             aad_unit == 3 & dif_y > 2 ~ 1,
           TRUE ~ aad_unit
         )
  ) 
nrow(subset(dat, aad_value_orig != aad_value)) # 36 changes
nrow(subset(dat, aad_unit_orig != aad_unit)) # 10 changes

# identify deaths with changes
dat$flag[dat$aad_value_orig != dat$aad_value] <- 1

# Recalculate aadm, aadd
dat$aadm <- dat$aad_value
dat$aadm[which(dat$aad_unit == 1)] <- dat$aadm[which(dat$aad_unit == 1)] / 30.5
dat$aadm[which(dat$aad_unit == 3)] <- dat$aadm[which(dat$aad_unit == 3)]*12
dat$aadd <- dat$aad_value
dat$aadd[which(dat$aad_unit == 2)] <- dat$aadd[which(dat$aad_unit == 2)] * 30.5
dat$aadd[which(dat$aad_unit == 3)] <- dat$aadd[which(dat$aad_unit == 3)] * 365.25

# How many deaths were shifted to 2025 with this correction?
n_2025shift1 <- dat %>%
  filter(!is.na(flag) & flag == 1) %>%
  mutate(newdod = dob + aadd,
         newyod = year(newdod),
         oldyod = year(dod)) %>%
  filter(oldyod != 2025 & newyod == 2025) %>%
  nrow()

# dat %>%
#   filter(qwsec2b_id %in% foo) %>%
#   mutate(dod = dob + aadd,
#           flag = ifelse(dod > qintdate, 1 ,0)) %>%
#   select(q220y, q220m, q220d, dob, aad_unit, aad_value, aadm, aadd, dod, qintdate, dif_d, dif_m, dif_y, flag) %>% 
#   View()

# Drop those with AOD that still impossible
id_drop1 <- dat %>%
  filter(!is.na(q220m) & q220m != 98 & !is.na(q220d) & q220d != 98) %>%
  mutate(dob = as.Date(paste(q220y, q220m, q220d, sep = "-"), format = "%Y-%m-%d"),
         dod = dob + aadd) %>%
  filter(dod > qintdate)
nrow(id_drop1) # 7
dat <- subset(dat, !(qwsec2b_id %in% id_drop1$qwsec2b_id))
nrow(dat) # 79766

nrow(subset(dat, !is.na(aad_unit))) # 6914, 7 less than 6921
nrow(subset(dat, !is.na(aad_value))) # 6914, 7 less than 6921

# Impossible aad - based on ym --------------------------------------------

dat %>%
  filter(!is.na(q220m) & q220m != 98 & (is.na(q220d) | q220d == 98)) %>%
  mutate(earliestdob = ymd(sprintf("%04d-%02d-01", q220y, q220m)),
         dod = earliestdob + aadd,
         dif_d = as.numeric((dod - qintdate)),
         dif_m = as.numeric((dod - qintdate)/30.5),
         dif_y = as.numeric((dod - qintdate)/365.25)) %>%
  select(q220y, q220m, q220d, earliestdob, aad_unit, aad_value, aadm, aadd, dod, qintdate, dif_d, dif_m, dif_y) %>% 
  filter(dod > qintdate) %>% 
  nrow() # 6

foo <-  dat %>%
  filter(!is.na(q220m) & q220m != 98 & (is.na(q220d) | q220d == 98)) %>%
  mutate(earliestdob = ymd(sprintf("%04d-%02d-01", q220y, q220m)),
         dod = earliestdob + aadd) %>%
  filter(dod > qintdate) %>% pull(qwsec2b_id)

# Apply rules (same as above)
# If aad_unit == 2 and dif_m < 1, reduce aad_value by 1
# If aad_unit == 2 and dif_m >=1 and <2, reduce aad_value by 2
# If aad_unit == 3 and dif_y < 1, reduce aad_value by 1
# If aad_unit == 3 and dif_y > 2, reduce aad_unit by 1
# If aad_unit == 1 and dif_d < 7, recode aad_value as aad_value - dif_d
dat <- dat %>%
  mutate(aad_value_orig = aad_value, aad_unit_orig = aad_unit) %>%
  mutate(earliestdob = ymd(sprintf("%04d-%02d-01", q220y, q220m)),
         dod = earliestdob + aadd,
         dif_d = as.numeric((dod - qintdate)),
         dif_m = as.numeric((dod - qintdate)/30.5),
         dif_y = as.numeric((dod - qintdate)/365.25),
         aad_value = case_when(
           !is.na(q220m) & q220m != 98 & (is.na(q220d) | q220d == 98) & dod > qintdate &
             aad_unit == 2 & dif_m < 1 ~ aad_value - 1,
           !is.na(q220m) & q220m != 98 & (is.na(q220d) | q220d == 98) & dod > qintdate &
             aad_unit == 2 & dif_m >= 1 & dif_m < 2 ~ aad_value - 2,
           !is.na(q220m) & q220m != 98 & (is.na(q220d) | q220d == 98) & dod > qintdate &
             aad_unit == 3 & dif_y < 1 ~ aad_value - 1,
           !is.na(q220m) & q220m != 98 & (is.na(q220d) | q220d == 98) & dod > qintdate &
             aad_unit == 1 & dif_d < 7 ~ aad_value - dif_d,
           TRUE ~ aad_value
         ),
         aad_unit = case_when(
           !is.na(q220m) & q220m != 98 & (is.na(q220d) | q220d == 98) & dod > qintdate &
             aad_unit == 3 & dif_y > 2 ~ 1,
           TRUE ~ aad_unit
         )
  ) 
# It's ok if there is a warning message here for when earliestdob failed to parse.
# This happens when month was either missing or 98
# And these AAD don't get adjusted

nrow(subset(dat, aad_value_orig != aad_value)) # 4 changes
nrow(subset(dat, aad_unit_orig != aad_unit)) # 1 change

# identify deaths with changes
dat$flag[dat$aad_value_orig != dat$aad_value] <- 1

# Recalculate aadm, aadd
dat$aadm <- dat$aad_value
dat$aadm[which(dat$aad_unit == 1)] <- dat$aadm[which(dat$aad_unit == 1)] / 30.5
dat$aadm[which(dat$aad_unit == 3)] <- dat$aadm[which(dat$aad_unit == 3)]*12
dat$aadd <- dat$aad_value
dat$aadd[which(dat$aad_unit == 2)] <- dat$aadd[which(dat$aad_unit == 2)] * 30.5
dat$aadd[which(dat$aad_unit == 3)] <- dat$aadd[which(dat$aad_unit == 3)] * 365.25

# How many deaths were shifted to 2025 with this correction?
n_2025shift2 <- dat %>%
  filter(!is.na(flag) & flag == 1) %>%
  mutate(newdod = dob + aadd,
         newyod = year(newdod),
         oldyod = year(dod)) %>%
  filter(oldyod != 2025 & newyod == 2025) %>%
  nrow()

# View corrections for those that had impossible aod
# dat %>%
#  filter(qwsec2b_id %in% foo) %>%
#  mutate(earliestdob = ymd(sprintf("%04d-%02d-01", q220y, q220m)),
#         dod = earliestdob + aadd,
#         flag = ifelse(dod > qintdate, 1, 0)) %>%
#  select(q220y, q220m, q220d, dob, aad_unit, aad_value, aadm, aadd, dod, qintdate, dif_d, dif_m, dif_y, flag) %>% 
#  View()

# Drop those with AOD that still impossible
id_drop2 <- dat %>%
  filter(!is.na(q220m) & q220m != 98 & (is.na(q220d) | q220d == 98)) %>%
  mutate(earliestdob = ymd(sprintf("%04d-%02d-01", q220y, q220m)),
         dod = earliestdob + aadd) %>%
  filter(dod > qintdate)
nrow(id_drop2) # 1
dat <- subset(dat, !(qwsec2b_id %in% id_drop2$qwsec2b_id))
nrow(dat) # 79765

nrow(subset(dat, !is.na(aad_unit))) # 6913, 1 less than 6914
nrow(subset(dat, !is.na(aad_value))) # 6913, 1 less than 6914
nrow(subset(dat, !is.na(aadd))) # 6913
nrow(subset(dat, !is.na(aadm))) # 6913
nrow(subset(dat, !is.na(aady))) # 6913

# Impossible aad - based on y ---------------------------------------------

dat %>%
  filter(is.na(q220m) | q220m == 98) %>%
  mutate(earliestdob = ymd(sprintf("%04d-%02d-01", q220y, 1)),
         dod = earliestdob + aadd,
         dif_d = as.numeric((dod - qintdate)),
         dif_m = as.numeric((dod - qintdate)/30.5),
         dif_y = as.numeric((dod - qintdate)/365.25)) %>%
  filter(dod > qintdate) %>% 
  select(q220y, q220m, q220d, earliestdob, aad_unit, aad_value, aadm, aadd, dod, qintdate, dif_d, dif_m, dif_y) %>% 
  nrow() # 9

foo <-  dat %>%
  filter(is.na(q220m) | q220m == 98) %>%
  mutate(earliestdob = ymd(sprintf("%04d-%02d-01", q220y, 1)),
         dod = earliestdob + aadd) %>%
  filter(dod > qintdate) %>% pull(qwsec2b_id)

# Apply rules (same as above)
# If aad_unit == 2 and dif_m < 1, reduce aad_value by 1
# If aad_unit == 2 and dif_m >=1 and <2, reduce aad_value by 2
# If aad_unit == 3 and dif_y < 1, reduce aad_value by 1
# If aad_unit == 3 and dif_y > 2, reduce aad_unit by 1
# If aad_unit == 1 and dif_d < 7, recode aad_value as aad_value - dif_d
dat <- dat %>%
  mutate(aad_value_orig = aad_value, aad_unit_orig = aad_unit) %>%
  mutate(earliestdob = ymd(sprintf("%04d-%02d-01", q220y, 1)),
         dod = earliestdob + aadd,
         dif_d = as.numeric((dod - qintdate)),
         dif_m = as.numeric((dod - qintdate)/30.5),
         dif_y = as.numeric((dod - qintdate)/365.25),
         aad_value = case_when(
           is.na(q220m) | q220m == 98 & dod > qintdate &
             aad_unit == 2 & dif_m < 1 ~ aad_value - 1,
           is.na(q220m) | q220m == 98 & dod > qintdate &
             aad_unit == 2 & (dif_m >= 1 & dif_m < 2) ~ aad_value - 2,
           is.na(q220m) | q220m == 98 & dod > qintdate &
             aad_unit == 3 & dif_y < 1 ~ aad_value - 1,
           is.na(q220m) | q220m == 98 & dod > qintdate &
             aad_unit == 1 & dif_d < 7 ~ aad_value - dif_d,
           TRUE ~ aad_value
         ),
         aad_unit = case_when(
           (is.na(q220m) | q220m == 98) & (is.na(q220d) | q220d == 98) & dod > qintdate &
             aad_unit == 3 & dif_y > 2 ~ 1,
           TRUE ~ aad_unit
         )
  ) 
nrow(subset(dat, aad_value_orig != aad_value)) # 2 changes
nrow(subset(dat, aad_unit_orig != aad_unit)) # 5 changes

# identify deaths with changes
dat$flag[dat$aad_value_orig != dat$aad_value] <- 1

# Recalculate aadm, aadd
dat$aadm <- dat$aad_value
dat$aadm[which(dat$aad_unit == 1)] <- dat$aadm[which(dat$aad_unit == 1)] / 30.5
dat$aadm[which(dat$aad_unit == 3)] <- dat$aadm[which(dat$aad_unit == 3)]*12
dat$aadd <- dat$aad_value
dat$aadd[which(dat$aad_unit == 2)] <- dat$aadd[which(dat$aad_unit == 2)] * 30.5
dat$aadd[which(dat$aad_unit == 3)] <- dat$aadd[which(dat$aad_unit == 3)] * 365.25

# How many deaths were shifted to 2025 with this correction?
n_2025shift3 <- dat %>%
  filter(!is.na(flag) & flag == 1) %>%
  mutate(newdod = dob + aadd,
         newyod = year(newdod),
         oldyod = year(dod)) %>%
  filter(oldyod != 2025 & newyod == 2025) %>%
  nrow()

# View corrections for those that had impossible aod
# dat %>%
#  filter(qwsec2b_id %in% foo) %>%
#  mutate(earliestdob = ymd(sprintf("%04d-%02d-01", q220y, 1)),
#         dod = earliestdob + aadd,
#         flag = ifelse(dod > qintdate, 1, 0)) %>%
#  select(q220y, q220m, q220d, dob, aad_unit, aad_value, aadm, aadd, dod, qintdate, dif_d, dif_m, dif_y, flag) %>%
#  View()

# Drop those with AOD that still impossible
id_drop3 <- dat %>%
  filter(is.na(q220m) | q220m == 98) %>%
  mutate(earliestdob = ymd(sprintf("%04d-%02d-01", q220y, 1)),
         dod = earliestdob + aadd) %>%
  filter(dod > qintdate)
nrow(id_drop3) # 2
dat <- subset(dat, !(qwsec2b_id %in% id_drop3$qwsec2b_id))
nrow(dat) # 79763

nrow(subset(dat, !is.na(aad_unit))) # 6911, 2 less than 6913
nrow(subset(dat, !is.na(aad_value))) # 6911, 2 less than 6913
nrow(subset(dat, !is.na(aadd))) # 6911
nrow(subset(dat, !is.na(aadm))) # 6911
nrow(subset(dat, !is.na(aady))) # 6911
table(dat$event, useNA = "always") # 6911
table(subset(dat, is.na(event))$q223_aug) # all those missing event were not live births

# Remove columns ----------------------------------------------------------

dat <- dat %>%
  select(-c(aad_value_orig, aad_unit_orig,
            dob, dod, dif_d, dif_m, dif_y, earliestdob))

# Save audit --------------------------------------------------------------

# There are a lot of deaths reported for 2025
# Check how many deaths i shifted to 2025 in cleaning AOD
n_2025shift1 + n_2025shift2 + n_2025shift3 # 9

n_impossible_aad_corrections <- nrow(id_drop1) + nrow(id_drop2) + nrow(id_drop3)
n_impossible_aad_corrections  # 10
dat_aud_aad <- data.frame(variable = c("impossible aad original","impossible aad after corrections"),
                      n = c(n_impossible_aad_orig, n_impossible_aad_corrections))
write.csv(dat_aud_aad, paste0("./gen/fph/audit/dat_aud-aad_", format(Sys.Date(), "%Y%m%d"), ".csv"), row.names = FALSE)

# Save --------------------------------------------------------------------

saveRDS(dat, "./gen/fph/temp/fph-clean-aod.rds")
