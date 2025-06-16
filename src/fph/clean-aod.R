################################################################################
#' @description Adjust values for impossible ages of death according to rules,
#' when not possible to discern how to correct aod, drop
#' @return 
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

nrow(dat) # 79843
nrow(dat) == length(unique(dat$qwsec2b_id)) # unique identifier
length(unique(dat$level_1_id)) # should be mother identifier

# Indicator for event (death)
dat$event <- ifelse(dat$q224 == 2, 1, 0)
# check that when event is missing, it was not a live birth (q223_aug != 1)
table(subset(dat, is.na(event))$q223_aug, useNA =  "always")

# Year of birth is missing
nrow(subset(dat, is.na(q220y))) # 50
# Recover when possible from current age (q225)
dat <- dat %>%
  mutate(q220y = case_when(
    !is.na(q225) & is.na(q220y) ~ as.numeric(format(qintdate, "%Y")) - q225,
    TRUE ~ q220y
  ))
nrow(subset(dat, is.na(q220y))) # 48

# Drop observations that are still missing year of birth
dat <- subset(dat, !is.na(q220y))

# q228 Age au décès
# Replace blank pace with 0
dat$q228_clean <- gsub(" ", "0", dat$q228)
dat$q228_clean <- as.numeric(as.character(dat$q228_clean))
unique(dat$q228)
unique(dat$q228_clean)
nrow(subset(dat, !is.na(q228))) # 6916
nrow(subset(dat, !is.na(q228_clean))) # 6916

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

# Format interview date
dat$qintdate_y <- lubridate::year(dat$qintdate)
dat$qintdate_m <- lubridate::month(dat$qintdate)
dat$qintdate_d <- lubridate::day(dat$qintdate)

nrow(subset(dat, !is.na(aad_unit))) # 6916
nrow(subset(dat, !is.na(aad_value))) # 6916

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

# Recalculate aadm, aadd
dat$aadm <- dat$aad_value
dat$aadm[which(dat$aad_unit == 1)] <- dat$aadm[which(dat$aad_unit == 1)] / 30.5
dat$aadm[which(dat$aad_unit == 3)] <- dat$aadm[which(dat$aad_unit == 3)]*12
dat$aadd <- dat$aad_value
dat$aadd[which(dat$aad_unit == 2)] <- dat$aadd[which(dat$aad_unit == 2)] * 30.5
dat$aadd[which(dat$aad_unit == 3)] <- dat$aadd[which(dat$aad_unit == 3)] * 365.25

# dat %>%
#   filter(qwsec2b_id %in% foo) %>%
#   mutate(dod = dob + aadd,
#           flag = ifelse(dod > qintdate, 1 ,0)) %>%
#   select(q220y, q220m, q220d, dob, aad_unit, aad_value, aadm, aadd, dod, qintdate, dif_d, dif_m, dif_y, flag) %>% 
#   View()

# Drop
id_drop1 <- dat %>%
  filter(!is.na(q220m) & q220m != 98 & !is.na(q220d) & q220d != 98) %>%
  mutate(dob = as.Date(paste(q220y, q220m, q220d, sep = "-"), format = "%Y-%m-%d"),
         dod = dob + aadd) %>%
  filter(dod > qintdate)
nrow(id_drop1) # 7
dat <- subset(dat, !(qwsec2b_id %in% id_drop1$qwsec2b_id))
nrow(dat) # 79788

nrow(subset(dat, !is.na(aad_unit))) # 6909, 7 less than 6916
nrow(subset(dat, !is.na(aad_value))) # 6909, 7 less than 6916

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
nrow(subset(dat, aad_value_orig != aad_value)) # 4 changes
nrow(subset(dat, aad_unit_orig != aad_unit)) # 1 change

# It's ok if there is a warning message here for when earliestdob failed to parse.
# This happens when month was either missing or 98
# And these AAD don't get adjusted

# Recalculate aadm, aadd
dat$aadm <- dat$aad_value
dat$aadm[which(dat$aad_unit == 1)] <- dat$aadm[which(dat$aad_unit == 1)] / 30.5
dat$aadm[which(dat$aad_unit == 3)] <- dat$aadm[which(dat$aad_unit == 3)]*12
dat$aadd <- dat$aad_value
dat$aadd[which(dat$aad_unit == 2)] <- dat$aadd[which(dat$aad_unit == 2)] * 30.5
dat$aadd[which(dat$aad_unit == 3)] <- dat$aadd[which(dat$aad_unit == 3)] * 365.25


# dat %>%
#  filter(qwsec2b_id %in% foo) %>%
#  mutate(earliestdob = ymd(sprintf("%04d-%02d-01", q220y, q220m)),
#         dod = earliestdob + aadd,
#         flag = ifelse(dod > qintdate, 1, 0)) %>%
#  select(q220y, q220m, q220d, dob, aad_unit, aad_value, aadm, aadd, dod, qintdate, dif_d, dif_m, dif_y, flag) %>% 
#  View()


# Drop
id_drop2 <- dat %>%
  filter(!is.na(q220m) & q220m != 98 & (is.na(q220d) | q220d == 98)) %>%
  mutate(earliestdob = ymd(sprintf("%04d-%02d-01", q220y, q220m)),
         dod = earliestdob + aadd) %>%
  filter(dod > qintdate)
nrow(id_drop2) # 1
dat <- subset(dat, !(qwsec2b_id %in% id_drop2$qwsec2b_id))
nrow(dat) # 79787

nrow(subset(dat, !is.na(aad_unit))) # 6908, 1 less than 6909
nrow(subset(dat, !is.na(aad_value))) # 6908, 1 less than 6909

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

# Recalculate aadm, aadd
dat$aadm <- dat$aad_value
dat$aadm[which(dat$aad_unit == 1)] <- dat$aadm[which(dat$aad_unit == 1)] / 30.5
dat$aadm[which(dat$aad_unit == 3)] <- dat$aadm[which(dat$aad_unit == 3)]*12
dat$aadd <- dat$aad_value
dat$aadd[which(dat$aad_unit == 2)] <- dat$aadd[which(dat$aad_unit == 2)] * 30.5
dat$aadd[which(dat$aad_unit == 3)] <- dat$aadd[which(dat$aad_unit == 3)] * 365.25

# drop cols
dat <- dat %>%
  select(-c(aad_value_orig, aad_unit_orig))

# dat %>%
#  filter(qwsec2b_id %in% foo) %>%
#  mutate(earliestdob = ymd(sprintf("%04d-%02d-01", q220y, 1)),
#         dod = earliestdob + aadd,
#         flag = ifelse(dod > qintdate, 1, 0)) %>%
#  select(q220y, q220m, q220d, dob, aad_unit, aad_value, aadm, aadd, dod, qintdate, dif_d, dif_m, dif_y, flag) %>%
#  View()

# Drop
id_drop3 <- dat %>%
  filter(is.na(q220m) | q220m == 98) %>%
  mutate(earliestdob = ymd(sprintf("%04d-%02d-01", q220y, 1)),
         dod = earliestdob + aadd) %>%
  filter(dod > qintdate)
nrow(id_drop3) # 2
dat <- subset(dat, !(qwsec2b_id %in% id_drop3$qwsec2b_id))
nrow(dat) # 79785

nrow(subset(dat, !is.na(aad_unit))) # 6906, 2 less than 6908
nrow(subset(dat, !is.na(aad_value))) # 6906, 2 less than 6908

# Save audit --------------------------------------------------------------

n_impossible_aad_corrections <- nrow(id_drop1) + nrow(id_drop2) + nrow(id_drop3) # 10
dat_aud <- data.frame(variable = c("impossible aad original","impossible aad after corrections"),
                      n = c(n_impossible_aad_orig, n_impossible_aad_corrections))
write.csv(dat_aud, paste0("./gen/fph/audit/dat_aud-aad_", format(Sys.Date(), "%Y%m%d"), ".csv"), row.names = FALSE)

# Save --------------------------------------------------------------------

saveRDS(dat, "./gen/fph/temp/fph-clean-aod.rds")
