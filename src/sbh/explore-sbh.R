################################################################################
#' @description Explore SBH
#' @return None
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
dat <- read.csv("./data/reach_mortalite_femme_qwsec2a.csv")
################################################################################

# Fils ou filles décédés
table(dat$q206, useNA = "always")

# Fils décédés
table(dat$q207a, useNA = "always")

# Filles décédés
table(dat$q207b, useNA = "always")

# Nombre total d'enfants nés
table(dat$q208, useNA = "always")

# Sonder total enfants nés
table(dat$q209, useNA = "always") # 1 ;Oui, c'est correct

# Eu une grossesse  qui s'était terminée par une naissance non vivante
table(dat$q210, useNA = "always")

# I'm confused about 211 but i think it's ok.
# in the pdf questionnaire it is Combien de fausses couches, d'avortements et de morts-nés avez-vous eu ?
# in the pregnancy_history.dcf it is Nombre de naissances non vivantes
# the data dictionary seems like it would just be stillbirths
table(dat$q211, useNA = "always")

# Nombre total de grossesses
table(dat$q212, useNA = "always")

# missing q208 (nombre total d'enfants nés), do they have any kids reported as living with them or elsewhere?
# In a few cases, yes.
nrow(subset(dat, is.na(q208))) # 49
table(subset(dat, is.na(q208))$q201, useNA = "always")  # Eu une naissance
table(subset(dat, is.na(q208))$q202, useNA = "always")  # Fils et filles à la maison
table(subset(dat, is.na(q208))$q203a, useNA = "always")  # Fils a la maison
table(subset(dat, is.na(q208))$q203b, useNA = "always")  # Filles a la maison
table(subset(dat, is.na(q208))$q204, useNA = "always")   # Fils et filles vivant ailleurs
# do they have any kids who died?
# No, all NA.
table(subset(dat, is.na(q208))$q206, useNA = "always")   # Fils ou filles décédés
table(subset(dat, is.na(q208))$q207a, useNA = "always")   # Fils décédés
table(subset(dat, is.na(q208))$q207b, useNA = "always")   # Filles décédés
# do they have any pregnancy losses or pregnancies?
# No, all NA.
table(subset(dat, is.na(q208))$q211, useNA = "always") 
table(subset(dat, is.na(q208))$q212, useNA = "always") 

# Eu une grossesse  qui s'était terminée par une naissance non vivante
table(dat$q210, useNA = "always") # 3229 yes
# q211 (nombre de naissances non vivantes)
table(dat$q211, useNA = "always")
nrow(subset(dat, !is.na(q211) & q211 != 0)) # 3236
# slightly more pregnancy losses reported than those who said yes to q210


# Add variable for number of children surviving (living with them or elsewhere)
# Add variable for number of children died
# will be 0 if NAs in q207a and q207b
dat <- dat %>%
  rowwise() %>%
  mutate(q203_comb = sum(q203a, q203b, na.rm = TRUE)) %>% # sons living at home, daughters living at home
  mutate(q205_comb = sum(q205a, q205b, na.rm = TRUE)) %>% # sons living elsewhere, daughters living elsewhere
  mutate(q207_comb = sum(q207a, q207b, na.rm = TRUE)) %>% # sons died, daughters died
  mutate(q207_comb = ifelse(!is.na(q206) & q206 == 1 & q207_comb == 0, NA, q207_comb))
# Recode children died to NA if it is reported that a child has died (q206) but q207_comb is zero

# Add up birth and pregnancy totals
dat <- dat %>%
  rowwise() %>%
  mutate(sum_cebcd = sum(q203_comb, q205_comb, q207_comb, na.rm = TRUE),
         sum_preg = sum(q203_comb, q205_comb, q207_comb, q211, na.rm = TRUE)) # q211 is nombre de naissance non-vivante

# total children
unique(dat$q208)
# total pregnancies
unique(dat$q212)
# total pregnancies missing
nrow(subset(dat, is.na(q212))) # 53
# missing value for has had a birth
nrow(subset(dat, is.na(q201))) # 44
# total pregnancies missing when had a birth is yes
nrow(subset(dat, !is.na(q201) & q201 == 1 & is.na(q212))) # 9

# Total children surviving and died equals total children
nrow(subset(dat, sum_cebcd == q208)) # 26072
# Children surviving is more than total children
nrow(subset(dat, q203_comb + q205_comb > q208)) # 9
# Children died is more than total children
nrow(subset(dat, q207_comb > q208)) # 1
# Sum of children surviving and died is more than total
nrow(subset(dat, sum_cebcd > q208)) # 23
# Sum of children surviving and died is less than total
nrow(subset(dat, sum_cebcd < q208)) # 2
# Total children surviving and died plus pregnancy losses equals total pregnancies
nrow(subset(dat, sum_preg == q212)) # 25971
# Total children surviving and died plus pregnancy losses is more than total pregnancies
nrow(subset(dat, sum_preg > q212)) # 40
# Total children surviving and died plus pregnancy losses is less than total pregnancies
nrow(subset(dat, sum_preg < q212)) # 82

# adjust totals to match reported number of births and pregnancies
# adjustments will be made in cleaning file to these ones
nrow(subset(dat, sum_cebcd != dat$q208)) # 25 births
nrow(subset(dat, !is.na(dat$q212) & sum_preg != dat$q212)) # 122 pregnancies
nrow(subset(dat, is.na(dat$q212))) # 53 total preg variable missing


