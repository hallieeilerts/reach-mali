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
table(dat$q210, useNA = "always") # 3225 yes
# q211 (nombre de naissances non vivantes)
table(dat$q211, useNA = "always")
nrow(subset(dat, !is.na(q211) & q211 != 0)) # 3232
# slightly more pregnancy losses reported than those who said yes to q210



