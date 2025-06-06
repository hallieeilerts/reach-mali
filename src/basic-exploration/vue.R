################################################################################
#' @description Basic exploration of all the files that are part of first section, vue suffix
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
################################################################################

# One row with number of children in Sikasso
dat <- read.csv("./data/reach_accueil_vue.csv")
head(dat)

# Appears to be information about interview plus CARACTÉRISTIQUES SOCIODÉMOGRAPHIQUES DE L'ENQUÊTÉE  from mortalite-femme
dat <- read.csv("./data/reach_qwsec01_vue.csv")
head(dat)

# Appears to be information about interview plus SBH from mortalite-femme
dat <- read.csv("./data/reach_qwsec2a_vue.csv")
head(dat)

# Appears to be information about interview plus FPH from mortalite-femme
dat <- read.csv("./data/reach_qwsec2b_vue.csv")
head(dat)

# Appears to be information about interview plus child health questions from mortalite-femme
dat <- read.csv("./data/reach_qwsec6a_vue.csv")
head(dat)

# Empty
dat <- read.csv("./data/reach_naiss_enfant_vue.csv")

# Information on child, mom, date of AZ administration?
dat <- read.csv("./data/reach_traitement_surv_vue.csv")
