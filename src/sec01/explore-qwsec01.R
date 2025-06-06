################################################################################
#' @description Exploration of qwsec01
#' @return None
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(forcats)
library(ggplot2)
#' Inputs
dat <- read.csv("./data/reach_qwsec01_vue.csv")
################################################################################

# one row per individual

nrow(dat) # 26357
length(unique(dat$id_data_cover)) # 26357
length(unique(dat$guid)) # 26357

#id_data_cover
#guid  
#grappe  Numéro GRAPPE
#w_enum  CODE ENUMERATEUR
#w_cons  NUMÉRO DE CONCESSION
#w_men   NUMERO DU MENAGE
#w_ind   NUMERO INDIVIDU
#w_nom   Nom de la mère/gardienne
#wi12  Code de l'enquêteur
#wi13  Code de superviseur
#qregion  Region|Région
#qlregion  Nom de la région
#qcercle  Cercle
#qlcercle  Nom du cercle
#qdistrict  District sanitaire
#wi3  commune
#wli3 nom de la commune
#wi4 nom du village
#wi5 Nom de l'aire de santé
#qtype  Type de résidence (num)
#qqtype  Type de questionnaire
#qvisites  Contrôle visites
#qcontvisit  Group to control visits
#qvline  Numéro de ligne Visite
# qvday jour de visite
# qvmonth mois de visite
# qvyear Année de visite
#qvhour
#qvminute
# qvresult Résultat de la visite
# qintd Jour de l'interview
# qintm Mois de l'interview
# qinty Année de l'interview
# qintnum Numero interviewer ménage
# qresult Résultat de l'interview individuel
# qvisits nombre total de visites
# qteam numero equipe
#qsuperv  Numero superviseur
# qlangq Langue du questionnaire
# qlangi langue de l'interview
# qlangr langue de l’enquêté(e)
# qcorrect Cette personne est-elle la bonne? (is this the right person?)
#qconsent  Consentement éclairé
#wco3  Résultat final de l’enquête
#wco3_bis  Résultat de l’enquête du Questionnaire femme
#q214  Introduction de l'historique de grossesse
#q602  Introduction de l'historique de grossesse
#q502  Introduction to section 5
#qesec5  Entries in section 5 immunizations
#qesec6  Rubriques de la section 6 santé et nutrition
#wfin    Fin du questionnaire
#id_data  
#level_1_id  
#q101  Début de l'interview
#q101h  Début de l'interview (heure)
#q101m  Début de l'interview (minutes)
#q104  Combien de temps a habité au lieu de l’interview
#q106  Date de déménagement
#q106m  Mois de déménagement
#q106y  Année de déménagement
#q107  Région habité avant d'être au lieu d’interview
#q108  A habité dans une ville/village ou une zone rurale
#q109  Raison du déménagement
#q110  Date de naissance
#q110m  Mois de naissance
#q110y  Année de naissance
#q111  Age actuel de l'enquétée
#q112  Auto-déclaration de l’état de santé
#q113  A déjà fréquenté l'école
#q114  Plus haut niveau d'étude
#q115  Plus haute Classe/Année à ce niveau
#q122  Posséde un téléphone mobile
#q131  Ethnie
#q132  Actuellement en union
#heure_fin  Fin de l'interview
#q101h_f  Fin de l'interview (heure)
#q101m_f  Fin de l'interview (minutes)

# Missingness of variables
dat %>%
  summarise(across(everything(), ~ sum(is.na(.)) / n())) %>%
  pivot_longer(cols = everything()) %>%
  ggplot(aes(x = fct_reorder(name, value), y = value)) +
  geom_col() +
  coord_flip() + theme(text = element_text(size = 10))

# Variables that are always missing
dat %>%
  summarise(across(everything(), ~ sum(is.na(.)) / n())) %>%
  pivot_longer(cols = everything()) %>%
  filter(value == 1)
# wi13 Code de superviseur
# qresult Résultat de l'interview individuel
# q214 Introduction de l'historique de grossesse
# q602 Introduction de l'historique de grossesse
# q502 Introduction to section 5
# qesec5 Entries in section 5 immunizations

# variable that are never missing
dat %>%
  summarise(across(everything(), ~ sum(is.na(.)) / n())) %>%
  pivot_longer(cols = everything()) %>%
  filter(value == 0)
# id_data_cover
# guid
# grappe
# w_enum
# w_cons
# w_men
# w_ind
# qregion
# qlregion
# qcercle
# qlcercle
# qdistrict
# wi3       commune
# wli3      nom de la commune
# wi4       nom du village
# wi5       Nom de l'aire de santé
# qtype     Type de résidence (num)
# qintnum   Numero interviewer ménage
# qteam     Numero equipe


# This file only has one interview date while qsecover has all 3
# and sometimes one of those multiple dates can be used to replace a missing value in intdate
# so should rely on qsecover file for intdate

# explore dob
unique(dat$q110)[1:10] # written mmyyyy
table(dat$q110, useNA = "always") # 335 NA
table(dat$q110m, useNA = "always") # 8228 are 98, 335 NA
table(dat$q110y, useNA = "always") # 893 are 9998, 336 NA

# q111  Age actuel de l'enquétée
table(dat$q111, useNA = "always")
# 346 missing, no 98 or 9998 values

# q112  Auto-déclaration de l’état de santé
table(dat$q112, useNA = "always")

#q113  A déjà fréquenté l'école
table(dat$q113, useNA = "always")

#q114  Plus haut niveau d'étude
table(dat$q114, useNA = "always")

#q115  Plus haute Classe/Année à ce niveau
table(dat$q115, useNA = "always")

#q122  Posséde un téléphone mobile
table(dat$q122, useNA = "always")

#q131  Ethnie
table(dat$q131, useNA = "always")

#q132  Actuellement en union
table(dat$q132, useNA = "always")


# number of cercle, district, commune, village, air_sanitaire by region
# dont have variable for grappe and menage
dat %>%
  group_by(qlregion) %>%
  summarise(cercle = n_distinct(qcercle),
            district = n_distinct(qdistrict),
            commune = n_distinct(wi3),
            village = n_distinct(wi4),
            aire_sanitaire = n_distinct(wi5),
            grappe = n_distinct(grappe),
            menage = n_distinct(w_men))

# check that i have right number of communes (grouping by more variables doesn't change it)
dat %>%
  group_by(qlregion, qcercle, qdistrict) %>%
  summarise(n = n_distinct(wi3)) %>%
  group_by(qlregion) %>%
  summarise(n = sum(n))
# check that i have right number of villages (grouping by more variables doesn't change it)
dat %>%
  group_by(qlregion, qcercle, qdistrict, wi3) %>%
  summarise(n = n_distinct(wi4)) %>%
  group_by(qlregion) %>%
  summarise(n = sum(n))
# check that i have right number of aire sanitaire (grouping by more variables doesn't change it)
# THE NUMBER DOES CHANGE WHEN GROUPING
# very slightly for sikasso, segou, mopti
dat %>%
  group_by(qlregion, qcercle, qdistrict, wi3, wi4) %>%
  summarise(n = n_distinct(wi5)) %>%
  group_by(qlregion) %>%
  summarise(n = sum(n))
# check that i have right number of grappe (grouping by more variables doesn't change it)
dat %>%
  group_by(qlregion, qcercle, qdistrict, wi3, wi4, wi5) %>%
  summarise(n = n_distinct(grappe)) %>%
  group_by(qlregion) %>%
  summarise(n = sum(n))

# check that i have right number of menage (grouping by more variables doesn't change it)
# THE NUMBER DOES CHANGE WHEN GROUPING
dat %>%
  group_by(qlregion, qcercle, qdistrict, wi3, wi4, wi5, grappe) %>%
  summarise(n = n_distinct(w_men)) %>%
  group_by(qlregion) %>%
  summarise(n = sum(n))
# try grouping menage by more
# even more menage when adding additional variables
dat %>%
  group_by(qlregion, qcercle, qdistrict, wi3, wi4, wi5, grappe, w_enum, w_cons) %>%
  summarise(n = n_distinct(w_men)) %>%
  group_by(qlregion) %>%
  summarise(n = sum(n))
# same result without qcercle, qdistrict, wi3, wi4, wi5
dat %>%
  group_by(qlregion, grappe, w_enum, w_cons) %>%
  summarise(n = n_distinct(w_men)) %>%
  group_by(qlregion) %>%
  summarise(n = sum(n))
# relationship between grappe, w_enum, w_cons, w_men
#View(dat[,c("qregion","qcercle", "qdistrict","grappe", "w_enum", "w_cons", "w_men", "w_ind")])
# I think grappe contains these others. grappe > w_enum > w_cons > w_men
# And w_ind is NUMERO INDIVIDU
