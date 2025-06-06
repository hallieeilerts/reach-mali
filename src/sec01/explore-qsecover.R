################################################################################
#' @description Exploration of qsecover file
#' @return None
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)
#' Inputs
dat <- read.csv("./data/reach_mortalite_femme_qsecover.csv")
################################################################################

nrow(dat) # 26357
length(unique(dat$qsecover_id)) # 26357
length(unique(dat$level_1_id)) # 26357
sum(dat$qsecover_id != dat$level_1_id) # 1000
# subset(dat, qsecover_id != level_1_id)
# not sure why these diverge at some point. still have same max
max(dat$qsecover_id) #  27151
max(dat$level_1_id) #  27151

# same unique length
length(unique(qsecover$qsecover_id)) # 26357
length(unique(qsecover$level_1_id))  # 26357
# same values
qsecover$qsecover_id[!(qsecover$qsecover_id %in% qsecover$level_1_id)]
qsecover$level_1_id[!(qsecover$level_1_id %in% qsecover$qsecover_id)]
# but they do not always equal each other
# qsecover_id stays sequential, and level_1_id jumps to 1500
qsecover$qsecover_id[which(qsecover$qsecover_id != qsecover$level_1_id)]
qsecover$level_1_id[which(qsecover$level_1_id != qsecover$qsecover_id)]
#View(qsecover[990:1050, c("qsecover_id", "level_1_id")])


# w_men missing? numero de menage
# w_grappe missing? numéro grappe

# w_nom nom de la mère/gardienne
# wi12 Code de l'enquêteur
# wi13 Code de superviseur
# wi3 commune
# wli3 nom de la commune
# wi4 nom du village
# wi5 Nom de l'aire de santé
# qtype Type de résidence (num)
# qqtype Type de questionnaire
# qvisites Contrôle visites
# qcontvisit Group to control visits
# qvline Numéro de ligne Visite
# qvday jour de visite
# qvmonth mois de visite
# qvyear Année de visite
# qvresult Résultat de la visite
# qvday2 day of second visit
# qvday3 day of third visit
# qintd Jour de l'interview
# qintm Mois de l'interview
# qinty Année de l'interview
# qintnum Numero interviewer ménage
# qresult Résultat de l'interview individuel
# qvisits nombre total de visites
# qvisites Contrôle visites
# qteam numero equipe
# qlangq Langue du questionnaire
# qlangi langue de l'interview
# qlangr langue de l’enquêté(e)
# qcorrect Cette personne est-elle la bonne? (is this the right person?)
# wco3 Résultat final de l’enquête

# Data types
t(as.data.frame(t(sapply(dat, class))))

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
# qsecover_id
# level_1_id
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

# Visit dates have more missing that interview date
table(dat$qvyear, useNA = "always")
table(dat$qinty, useNA = "always")

# Is interview date ever missing when the interview was completed?
nrow(subset(dat, is.na(qinty) & wco3 == 1)) # yes, 32
nrow(subset(dat, is.na(qinty) & wco3 == 1 & !is.na(qvyear)))  # 0
nrow(subset(dat, is.na(qinty) & wco3 == 1 & !is.na(qvyear2))) # 0
nrow(subset(dat, is.na(qinty) & wco3 == 1 & !is.na(qvyear3))) # 32
# In these 32 cases, qvyear3 should be used to fill in interview date

# qvresult Resultat de visite|Résultat de la visite
unique(dat$qvresult)
dat %>%
  mutate(qvresult = case_when(
    qvresult == 1 ~ "Commencer l'enquête ménage",
    qvresult == 2 ~ "Pas de membre du ménage à la maison ou pas d'enquêté compétent au moment de la visite",
    qvresult == 3 ~ "Ménage totalement absent pour une longue période",
    qvresult == 4 ~ "Différé",
    qvresult == 5 ~ "Refusé",
    qvresult == 6 ~ "Logement vide ou pas de logement à l'adresse",
    qvresult == 7 ~ "Logement détruit",
    qvresult == 8 ~ "Logement non trouvé",
    qvresult == 9 ~ "Autre",
    TRUE ~ NA_character_
  )) %>%
  group_by(qvresult) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=qvresult, y = n), stat = "identity") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  coord_flip()

## NOTE:
## Issue with the "other" category for wco3
# Value=1;Complete
# Value=2;Pas de membre du ménage à la maison ou pas de repondant du ménage competent à maison au moment de la visite
# Value=3;Menage entier absent pour une longue periode de te
# Value=4;Refus
# Value=5;Logement vide ou adresse n’est pas celle d’un loge
# Value=6;Logement detruit
# Value=7;Logement pas trouve
# Value=6;Autre
# Value='  ';Partiel
# Name=NOTAPPL,Special

# wco3 Résultat final de l’enquête
unique(dat$wco3)
dat %>%
  mutate(wco3 = case_when(
    wco3 == 1 ~ "Complet",
    wco3 == 2 ~ "Pas de membre du ménage à la maison ou pas de répondant du ménage compétent à la maison au moment de la visite",
    wco3 == 3 ~ "Ménage entier absent pour une longue période",
    wco3 == 4 ~ "Refus",
    wco3 == 5 ~ "Logement vide ou adresse n’est pas celle d’un logement",
    wco3 == 6 ~ "Logement détruit ou Autre",
    wco3 == 7 ~ "Logement pas trouvé",
    trimws(wco3) == "" ~ "Partiel",
    TRUE ~ NA_character_
  )) %>%
  group_by(wco3) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=wco3, y = n), stat = "identity") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  coord_flip()

# Missing visit year
table(dat$qvyear, useNA = "always")
# Missing interview year
table(dat$qinty, useNA = "always")

# When interview year is missing, what was the Résultat final de l’enquête
dat %>%
  mutate(wco3 = case_when(
    wco3 == 1 ~ "Complet",
    wco3 == 2 ~ "Pas de membre du ménage à la maison ou pas de répondant du ménage compétent à la maison au moment de la visite",
    wco3 == 3 ~ "Ménage entier absent pour une longue période",
    wco3 == 4 ~ "Refus",
    wco3 == 5 ~ "Logement vide ou adresse n’est pas celle d’un logement",
    wco3 == 6 ~ "Logement détruit ou Autre",
    wco3 == 7 ~ "Logement pas trouvé",
    trimws(wco3) == "" ~ "Partiel",
    TRUE ~ NA_character_
  )) %>%
  filter(is.na(qinty)) %>%
  group_by(wco3) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=wco3, y = n), stat = "identity") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  coord_flip()

# Is this the right person
table(dat$qcorrect, useNA = "always")
# When is this the right person is missing, what was the Résultat final de l’enquête (wco3)
dat %>%
  filter(is.na(qcorrect)) %>%
  group_by(wco3) %>%
  summarise(n = n())
# When is this the right person is missing, what was the Résultat de la visite
dat %>%
  filter(is.na(qcorrect)) %>%
  group_by(qvresult) %>%
  summarise(n = n())
# Any time it was not the right person, the interview/visit result is missing
# View(subset(dat, is.na(qcorrect)))

# Id variables are unique
nrow(dat) == length(unique(dat$qsecover_id)) 
nrow(dat) == length(unique(dat$level_1_id))

# Multiple visits
dat %>%
  mutate(q1 = as.Date(paste(qvyear, qvmonth, qvday, sep = "-"), format = "%Y-%m-%d"),
         q2 = as.Date(paste(qvyear2, qvmonth2, qvday2, sep = "-"), format = "%Y-%m-%d"),
         q3 = as.Date(paste(qvyear3, qvmonth3, qvday3, sep = "-"), format = "%Y-%m-%d")) %>%
  select(level_1_id, q1, q2, q3) %>% 
  pivot_longer(
    cols = -level_1_id
  ) %>%
  group_by(name, value) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=value, y = n), stat = "identity") +
  facet_wrap(~name)

# Does qvisits add up to recorded visits
table(dat$qvisits, useNA = "always")
dat %>%
  mutate(had_vis1 = !is.na(qvyear),
         had_vis2 = !is.na(qvyear2),
         had_vis3 = !is.na(qvyear3),
         nvis = had_vis1 + had_vis2 + had_vis3) %>% 
  filter(nvis != qvisits) %>%
  filter(qvisits == 3)
# No, if you add up the recorded dates, it does not sum to qvisits  
# qvisits was generated from the latest recorded qvyear
# For example, if there was a qvyear3, qvisits == 3 even if qvyear and qvyear2 are NA

# Interview completion by...
dat_plot <- dat %>%
  mutate(wco3 = case_when(
    wco3 == 1 ~ "Complet",
    wco3 == 2 ~ "Pas de membre du ménage à la maison ou pas de répondant du ménage compétent à la maison au moment de la visite",
    wco3 == 3 ~ "Ménage entier absent pour une longue période",
    wco3 == 4 ~ "Refus",
    wco3 == 5 ~ "Logement vide ou adresse n’est pas celle d’un logement",
    wco3 == 6 ~ "Logement détruit ou Autre",
    wco3 == 7 ~ "Logement pas trouvé",
    trimws(wco3) == "" ~ "Partiel",
    TRUE ~ NA_character_
  ))

# Region
dat_plot %>%
  group_by(qlregion, wco3) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=qlregion, y = n, fill = qlregion), stat = "identity") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(title = "Résultat final de l’enquête") +
  facet_wrap(~wco3, scales = "free_y",  labeller = label_wrap_gen()) +
  guides(fill = guide_legend(title = NULL))
dat_plot %>%
  group_by(qlregion, wco3) %>%
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) %>%
  ggplot() +
  geom_bar(aes(x=qlregion, y = perc, fill = wco3), stat = "identity") +
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 20)) +
  coord_flip() +
  labs(title = "Résultat final de l’enquête") +
  guides(fill = guide_legend(title = NULL))

# Cercle
dat_plot %>%
  group_by(qlcercle, wco3) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=qlcercle, y = n, fill = qlcercle), stat = "identity") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(title = "Résultat final de l’enquête") +
  facet_wrap(~wco3, scales = "free_y",  labeller = label_wrap_gen()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
dat_plot %>%
  group_by(qlcercle, wco3) %>%
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) %>%
  ggplot() +
  geom_bar(aes(x=qlcercle, y = perc, fill = wco3), stat = "identity") +
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 20)) +
  coord_flip() +
  labs(title = "Résultat final de l’enquête")

# district
dat_plot %>%
  group_by(qdistrict, wco3) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=qdistrict, y = n, fill = qdistrict), stat = "identity") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(title = "Résultat final de l’enquête") +
  facet_wrap(~wco3, scales = "free_y",  labeller = label_wrap_gen()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
dat_plot %>%
  group_by(qdistrict, wco3) %>%
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count)) %>%
  ggplot() +
  geom_bar(aes(x=qdistrict, y = perc, fill = wco3), stat = "identity") +
  scale_fill_discrete(labels = function(x) str_wrap(x, width = 20)) +
  coord_flip() +
  labs(title = "Résultat final de l’enquête")

# To do's
#' Keep those with a completed interview (wco3 == 1)
#' When interview was completed (wco3 == 1), fill in missing interview dates (is.na(qinty)) with dates from vist 3 (qvyear3, qvmonth3, qvday3)
#' Create dates
#' Add factor labels

