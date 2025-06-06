################################################################################
#' @description Figures and tables for qsecover and qwsec01
#' @return Figures and tables covering number of respondents and dates of visits
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(kableExtra)
library(ggplot2)
#' Inputs
dat <- readRDS("./gen/sec01/output/qsecover-qwsec01.rds")
################################################################################

# number of individuals interviewed
length(unique(dat$qsecover_id)) # 26357
length(unique(dat$level_1_id)) # 26357


tab1 <- dat %>%
  group_by(qlregion) %>%
  summarise(cercle = n_distinct(qcercle),
            district = n_distinct(qdistrict),
            commune = n_distinct(wi3),
            village = n_distinct(wi4),
            grappe = n_distinct(grappe))
tab2 <- dat %>%
  group_by(qlregion, grappe, w_enum, w_cons) %>%
  summarise(menage = n_distinct(w_men)) %>%
  group_by(qlregion) %>%
  summarise(menage = sum(menage))
tab1 <- merge(tab1, tab2, by = "qlregion")

# number of women
nind <- dat %>%
  group_by(qlregion) %>%
  summarise(femme = n())
# this doesn't get the same tally by grouping. need the level_1_id to have truly unique indiv
dat %>%
  group_by(qlregion, qcercle, qdistrict, wi3, wi4, wi5, grappe, w_enum, w_cons, w_men) %>%
  summarise(n = n_distinct(w_ind)) %>%
  group_by(qlregion) %>%
  summarise(n = sum(n))

compint <- dat %>%
   group_by(qlregion, wco3) %>%
   summarise(count = n()) %>% 
   group_by(qlregion) %>%
   mutate(perc = sprintf("%0.1f",round(count/sum(count) * 100, 1))) %>%
   filter(wco3 == "Complet") %>%
   select(qlregion, perc)
compinttotal <- dat %>%
  group_by(wco3) %>%
  summarise(count = n()) %>% 
  mutate(perc = sprintf("%0.1f",round(count/sum(count) * 100, 1))) %>%
  filter(wco3 == "Complet")
compinttotal <- compinttotal$perc
tab1 <- merge(tab1, nind, by = "qlregion")
tab1 <- merge(tab1, compint, by = "qlregion") %>%
  rename(region = qlregion)
tab1 <- rbind(tab1, c("Total", colSums(tab1[,-c(1,ncol(tab1))]), compinttotal))
names(tab1)[which(names(tab1) == "aire_sanitaire")] <- "aire sanitaire"
names(tab1)[which(names(tab1) == "perc")] <- "% int. complet"
tab1[is.na(tab1)] <- ""
kbl(tab1, format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
    format.args = list(big.mark = ",", scientific = FALSE), 
    caption = "Geographic coverage and interview completion by region.") %>% 
    column_spec(ncol(tab1), width = "5em")


# p1 <- dat %>%
#   group_by(qlregion, wco3) %>%
#   summarise(count = n()) %>% 
#   mutate(perc = count/sum(count) * 100) %>%
#   ggplot() +
#   geom_bar(aes(x=qlregion, y = perc, fill = wco3), stat = "identity") +
#   labs(title = "Résultat final de l’enquête", x = "", y = "Pourcentage") +
#   guides(fill = guide_legend(title = NULL)) +
#   scale_fill_viridis_d(labels = label_wrap_gen(width = 20), option = "C", na.value = "grey80") +
#   theme_bw() +
#   theme(text = element_text(size = 10))
# ggsave("./gen/prep-fph/figures-baseline/responses.png", p1, dpi = 300, width = 6, height = 3)

# Visit range, multiple views
visrange <- range(dat$qintdate, dat$qvdate1, dat$qvdate2, dat$qvdate3, na.rm = TRUE)
visrange


# number of interviews per person
# i count this by counting the visit dates. there are 60 without a visit date.
nrow(subset(dat, is.na(qintdate))) # 60
nrow(subset(dat, is.na(qvdate1) & is.na(qvdate2) & is.na(qvdate3))) # 60
# these are 56 C'est pas la bonne personne, and 4 missing
# count these as having 1 interview for the purposes of this table
table(subset(dat, is.na(qintdate))$wco3, useNA = "always")
tab2 <- dat %>%
  select(level_1_id, qvdate1, qvdate2, qvdate3) %>% 
  mutate(nvis = case_when(
    !is.na(qvdate3) ~ 3,
    TRUE ~ NA)) %>%
  mutate(nvis = case_when(
     is.na(nvis) & !is.na(qvdate2) ~ 2,
     TRUE ~ nvis)) %>%
  mutate(nvis = case_when(
     is.na(nvis) & !is.na(qvdate1) ~ 1,
     TRUE ~ nvis)) %>%
  mutate(nvis = case_when(
    is.na(nvis) ~ 1,
    TRUE ~ nvis)) %>%
  group_by(nvis) %>%
  summarise(n = n())
tab2$per <- tab2$n/sum(tab2$n)
tab2 <- rbind(tab2, data.frame(nvis = "Total", n = sum(tab2$n), per = 1))
tab2$per <- round(tab2$per*100)
kbl(tab2, format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
    format.args = list(big.mark = ",", scientific = FALSE), 
    col.names = c("Visits", "Number of respondents", "%"),
    caption = "Number of visits made to conduct interview per person.")


# Visit dates by number ---------------------------------------------------


p2 <- dat %>%
  select(level_1_id, qvdate1, qvdate2, qvdate3) %>% 
  pivot_longer(
    cols = -level_1_id
  ) %>%
  mutate(name = case_when(
    name == "qvdate1" ~ 1,
    name == "qvdate2" ~ 2,
    name == "qvdate3" ~ 3,
    TRUE ~ NA
  )) %>%
  group_by(name, value) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=value, y = n), fill = "#0D0887FF", stat = "identity") +
  labs(x = "", title = "Visit date by visit number") + #  
  facet_wrap(~name) +
  theme_bw() + theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/sec01/figures/visit-dates.png", p2, dpi = 300, width = 6, height = 2)


# Respondent age ----------------------------------------------------------

# number of ages imputed
table(dat$q111_imp_ind)
round(100 * prop.table(table(dat$q111_imp_ind)), 1)

p3 <- dat %>%  
  group_by(q111_comb) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = q111_comb, y = n), stat = "identity", fill = "#0D0887FF") + #  fill = barfill
  labs(y = "n", x = "Years", title = "Age of respondent") +
  scale_x_continuous(breaks = c(15, 25, 35, 45)) +
  guides(fill="none") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/sec01/figures/respondent-age.png", p3, dpi = 300, width = 3, height = 2)
