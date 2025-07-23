################################################################################
#' @description 
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(viridis)
library(cowplot)
library(ggpubr)
library(kableExtra)
library(stringi)
#' Inputs
source("./src/utils.R")
dat <- readRDS("./gen/sbh/temp/sbh-clean.rds")
dat_tfr <- readRDS("./gen/mort/output/demog-tfr-reg.rds")
dhs <- readRDS("./gen/dhs/output/mldhs-sbh.rds")
dhs_reg <- readRDS("./gen/dhs/output/mldhs-sbh-reg.rds")
dhs_prop_reg <- readRDS("./gen/dhs/output/mldhs-sbh-prop-reg.rds")
dhs_reg_cohort <- readRDS("./gen/dhs/output/mldhs-sbh-cohort-reg.rds")
dhs_reg_parity <- readRDS("./gen/dhs/output/mldhs-sbh-parity-reg.rds")
dhs_reg_tfr <- readRDS("./gen/dhs/output/mldhs-tfr-reg.rds")
################################################################################

# Children born by respondent age -----------------------------------------

dat %>%
  group_by(agecat_resp, q208) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=q208, y = n), stat = "identity") +
  facet_wrap(~agecat_resp)

# Children born by age of mother ----------------------------------------------------

p <- dat %>%
  group_by(agecat_resp, q208) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=q208, y = n, fill = agecat_resp), position = "dodge", stat = "identity") +
  scale_fill_viridis_d(option = "C", name = "Âge")  +
  labs(
    title = "Nombre total d'enfants nés selon l’âge des enquêtées",
    x = "Nombre total d'enfants nés",
    y = "n"
  ) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/sbh/figures/sbh-children-born-byage.png", p, dpi = 300, width = 5, height = 3)

dat %>%
  group_by(agecat_resp, q212) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=q212, y = n, fill = agecat_resp), position = "dodge", stat = "identity") +
  scale_fill_viridis_d(option = "C", name = "Âge")  +
  labs(
    title = "Nombre total de grossesses selon l’âge des enquêtées",
    x = "Nombre total de grossesses",
    y = "n"
  ) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))

# Figure~\ref{fig:sbh-children-born-byage} shows the total number of children born by respondent age. In the summary birth history, this is a sum of reported children living with the respondent, living elsewhere, and deceased. It does not include pregnancy losses or stillbirths (unless reported as part of children deceased). The distribution looks reasonable with the vast majority of respondents aged 15-19 having only 1 birth, and the distributions gradually shifting rightward with age, reflecting higher parity among older respondents.


# Table for children born, surviving --------------------------------------

# q208 is total children born
# q203_comb is total living at home
# q205_comb is total living elsewhere
# q207_comb is total died
dat %>%
  filter(q208 != 0) %>% #drop those with 0 children for comparison with DHS
  group_by(agecat_resp) %>%
  summarise(avg_total = sprintf("%0.2f", round(mean(q208), 2)),
            avg_surv = sprintf("%0.2f", round(mean(q203_comb + q205_comb), 2)),
            avg_died = sprintf("%0.2f", round(mean(q207_comb), 2)))  %>%
  kbl(format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
      format.args = list(big.mark = ",", scientific = FALSE), 
      col.names = c("Mother age", "Ever born", "Surviving", "Died"), 
      label = c("sbh-avgn"),
      caption = "Average children ever born, surviving, and died by age of mother.") 


# Fertility table ---------------------------------------------------------

# proportion childless
# comes from the SBH for REACH-Mali
# for DHS this was calculated from the individual recode because those with 0 births were not included in birth recode SBH
nrow(subset(dat, is.na(q208))) # 0
tab1 <- dat %>%
  mutate(v024 = tolower(qlregion)) %>%
  mutate(q208_cat = cut(q208, breaks = c(-Inf, 0,1,2,3,4,5,6,Inf),
                        labels = c("0","1","2","3","4","5","6", "7+"))) %>% 
  #select(q203_comb, q205_comb, q207_comb, q208, q208_cat, q212) %>% filter(q208_cat == 0) %>% View
  group_by(v024, q208_cat) %>% 
  summarise(n = n()) %>%
  group_by(v024) %>%
  mutate(total = sum(n),
         per = n/total) %>%
  mutate(source = "REACH-Mali") %>% 
  filter(q208_cat == 0) %>%
  bind_rows(dhs_prop_reg %>% 
              rename(q208_cat = v201_cat) %>%
              group_by(source, v024, q208_cat) %>%
              summarise(n = sum(n)) %>%
              group_by(source, v024) %>%
              mutate(total = sum(n),
                     per = n/total) %>%
              filter(q208_cat == 0)) %>%
  filter(!(v024 %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou")),
         !(source %in% c("ML2021MIS", "ML2015MIS"))) %>%
  mutate(v024 = stri_trans_general(str = v024, id = "Latin-ASCII"),
         v024 = stri_trans_totitle(v024),
         per = sprintf("%0.2f", round(per*100, 2))) %>%
  select(source, v024, per) %>% 
  pivot_wider(names_from = v024, values_from = per) %>%
  arrange(source) %>%
  mutate(variable = "% childless") %>%
  rename(Source = source)

# average parity
# calculated from SBH for DHS and Reach Mali
# had to drop those with zero births in reach mali
tab2 <- dat %>%
  mutate(v024 = tolower(qlregion)) %>%
  group_by(v024) %>%
  filter(q208 != 0) %>% #drop those with 0 children for comparison with DHS
  summarise(avg_total = mean(q208)) %>%
  mutate(source = "REACH-Mali") %>% 
  bind_rows(dhs_reg_parity) %>%
  filter(!(v024 %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  mutate(v024 = stri_trans_general(str = v024, id = "Latin-ASCII"),
         v024 = stri_trans_totitle(v024),
         avg_total = sprintf("%0.2f", round(avg_total, 2))) %>%
  pivot_wider(names_from = v024, values_from = avg_total) %>%
  arrange(source) %>%
  mutate(variable = "Average parity") %>%
  rename(Source = source)

# tfr
# calculated from individual recode in DHS
# in reach mali, had to create and individual recode file
# it contained a count of all live births from the FPH, and also appended women with zero births who weren't administered and FPH
tab3 <- dat_tfr %>%
  rename(v024 = qlregion) %>%
  group_by(v024) %>%
  mutate(source = "REACH-Mali") %>%
  bind_rows(dhs_reg_tfr %>% rename(source = SurveyId)) %>%
  filter(!(v024 %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou")),
           !(source %in% c("ML2021MIS", "ML2015MIS"))) %>%
  mutate(v024 = stri_trans_general(str = v024, id = "Latin-ASCII"),
         v024 = stri_trans_totitle(v024)) %>%
  mutate(ci = paste0("(", sprintf("%0.2f", round(tfr-1.96*se_tfr, 2)),
                                  ", ", sprintf("%0.2f", round(tfr+1.96*se_tfr, 2)), ")"),
         tfr = sprintf("%0.2f", round(tfr, 2)),
         combo = paste0(tfr, " ", ci)) %>%
  select(-c(tips, tfr, se_tfr, ci)) %>%
  pivot_wider(names_from = v024, values_from = combo) %>%
  arrange(source) %>%
  mutate(variable = "TFR") %>%
  rename(Source = source)


tabcomb <- tab1 %>% 
  bind_rows(tab2) %>%
  bind_rows(tab3)
  
tabcomb %>%
  select(-variable) %>%
  kbl(format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
      format.args = list(big.mark = ",", scientific = FALSE), 
      label = c("fertility"),
      caption = "Summary statistics for fertility in the Mali DHS and REACH-Mali by region.") %>%
  pack_rows(index = table(fct_inorder(tabcomb$variable))) 


# SBH averages by region --------------------------------------------------

p <- dat %>%
  filter(q208 != 0) %>% #drop those with 0 children for comparison with DHS
  group_by(qlregion, agecat_resp) %>%
  summarise(avg_total = mean(q208),
            avg_surv = mean(q203_comb + q205_comb),
            avg_died = mean(q207_comb)) %>%
  pivot_longer(
    cols = -c(qlregion, agecat_resp)
  ) %>%
  mutate(value = as.numeric(as.character(value)),
         name = case_when(
           name == "avg_total" ~ "Ever born",
           name == "avg_surv" ~ "Surviving",
           name == "avg_died" ~ "Died"
         ),
         name = factor(name, levels = c("Ever born", "Surviving", "Died"))) %>%
  ggplot() +
  geom_bar(aes(x=agecat_resp, y = value, fill = qlregion), stat = "identity", position = "dodge") +
  facet_wrap(~name) +
  labs(y = "Mean", x = "Age", title = "Summary birth history averages by age of mother in REACH-Mali regions") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        axis.text.x = element_text(angle = 30, hjust = .75),
        legend.title = element_blank())

ggsave("./gen/sbh/figures/sbh-reg-ceb.png", p, dpi = 300, width = 6, height = 3)


# Cohort comparison -------------------------------------------------------

p <- dat %>%
  mutate(v024 = tolower(qlregion),
         age_group2025 = agecat_resp) %>%
  filter(!(age_group2025 %in% c("15-19", "20-24"))) %>%
  group_by(v024, age_group2025, agecat_resp) %>%
  filter(q208 != 0) %>% #drop those with 0 children for comparison with DHS
  summarise(avg_total = sprintf("%0.2f", round(mean(q208), 2)),
            avg_surv = sprintf("%0.2f", round(mean(q203_comb + q205_comb), 2)),
            avg_died = sprintf("%0.2f", round(mean(q207_comb), 2))) %>% 
  mutate(source = "REACH-Mali") %>% 
  bind_rows(dhs_reg_cohort) %>% 
  filter(!(v024 %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  mutate(v024 = stri_trans_general(str = v024, id = "Latin-ASCII"),
         v024 = stri_trans_totitle(v024)) %>%
  pivot_longer(
    cols = -c(SurveyYear, source, age_group2025, agecat_resp, v024)
  ) %>%
  mutate(value = as.numeric(as.character(value)),
         name = case_when(
           name == "avg_total" ~ "Ever born",
           name == "avg_surv" ~ "Surviving",
           name == "avg_died" ~ "Died"
         ),
         name = factor(name, levels = c("Ever born", "Surviving", "Died"))) %>%
  filter(name == "Ever born") %>% 
  mutate(agecat_resp = ifelse(source == "REACH-Mali", NA, agecat_resp)) %>%
  ggplot() +
  geom_bar(aes(x=source, y = value, fill = source), stat = "identity", position = "dodge") +
  geom_text(aes(x= source, y = value+0.5, label = agecat_resp), size = 1.8) +
  facet_grid(v024~age_group2025) +
  labs(y = "Average children ever born", x = "Survey", title = "Cohort comparison of children ever born by age of respondent in REACH-Mali survey", subtitle = "Age of respondent in REACH-Mali survey") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        axis.text.x = element_text(size = 7, angle = 30, hjust = 1),
        legend.title = element_blank(),
        legend.position = "none")

ggsave("./gen/sbh/figures/dhs_reg_cohort.png", p, dpi = 300, width = 6, height = 6)

p <- dat %>%
  mutate(v024 = tolower(qlregion),
         age_group2025 = agecat_resp) %>%
  filter(!(age_group2025 %in% c("15-19", "20-24"))) %>%
  group_by(v024, age_group2025, agecat_resp) %>%
  filter(q208 != 0) %>% #drop those with 0 children for comparison with DHS
  summarise(avg_total = sprintf("%0.2f", round(mean(q208), 2)),
            avg_surv = sprintf("%0.2f", round(mean(q203_comb + q205_comb), 2)),
            avg_died = sprintf("%0.2f", round(mean(q207_comb), 2))) %>% 
  mutate(source = "REACH-Mali") %>% 
  bind_rows(dhs_reg_cohort) %>% 
  filter(!(v024 %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  mutate(v024 = stri_trans_general(str = v024, id = "Latin-ASCII"),
         v024 = stri_trans_totitle(v024)) %>%
  pivot_longer(
    cols = -c(SurveyYear, source, age_group2025, agecat_resp, v024)
  ) %>%
  mutate(value = as.numeric(as.character(value)),
         name = case_when(
           name == "avg_total" ~ "Ever born",
           name == "avg_surv" ~ "Surviving",
           name == "avg_died" ~ "Died"
         ),
         name = factor(name, levels = c("Ever born", "Surviving", "Died"))) %>%
  filter(name == "Died") %>%
  mutate(agecat_resp = ifelse(source == "REACH-Mali", NA, agecat_resp)) %>%
  ggplot() +
  geom_bar(aes(x=source, y = value, fill = source), stat = "identity", position = "dodge") +
  geom_text(aes(x= source, y = value+0.1, label = agecat_resp), size = 1.7) +
  facet_grid(v024~age_group2025) +
  labs(y = "Average children died", x = "Survey", title = "Cohort comparison of deceased children by age of respondent in REACH-Mali survey", subtitle = "Age of respondent in REACH-Mali survey") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        axis.text.x = element_text(size = 7, angle = 30, hjust = 1),
        legend.title = element_blank(),
        legend.position = "none")

ggsave("./gen/sbh/figures/dhs_reg_cohort-deaths.png", p, dpi = 300, width = 6, height = 6)


# SBH averages compared with other DHS ------------------------------------

p <- dat %>%
  group_by(agecat_resp) %>%
  filter(q208 != 0) %>% #drop those with 0 children for comparison with DHS
  summarise(avg_total = sprintf("%0.2f", round(mean(q208), 2)),
            avg_surv = sprintf("%0.2f", round(mean(q203_comb + q205_comb), 2)),
            avg_died = sprintf("%0.2f", round(mean(q207_comb), 2))) %>% 
  mutate(source = "REACH-Mali") %>% 
  bind_rows(dhs) %>%
  pivot_longer(
    cols = c(avg_total, avg_surv, avg_died)
  ) %>%
  mutate(value = as.numeric(as.character(value)),
         name = case_when(
           name == "avg_total" ~ "Ever born",
           name == "avg_surv" ~ "Surviving",
           name == "avg_died" ~ "Died"
         ),
         name = factor(name, levels = c("Ever born", "Surviving", "Died"))) %>%
  ggplot() +
  geom_bar(aes(x=agecat_resp, y = value, fill = source), stat = "identity", position = "dodge") +
  #scale_fill_viridis_d(option = "C", na.value = "grey80") +
  facet_wrap(~name) +
  labs(y = "Mean", x = "Age", title = "Summary birth history averages by age of mother in REACH-Mali and DHS") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        axis.text.x = element_text(angle = 30, hjust = .75),
        legend.title = element_blank())

ggsave("./gen/sbh/figures/sbh-dhs-ceb.png", p, dpi = 300, width = 6, height = 3)


# SBH averages compared with other DHS regions ------------------------------------

p <- dat %>%
  mutate(v024 = tolower(qlregion)) %>%
  group_by(v024, agecat_resp) %>%
  filter(q208 != 0) %>% #drop those with 0 children for comparison with DHS
  summarise(avg_total = sprintf("%0.2f", round(mean(q208), 2)),
            avg_surv = sprintf("%0.2f", round(mean(q203_comb + q205_comb), 2)),
            avg_died = sprintf("%0.2f", round(mean(q207_comb), 2))) %>% 
  mutate(source = "REACH-Mali") %>% 
  bind_rows(dhs_reg) %>%
  mutate(v024 = stri_trans_general(str = v024, id = "Latin-ASCII")) %>%
  pivot_longer(
    cols = c(avg_total, avg_surv, avg_died)
  ) %>%
  mutate(value = as.numeric(as.character(value)),
         name = case_when(
           name == "avg_total" ~ "Ever born",
           name == "avg_surv" ~ "Surviving",
           name == "avg_died" ~ "Died"
         ),
         name = factor(name, levels = c("Ever born", "Surviving", "Died"))) %>%
  filter(!(v024 %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  ggplot() +
  geom_bar(aes(x=agecat_resp, y = value, fill = source), stat = "identity", position = "dodge") +
  facet_grid(v024~name) +
  labs(y = "Mean", x = "Age", title = "Average children ever born, surviving, and died by age of mother") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        axis.text.x = element_text(angle = 30, hjust = .75))

# Save the final figure
ggsave("./gen/sbh/figures/sbh-dhs-reg-ceb.png", p, dpi = 300, width = 6, height = 6)

# SBH proportions compared with other DHS regions ------------------------------------

dat %>%
  mutate(v024 = tolower(qlregion)) %>%
  mutate(q208_cat = cut(q208, breaks = c(-Inf, 0,1,2,3,4,5,6,Inf),
                        labels = c("0","1","2","3","4","5","6", "7+"))) %>%
  group_by(v024, agecat_resp, q208_cat) %>% 
  summarise(n = n()) %>%
  group_by(v024, agecat_resp) %>%
  mutate(total = sum(n),
            per = n/total) %>%
  mutate(source = "REACH-Mali") %>% 
  bind_rows(dhs_prop_reg %>% rename(q208_cat = v201_cat)) %>%
  mutate(v024 = stri_trans_general(str = v024, id = "Latin-ASCII")) %>%
  filter(!(v024 %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  ggplot() +
  geom_bar(aes(x=q208_cat, y = per, fill = source), stat = "identity", position = "dodge") +
  facet_grid(v024~agecat_resp) +
  labs(y = "prop", x = "Age", title = "proportion children ever born by age of mother") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        axis.text.x = element_text(angle = 30, hjust = .75))

# single region, by age
dat %>%
  mutate(v024 = tolower(qlregion)) %>%
  mutate(q208_cat = cut(q208, breaks = c(-Inf, 0,1,2,3,4,5,6,Inf),
                        labels = c("0","1","2","3","4","5","6", "7+"))) %>%
  group_by(v024, agecat_resp, q208_cat) %>% 
  summarise(n = n()) %>%
  group_by(v024, agecat_resp) %>%
  mutate(total = sum(n),
         per = n/total) %>%
  mutate(source = "REACH-Mali") %>% 
  bind_rows(dhs_prop_reg %>% rename(q208_cat = v201_cat)) %>%
  mutate(v024 = stri_trans_general(str = v024, id = "Latin-ASCII")) %>%
  filter(!(v024 %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  filter(v024 %in% "sikasso") %>%
  ggplot() +
  geom_bar(aes(x= q208_cat, y = per), stat = "identity", position = "dodge") +
  facet_grid(source ~ agecat_resp) +
  labs(y = "prop", x = "Age", title = "proportion children ever born by age of mother") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        axis.text.x = element_text(angle = 30, hjust = .75))

# Proportion of children ever born/died by age of mother ------------------

# q208 is total children born
# q205_comb is total living at home
# q205_comb is total living elsewhere
dat %>%
  filter(q208 != 0) %>%
  mutate(propsurv = (q203_comb + q205_comb)/q208) %>%
  group_by(agecat_resp) %>%
  summarise(avgsurv = sprintf("%0.1f", round(mean(propsurv)*100, 1))) %>%
  kbl(format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
      format.args = list(big.mark = ",", scientific = FALSE), 
      col.names = c("Mother age", "%"), 
      caption = "Average proportion children surviving by age of mother.") 

# q208 is total children born
# q207_comb is total died
dat %>%
  filter(q208 != 0) %>%
  mutate(propsurv = (q207_comb)/q208) %>%
  group_by(agecat_resp) %>%
  summarise(avgsurv = sprintf("%0.1f", round(mean(propsurv)*100, 1))) %>%
  kbl(format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
      format.args = list(big.mark = ",", scientific = FALSE), 
      col.names = c("Mother age", "%"), 
      label = c("sbh-propdied"),
      caption = "Average proportion children died by age of mother.") 

# histogram
dat %>%
  filter(q208 != 0) %>%
  mutate(propdied = q207_comb / q208) %>%
  filter(propdied <= 1) %>%
  ggplot(aes(x = propdied)) +
  geom_histogram(fill = "#0D0887FF") +
  facet_wrap(~agecat_resp) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
#ggsave("./gen/fph/figures/cebcd-matage.png", p, dpi = 300, width = 6, height = 6)

# density plot
dat %>%
  filter(q208 != 0) %>%
  mutate(propdied = q207_comb / q208) %>%
  filter(propdied <= 1) %>%
  ggplot(aes(x = propdied)) +
  geom_density(fill = "#0D0887FF") +
  facet_wrap(~agecat_resp, scales = "free_y") +
  labs(x = "", y = "Density") +
  theme_minimal()

# Proportion of children reported by respondent age -----------------------

# interested in higher parity reporting
# group lower parities

dat %>%
  mutate(v024 = tolower(qlregion)) %>%
  mutate(q208_cat = cut(q208, breaks = c(-Inf, 0,1,6,7,8,9,10,11, 12, 13, 14, 15, 16),
                        labels = c("0","1-5","6","7", "8", "9", "10", "11", "12", "13", "14", "15", "16"))) %>% 
  #select(q203_comb, q205_comb, q207_comb, q208, q208_cat, q212) %>% View
  group_by(v024, agecat_resp, q208_cat) %>% 
  summarise(n = n()) %>%
  group_by(v024) %>%
  mutate(total = sum(n),
         per = round(n/total*100, 2)) %>%
  select(v024, agecat_resp, q208_cat, per) %>%
  pivot_wider(
    names_from = v024,
    values_from = per
  )

dat %>%
  mutate(v024 = tolower(qlregion)) %>%
  mutate(q208_cat = cut(q208, breaks = c(-Inf, 0,1,11, 12, 13, 14, 15, 16),
                        labels = c("0","1-10","11", "12", "13", "14", "15", "16"))) %>% 
  #select(q203_comb, q205_comb, q207_comb, q208, q208_cat, q212) %>% View
  group_by(v024, agecat_resp, q208_cat) %>% 
  summarise(n = n()) %>%
  group_by(v024, agecat_resp) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  ggplot() +
  geom_bar(aes(x=agecat_resp, y = per, fill = q208_cat), stat = "identity") +
  facet_wrap(~v024)

dat %>%
  mutate(v024 = tolower(qlregion)) %>%
  mutate(q208_cat = cut(q208, breaks = c(-Inf, 0,1,6,7,8,9,10,11, 12, 13, 14, 15, 16),
                        labels = c("0","1-5","6","7", "8", "9", "10", "11", "12", "13", "14", "15", "16"))) %>% 
  #select(q203_comb, q205_comb, q207_comb, q208, q208_cat, q212) %>% View
  group_by(v024, agecat_resp, q208_cat) %>% 
  summarise(n = n()) %>%
  group_by(v024, agecat_resp) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  ggplot() +
  geom_bar(aes(x=agecat_resp, y = per, fill = q208_cat), stat = "identity") +
  facet_wrap(~v024, nrow = 1)


# Combined plot for pregnancies by age of mother ------------------------------------

# Bar plot with legend
p1_full <- dat %>%
  group_by(agecat_resp, q212) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot() +
  geom_bar(aes(x = q212, y = n, fill = agecat_resp),
           position = "dodge", stat = "identity") +
  scale_fill_viridis_d(option = "C", name = "Âge") +
  labs(
    x = "Nombre total de grossesses",
    y = "n"
  ) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        legend.position = "bottom",
        #panel.border = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank()) +
  guides(fill = guide_legend(nrow = 1))

# Extract and wrap legend for ggarrange
legend <- ggpubr::as_ggplot(ggpubr::get_legend(p1_full)) +  
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )


# Remove legend from plots
p1 <- p1_full + theme(legend.position = "none",
                      #panel.border = element_blank(),
                      plot.margin = margin(0, 0, 0, 0),
                      legend.box.background = element_blank(),
                      legend.background = element_blank())

# Density plot
p2 <- dat %>%
  ggplot() +
  geom_density(aes(x = q212, color = agecat_resp), adjust = 4) +
  scale_color_viridis_d(option = "C", guide = "none") +
  labs(
    x = "Nombre total de grossesses",
    y = "Densité"
  ) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        #panel.border = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank())

# Title as a plot
title_plot <- as_ggplot(text_grob(
  "Nombre total de grossesses selon l’âge des enquêtées",
  size = 10, just = "center"
)) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

# remove lines that appear between ggarrange
p1 <- p1 + theme(plot.margin = margin(0, 0, 0, 0))
p2 <- p2 + theme(plot.margin = margin(0, 0, 0, 0))

# Final combined plot
final_plot <- ggarrange(
  title_plot,
  ggarrange(p1, p2, ncol = 2),
  legend,
  nrow = 3,
  heights = c(0.1, 1, 0.15)
)

# Save the final figure
ggsave("./gen/sbh/figures/sbh-pregnancies-byage-combined.png",
       final_plot, dpi = 300, width = 6, height = 3)


# Combined plot for children born by age ----------------------------------

# Bar plot with legend
p1_full <- dat %>%
  group_by(agecat_resp, q208) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot() +
  geom_bar(aes(x = q208, y = n, fill = agecat_resp),
           position = "dodge", stat = "identity") +
  scale_fill_viridis_d(option = "C", name = "Âge") +
  labs(
    x = "Nombre total d'enfants nés",
    y = "n"
  ) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        legend.position = "bottom",
        #panel.border = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank()) +
  guides(fill = guide_legend(nrow = 1))

# Extract and wrap legend for ggarrange
legend <- ggpubr::as_ggplot(ggpubr::get_legend(p1_full)) +  
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )


# Remove legend from plots
p1 <- p1_full + theme(legend.position = "none",
                      #panel.border = element_blank(),
                      plot.margin = margin(0, 0, 0, 0),
                      legend.box.background = element_blank(),
                      legend.background = element_blank())

# Density plot
p2 <- dat %>%
  ggplot() +
  geom_density(aes(x = q208, color = agecat_resp), adjust = 4) +
  scale_color_viridis_d(option = "C", guide = "none") +
  labs(
    x = "Nombre total d'enfants nés",
    y = "Densité"
  ) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        #panel.border = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank())

# Title as a plot
title_plot <- as_ggplot(text_grob(
  "Nombre total d'enfants nés selon l’âge des enquêtées",
  size = 10, just = "center"
)) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

# remove lines that appear between ggarrange
p1 <- p1 + theme(plot.margin = margin(0, 0, 0, 0))
p2 <- p2 + theme(plot.margin = margin(0, 0, 0, 0))

# Final combined plot
final_plot <- ggarrange(
  title_plot,
  ggarrange(p1, p2, ncol = 2),
  legend,
  nrow = 3,
  heights = c(0.1, 1, 0.15)
)

# Save the final figure
ggsave("./gen/sbh/figures/sbh-children-born-byage-combined.png",
       final_plot, dpi = 300, width = 6, height = 3)

# Combined plot for children died by age ----------------------------------


# Bar plot with legend
p1_full <- dat %>%
  filter(!is.na(q207_comb) & q207_comb != 0) %>%
  group_by(agecat_resp, q207_comb) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=q207_comb, y = n, fill = agecat_resp), position = "dodge", stat = "identity") +
  scale_fill_viridis_d(option = "C", name = "Âge")  +
  scale_x_continuous(breaks = 1:10) +
  labs(
    #title = "Nombre total d'enfants décédés selon l’âge des enquêtées",
    subtitle = "a",
    x = "Nombre total d'enfants décédés",
    y = "n"
  ) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        legend.position = "bottom",
        #panel.border = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank()) +
  guides(fill = guide_legend(nrow = 1))

# Extract and wrap legend for ggarrange
legend <- ggpubr::as_ggplot(ggpubr::get_legend(p1_full)) +  
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )


# Remove legend from plots
p1 <- p1_full + theme(legend.position = "none",
                      #panel.border = element_blank(),
                      plot.margin = margin(0, 0, 0, 0),
                      legend.box.background = element_blank(),
                      legend.background = element_blank())

# Truncating plot because these high numbers don't show up in bars
dat %>%
  filter(!is.na(q211) & q211 != 0) %>%
  group_by(agecat_resp, q211) %>%
  summarise(n = n()) %>%
  filter(q211 > 6) %>%
  group_by(q211) %>%
  summarise(sum(n))

# other bar plot
p2 <- dat %>%
  filter(!is.na(q211) & q211 != 0) %>%
  group_by(agecat_resp, q211) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=q211, y = n, fill = agecat_resp), position=position_dodge(preserve = "single"), stat = "identity") +
  scale_fill_viridis_d(option = "C", name = "Âge")  +
  scale_x_continuous(breaks = 1:10) +
  labs(
    #title = "Nombre de naissances non vivantes selon l’âge des enquêtées",
    subtitle = "b",
    x = "Nombre de naissances non vivantes",
    y = "n"
  ) +
  coord_cartesian(x = c(0.75, 6.25)) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        #panel.border = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.position = "none",
        legend.box.background = element_blank(),
        legend.background = element_blank())

# Title as a plot
title_plot <- as_ggplot(text_grob(
  "Nombre total d'enfants (a) décédés et (b) pertes de grossesse selon l’âge des enquêtées",
  size = 10, just = "center"
)) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )



# remove lines that appear between ggarrange
p1 <- p1 + theme(plot.margin = margin(0, 0, 0, 0))
p2 <- p2 + theme(plot.margin = margin(0, 0, 0, 0))

# Final combined plot
final_plot <- ggarrange(
  title_plot,
  ggarrange(p1, p2, ncol = 2),
  legend,
  nrow = 3,
  heights = c(0.1, 1, 0.15)
)

# Save the final figure
ggsave("./gen/sbh/figures/sbh-children-died-pregloss-combined.png",
       final_plot, dpi = 300, width = 6, height = 3)

