################################################################################
#' @description Figures for mortality comparisons with old Mali DHS
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(viridis)
library(scales)
library(cowplot)
library(ggpubr)
library(pammtools) # geom_stepribbon
library(stringr) # str_wrap
library(stringi)
#' Inputs
reach <- readRDS("./gen/mort/output/reach-rates.rds")
gap <- readRDS("./gen/mort/output/gapu5m-for-plots.rds")

dhsnat <- readRDS("./gen/dhs/output/mldhs-rates-nat.rds")
dhsreg <- readRDS("./gen/dhs/output/mldhs-rates-reg.rds")
dhsres <- readRDS("./gen/dhs/output/mldhs-rates-res.rds")

dhsnat_gapu5m <- readRDS("./gen/dhs/output/mldhs-plot-nat.rds")
dhsreg_gapu5m <- readRDS("./gen/dhs/output/mldhs-plot-reg.rds")
dhsres_gapu5m <- readRDS("./gen/dhs/output/mldhs-plot-res.rds")
################################################################################

df_reach <- reach
df_reach$source <- "REACH-Mali"
df_reach$tips <- df_reach$cut_time
df_reach <- df_reach[,c("type", "byvar", "tips", "agegrp", "source", "qx")]
df_reach$byvar <- tolower(df_reach$byvar)

df_gap <- gap
df_gap$source <- "REACH-Mali"
df_gap$byvar <- tolower(df_gap$byvar)

df_dhsnat <- dhsnat
df_dhsnat$source <- df_dhsnat$SurveyId
df_dhsnat$qx <- df_dhsnat$est
df_dhsnat$type <- "All"
df_dhsnat$byvar <- "all"
df_dhsnat <- df_dhsnat[,c("type", "byvar", "tips", "agegrp", "source", "qx")]

df_dhsres <- dhsres
df_dhsres$source <- df_dhsres$SurveyId
df_dhsres$qx <- df_dhsres$est
df_dhsres$type <- "Residence"
df_dhsres$byvar <- as.character(df_dhsres$v025)
df_dhsres <- df_dhsres[,c("type", "byvar", "tips", "agegrp", "source", "qx")]

df_dhsreg <- dhsreg
df_dhsreg$source <- df_dhsreg$SurveyId
df_dhsreg$qx <- df_dhsreg$est
df_dhsreg$type <- "Region"
df_dhsreg$byvar <- as.character(df_dhsreg$v024)
df_dhsreg <- df_dhsreg[,c("type", "byvar", "tips", "agegrp", "source", "qx")]
df_dhsreg$byvar <- tolower(df_dhsreg$byvar)
df_dhsreg$byvar <- stri_trans_general(str = df_dhsreg$byvar, id = "Latin-ASCII")

df_dhsreg_gapu5m <- dhsreg_gapu5m
df_dhsreg_gapu5m$byvar <- tolower(df_dhsreg_gapu5m$byvar)
df_dhsreg_gapu5m$byvar <- stri_trans_general(str = df_dhsreg_gapu5m$byvar, id = "Latin-ASCII")


# Ab plots ----------------------------------------------------------------

df_reach %>%
  filter(type == "Region") %>% 
  bind_rows(df_dhsreg) %>%
  filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  pivot_wider(
    id_cols = c(type, byvar, tips, source),
    names_from = agegrp,
    values_from = qx
  ) %>%
  ggplot() +
  geom_point(aes(x=q0to28d, y = q28dto1y, color = source, shape = tips)) +
  facet_wrap(~byvar)

df_reach %>%
  filter(type == "Region") %>% 
  bind_rows(df_dhsreg) %>%
  filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  pivot_wider(
    id_cols = c(type, byvar, tips, source),
    names_from = agegrp,
    values_from = qx
  ) %>%
  ggplot() +
  geom_point(aes(x=q0to28d, y = q1to59m, color = source, shape = tips)) +
  facet_wrap(~byvar)

df_reach %>%
  filter(type == "Region") %>% 
  bind_rows(df_dhsreg) %>%
  filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  pivot_wider(
    id_cols = c(type, byvar, tips, source),
    names_from = agegrp,
    values_from = qx
  ) %>%
  ggplot() +
  geom_point(aes(x=q0to1y, y = q1to5y, color = source, shape = tips)) +
  facet_wrap(~byvar)


# Age pattern -------------------------------------------------------------

p <- df_reach %>%
  filter(type == "Region") %>% 
  bind_rows(df_dhsreg) %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  pivot_wider(
    id_cols = c(type, byvar, tips, source),
    names_from = agegrp,
    values_from = qx
  ) %>%
  mutate(ratio = q1to5y/q0to1y/q0to5y) %>%
  mutate(byvar = stri_trans_totitle(byvar)) %>%
  ggplot() +
  geom_bar(aes(x=source, y= ratio, fill = source), stat = "identity") +
  labs(x = "", y = "[q(1y,5y)/q(1y)]/q(5y)", title= "Age pattern against overall level of mortality",
       subtitle = "Ratio of q(1y,5y):q(1y), divided by q(5y)") +
  facet_grid(byvar~tips) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(),
        legend.position = "none")
ggsave("./gen/mort/figures/dhs-reg-agepat1.png", p, dpi = 300, width = 6, height = 6)

p <- df_reach %>%
  filter(type == "Region") %>% 
  bind_rows(df_dhsreg) %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  pivot_wider(
    id_cols = c(type, byvar, tips, source),
    names_from = agegrp,
    values_from = qx
  ) %>%
  mutate(ratio = q1to59m/q0to28d/q0to5y) %>%
  mutate(byvar = stri_trans_totitle(byvar)) %>%
  ggplot() +
  geom_bar(aes(x=source, y= ratio, fill = source), stat = "identity") +
  #labs(x = "", y = "[q(1m,59m)/q(1m)]/q(5y)", title= "Age pattern against overall level of mortality",
  #    subtitle = "Ratio of q(1m,59m):q(1m), divided by q(5y)") +
  labs(x = "", y = "[q(28d,59m)/q(28d)]/q(60m)", title= "Age pattern against overall level of mortality",
       subtitle = "Ratio of postneonatal:neonatal mortality, divided by under-5 mortality") +
  facet_grid(byvar~tips) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(),
        legend.position = "none")
ggsave("./gen/mort/figures/dhs-reg-agepat2.png", p, dpi = 300, width = 6, height = 6)

p <- df_reach %>%
  filter(type == "Region") %>% 
  bind_rows(df_dhsreg) %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  pivot_wider(
    id_cols = c(type, byvar, tips, source),
    names_from = agegrp,
    values_from = qx
  ) %>%
  mutate(ratio = q28dto1y/q0to28d/q0to1y) %>%
  ggplot() +
  geom_bar(aes(x=source, y= ratio, fill = source), stat = "identity") +
  labs(x = "", y = "[q(28d,1y)/q(28d)]/q(1y)", title= "Age pattern against overall level of mortality",
       subtitle = "Ratio of q(28d,1y):q(28d), divided by q(1y)") +
  facet_grid(byvar~tips) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
ggsave("./gen/mort/figures/dhs-reg-agepat3.png", p, dpi = 300, width = 6, height = 6)


# Proportion --------------------------------------------------------------

# Proportion of under-5 deaths that are neonatal
p <- df_gap %>%
  filter(type == "Region") %>%
  bind_rows(df_dhsreg_gapu5m) %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14"))) %>%
  filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  filter(Qx != 0) %>% # remove first row that is only there for plotting purposes
  mutate(agegrp = ifelse(age_d < 28, "Neonatal", "Post")) %>%
  group_by(source, type, byvar, tips, agegrp) %>%
  summarise(n_dth = sum(events)) %>%
  pivot_wider(
    names_from = agegrp,
    values_from = n_dth
  ) %>%
  group_by(source, type, byvar, tips) %>%
  mutate(total = Neonatal + Post) %>%
  mutate(frac = Neonatal/total) %>%
  mutate(byvar = stri_trans_totitle(byvar)) %>%
  ggplot() +
  geom_bar(aes(x=source, y=frac, fill = source), stat = "identity") +
  labs(x = "", y = "Proportion", title= "Proportion of under-5 deaths that are neonatal") +
  facet_grid(byvar~tips) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank(),
        legend.position = "none")
ggsave("./gen/mort/figures/dhs-reg-propneo.png", p, dpi = 300, width = 6, height = 6)


# Proportion of under-5 deaths that are infant
p <- df_gap %>%
  filter(type == "Region") %>%
  bind_rows(df_dhsreg_gapu5m) %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14"))) %>%
  filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  filter(Qx != 0) %>% # remove first row that is only there for plotting purposes
  mutate(agegrp = ifelse(age_y < 1, "Infant", "Post")) %>%
  group_by(source, type, byvar, tips, agegrp) %>%
  summarise(n_dth = sum(events)) %>%
  pivot_wider(
    names_from = agegrp,
    values_from = n_dth
  ) %>%
  group_by(source, type, byvar, tips) %>%
  mutate(total = Infant + Post) %>%
  mutate(frac = Infant/total) %>%
  ggplot() +
  geom_bar(aes(x=source, y=frac, fill = source), stat = "identity") +
  labs(x = "", y = "Proportion", title= "Proportion of under-5 deaths that are infant") +
  facet_grid(byvar~tips) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())
ggsave("./gen/mort/figures/dhs-reg-propinf.png", p, dpi = 300, width = 6, height = 6)


# Qx ----------------------------------------------------------------------


# Cumulative Qx by region
p <- df_gap %>%
  filter(type == "Region") %>%
  bind_rows(df_dhsreg_gapu5m) %>%
  mutate(cut_time = factor(cut_time, levels = c("0-4", "5-9", "10-14"))) %>%
  filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  ggplot() +
  geom_step(aes(x = age_y, y = Qx*1000, color = source)) +
  labs(x = "Age (years)", y = "Q(x)", title = "Cumulative probability of dying by region") +
  facet_grid(byvar~cut_time) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8), legend.title = element_blank())
ggsave("./gen/mort/figures/dhs-reg-qx.png", p, dpi = 300, width = 6, height = 6)

# Cumulative Qx by survey
df_gap %>%
  filter(type == "Region") %>%
  bind_rows(df_dhsreg_gapu5m) %>%
  mutate(cut_time = factor(cut_time, levels = c("0-4", "5-9", "10-14"))) %>%
  filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  ggplot() +
  geom_step(aes(x = age_y, y = Qx*1000, color = byvar)) +
  labs(x = "Age (years)", y = "Q(x)", title = "Cumulative probability of dying by region") +
  facet_grid(source~cut_time) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8), legend.title = element_blank())
# rates by survey
df_reach %>%
  filter(type == "Region") %>% 
  bind_rows(df_dhsreg) %>%
  filter(agegrp == "q0to5y") %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  ggplot() +
  geom_bar(aes(x=source, y= qx, fill = byvar), stat = "identity", position = "dodge") +
  facet_grid(agegrp~tips) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.title = element_blank())

df_gap %>%
  filter(type == "Region") %>%
  bind_rows(df_dhsreg_gapu5m) %>%
  mutate(cut_time = factor(cut_time, levels = c("0-4", "5-9", "10-14"))) %>%
  filter(byvar =="sikasso" & (source %in% "REACH-Mali" & cut_time == "10-14") | 
           byvar =="sikasso" & (source %in% "ML2018DHS" & cut_time == "5-9") | 
           byvar =="sikasso" &  (source == "ML2012DHS" & cut_time == "0-4")) %>% 
  ggplot() +
  geom_step(aes(x = age_y, y = Qx*1000, color = source)) +
  labs(x = "Age (years)", y = "Q(x)", title = "Cumulative probability of dying by region") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8), legend.title = element_blank())

# checking matching with DHS surveys
unique(dhsnat_gapu5m$source)
subset(dhsnat_gapu5m, cut_time == "0-4" & age_y_up == 5 & source == "ML2001DHS") # 221 compared to 229 in DHS
subset(dhsnat_gapu5m, cut_time == "0-4" & age_d_up == 28 & source == "ML2006DHS") # 46 compared to 46 in dhs
subset(dhsnat_gapu5m, cut_time == "0-4" & age_y_up == 5 & source == "ML2006DHS") # 182 compared to 191 in DHS
subset(dhsnat_gapu5m, cut_time == "0-4" & age_d_up == 28 & source == "ML2012DHS") # 34 compared to 34 in dhs
subset(dhsnat_gapu5m, cut_time == "0-4" & age_y_up == 5 & source == "ML2012DHS") # 96 compared to 95 in DHS
subset(dhsnat_gapu5m, cut_time == "0-4" & age_d_up == 28 & source == "ML2018DHS") # 32 compared to 33 in DHS
subset(dhsnat_gapu5m, cut_time == "0-4" & age_y_up == 5 & source == "ML2018DHS")  # 98 compared to 101 in DHS
# 2023-24 DHS: neonatal 29
# 2023-24 DHS: 5q0 87

# Trends ------------------------------------------------------------------

df_dhsreg %>%
  bind_rows(df_reach %>% filter(type == "Region")) %>%
  filter(agegrp == "q0to5y") %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  mutate(byvar = tools::toTitleCase(byvar)) %>%
  mutate(SurveyYear = as.numeric(substr(source, 3, 6)),
         SurveyYear = ifelse(source == "REACH-Mali", 2025, SurveyYear),
         year_m = SurveyYear - 2.5,
         year_m = case_when(
           tips == "5-9" ~ SurveyYear - 7.5,
           tips == "10-14" ~ SurveyYear - 12.5,
           TRUE ~ year_m
         ),
         year_l = year_m - 2.5,
         year_u = year_m + 2.5) %>%
  ggplot() + 
    geom_point(aes(x = year_m, y = qx*1000, color = byvar, shape = tips)) +
    geom_line(aes(x = year_m, y = qx*1000, color = byvar, group = interaction(source, byvar))) +
  annotate(geom="text", x=2015, y = 50, label="REACH-Mali", hjust = 0) +
  labs(x = "Age (years)", y = "Q(x)", title = "Under-5 mortality by region in REACH-Mali and DHS") +
  scale_shape_discrete(name = "Time prior to survey") +
  scale_color_discrete(name = "Region") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))



label_data <- df_dhsreg %>%
  bind_rows(df_reach %>% filter(type == "Region")) %>%
  filter(agegrp == "q0to5y") %>%
  filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  mutate(
    byvar = tools::toTitleCase(byvar),
    tips = factor(tips, levels = c("0-4", "5-9", "10-14")),
    SurveyYear = as.numeric(substr(source, 3, 6)),
    SurveyYear = ifelse(source == "REACH-Mali", 2025, SurveyYear),
    year_m = case_when(
      tips == "0-4" ~ SurveyYear - 2.5,
      tips == "5-9" ~ SurveyYear - 7.5,
      tips == "10-14" ~ SurveyYear - 12.5
    )
  ) %>%
  group_by(source, byvar) %>%
  filter(year_m == max(year_m)) %>%   # Get latest year per line
  ungroup() %>%
  mutate(label = source)
df_dhsreg %>%
  bind_rows(df_reach %>% filter(type == "Region")) %>%
  filter(agegrp == "q0to5y") %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  mutate(byvar = tools::toTitleCase(byvar)) %>%
  mutate(SurveyYear = as.numeric(substr(source, 3, 6)),
         SurveyYear = ifelse(source == "REACH-Mali", 2025, SurveyYear),
         year_m = SurveyYear - 2.5,
         year_m = case_when(
           tips == "5-9" ~ SurveyYear - 7.5,
           tips == "10-14" ~ SurveyYear - 12.5,
           TRUE ~ year_m
         ),
         year_l = year_m - 2.5,
         year_u = year_m + 2.5) %>%
  ggplot() + 
  geom_point(aes(x = year_m, y = qx*1000, color = byvar, shape = tips)) +
  geom_line(aes(x = year_m, y = qx*1000, color = byvar, group = interaction(source, byvar))) +
  geom_label(data = label_data,
             aes(x = year_m + 0.5, y = qx * 1000, label = label, fill = byvar),
             color = "black", alpha = 0.3, hjust = 0, size = 2.5, show.legend = FALSE) +
  labs(x = "Age (years)", y = "Q(x)", title = "Under-5 mortality by region in REACH-Mali and DHS") +
  scale_shape_discrete(name = "Time prior to survey") +
  scale_color_discrete(name = "Region") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))


# Region trends with label ------------------------------------------------

ml2024 <- data.frame(source = rep("ML2024DHS"),
                     agegrp = rep("q0to5y"),
                     qx = c(0.087, 0.111, 0.132),
                     tips = c("0-4", "5-9", "10-14"),
                     byvar = "National")

# Labels in the mix
# label_data <- df_dhsreg %>%
#   bind_rows(ml2024) %>%
#   bind_rows(df_reach %>% filter(type == "Region")) %>%
#   filter(agegrp == "q0to5y") %>%
#   filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
#   mutate(
#     byvar = tools::toTitleCase(byvar),
#     tips = factor(tips, levels = c("0-4", "5-9", "10-14")),
#     SurveyYear = as.numeric(substr(source, 3, 6)),
#     SurveyYear = ifelse(source == "REACH-Mali", 2025, SurveyYear),
#     year_m = case_when(
#       tips == "0-4" ~ SurveyYear - 2.5,
#       tips == "5-9" ~ SurveyYear - 7.5,
#       tips == "10-14" ~ SurveyYear - 12.5
#     ),
#     qx_scaled = qx * 1000
#   ) %>%
#   group_by(source) %>%
#   filter(tips == "0-4") %>%
#   summarise(
#     x = max(year_m+2.5, na.rm = TRUE),
#     y = mean(qx_scaled, na.rm = TRUE),
#     label = unique(source)
#   )
# Labels on top
label_data <- df_dhsreg %>%
  bind_rows(ml2024) %>%
  bind_rows(df_reach %>% filter(type == "Region")) %>%
  filter(agegrp == "q0to5y") %>%
  filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  mutate(
    byvar = tools::toTitleCase(byvar),
    tips = factor(tips, levels = c("0-4", "5-9", "10-14")),
    SurveyYear = as.numeric(substr(source, 3, 6)),
    SurveyYear = ifelse(source == "REACH-Mali", 2025, SurveyYear),
    year_m = case_when(
      tips == "0-4" ~ SurveyYear - 2.5,
      tips == "5-9" ~ SurveyYear - 7.5,
      tips == "10-14" ~ SurveyYear - 12.5
    ),
    qx_scaled = qx * 1000
  ) %>%
  group_by(source) %>%
  filter(tips == "0-4") %>%
  summarise(
    x = max(year_m + 2.5, na.rm = TRUE),
    label = unique(source)
  ) %>%
  ungroup() %>%
  arrange(x) %>%
  mutate(y = 375 - 25 * (row_number() - 1))  # Vertical staggering
survey_years <- df_dhsreg %>%
  bind_rows(ml2024) %>%
  bind_rows(df_reach %>% filter(type == "Region")) %>%
  filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  mutate(SurveyYear = as.numeric(substr(source, 3, 6)),
         SurveyYear = ifelse(source == "REACH-Mali", 2025, SurveyYear)) %>%
  pull(SurveyYear) %>%
  unique() %>%
  sort()

p <- df_dhsreg %>%
  bind_rows(ml2024) %>%
  bind_rows(df_reach %>% filter(type == "Region")) %>%
  filter(agegrp == "q0to5y") %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  filter(!(byvar %in% c("bamako", "gao", "kidal", "timbuktu", "tombouctou", "toumbouctou"))) %>%
  mutate(byvar = tools::toTitleCase(byvar)) %>%
  mutate(byvar = factor(byvar, levels = c("Kayes", "Koulikoro","Mopti","Segou","Sikasso", "National"))) %>%
  mutate(SurveyYear = as.numeric(substr(source, 3, 6)),
         SurveyYear = ifelse(source == "REACH-Mali", 2025, SurveyYear),
         year_m = SurveyYear - 2.5,
         year_m = case_when(
           tips == "5-9" ~ SurveyYear - 7.5,
           tips == "10-14" ~ SurveyYear - 12.5,
           TRUE ~ year_m
         ),
         year_l = year_m - 2.5,
         year_u = year_m + 2.5) %>% 
  ggplot() + 
  geom_vline(xintercept = survey_years, linetype = "dashed", color = "gray50", alpha = 0.5) +
  geom_point(aes(x = year_u, y = qx*1000, color = byvar, shape = tips)) +
  geom_line(aes(x = year_u, y = qx*1000, color = byvar, group = interaction(source, byvar))) +
  geom_label(data = label_data,
             aes(x = x, y = y, label = label),
             alpha = 0.4, fill = "white", color = "black", size = 3, label.size = NA, hjust = 1) +
  labs(x = "", y = "Q(x)", title = "Under-5 mortality by region in DHS and REACH-Mali") +
  scale_shape_discrete(name = "Time prior to survey") +
  scale_color_manual(name = "Region", values = c("#F8766D", "#A3A500", "#00BF7D", "#00B0F6", "#E76BF3", "black")) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave("./gen/mort/figures/trends-reg.png", p, dpi = 300, width = 6, height = 4)


# National trends with label ----------------------------------------------

ml2024 <- data.frame(source = "ML2024DHS",
                     agegrp = c(rep("q0to5y",3), 
                                rep("q0to28d",3),
                                rep("q0to1y",3),
                                rep("q28dto1y",3)
                                ),
                     qx = c(0.087, 0.111, 0.132,
                            0.029, 0.038, 0.037,
                            0.052, 0.061, 0.065,
                            0.022, 0.023, 0.028),
                     tips = rep(c("0-4", "5-9", "10-14"), 4),
                     byvar = "National")
ml1 <- subset(ml2024, agegrp == "q0to28d")
ml2 <- subset(ml2024, agegrp == "q0to5y")
ml_m <- merge(ml1, ml2, by = c("tips", "source", "byvar"))
ml_m$qx <- ml_m$qx.y - ml_m$qx.x
ml_m$agegrp <- "q1to59m"
ml_m <- ml_m[,c("tips", "source", "byvar", "agegrp", "qx")]
ml2024 <- rbind(ml2024, ml_m)

# Labels on top
label_data <- df_dhsnat %>%
  bind_rows(ml2024) %>%
  bind_rows(df_reach %>% filter(type == "All")) %>%
  filter(agegrp == "q0to5y") %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  mutate(SurveyYear = as.numeric(substr(source, 3, 6)),
         SurveyYear = ifelse(source == "REACH-Mali", 2025, SurveyYear),
         year_m = SurveyYear - 2.5,
         year_m = case_when(
           tips == "5-9" ~ SurveyYear - 7.5,
           tips == "10-14" ~ SurveyYear - 12.5,
           TRUE ~ year_m),
         qx_scaled = qx * 1000) %>%
  group_by(source) %>%
  filter(tips == "0-4") %>%
  summarise(
    x = max(year_m + 2.5, na.rm = TRUE),
    label = unique(source)
  ) %>%
  ungroup() %>%
  arrange(x) %>%
  mutate(y = 375 - 25 * (row_number() - 1))  # Vertical staggering
survey_years <- df_dhsnat %>%
  bind_rows(ml2024) %>%
  bind_rows(df_reach %>% filter(type == "All")) %>%
  mutate(SurveyYear = as.numeric(substr(source, 3, 6)),
         SurveyYear = ifelse(source == "REACH-Mali", 2025, SurveyYear)) %>%
  pull(SurveyYear) %>%
  unique() %>%
  sort()

p <- df_dhsnat %>%
  bind_rows(ml2024) %>%
  bind_rows(df_reach %>% filter(type == "All")) %>%
  filter(agegrp == "q0to5y") %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  mutate(SurveyYear = as.numeric(substr(source, 3, 6)),
         SurveyYear = ifelse(source == "REACH-Mali", 2025, SurveyYear),
         year_m = SurveyYear - 2.5,
         year_m = case_when(
           tips == "5-9" ~ SurveyYear - 7.5,
           tips == "10-14" ~ SurveyYear - 12.5,
           TRUE ~ year_m
         ),
         year_l = year_m - 2.5,
         year_u = year_m + 2.5) %>% 
  ggplot() + 
  geom_vline(xintercept = survey_years, linetype = "dashed", color = "gray50", alpha = 0.5) +
  geom_point(aes(x = year_u, y = qx*1000, color = source, shape = tips)) +
  geom_line(aes(x = year_u, y = qx*1000, color = source)) +
  geom_label(data = label_data,
             aes(x = x, y = y, label = label),
             alpha = 0.4, fill = "white", color = "black", size = 3, label.size = NA, hjust = 1) +
  labs(x = "", y = "Q(x)", title = "Under-5 mortality in DHS and REACH-Mali") +
  scale_shape_discrete(name = "Time prior to survey") +
  scale_color_discrete(name = "Source") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
ggsave("./gen/mort/figures/trends-nat.png", p, dpi = 300, width = 6, height = 4)


# Table -------------------------------------------------------------------


df_dhsnat %>%
  bind_rows(ml2024) %>%
  bind_rows(df_reach %>% filter(type == "All")) %>%
  filter(agegrp == "q0to5y" & tips == "0-4") 

df_dhsnat %>%
  bind_rows(ml2024) %>%
  bind_rows(df_reach %>% filter(type == "All")) %>%
  filter(agegrp == "q0to28d" & tips == "0-4") 
