################################################################################
#' @description Compare demogsurv with my calculations
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(kableExtra)
library(ggplot2)
#' Inputs
reach <- readRDS("./gen/mort/output/reach-rates.rds")
demog <- readRDS("./gen/mort/audit/demog-rates.rds")
################################################################################

df_reach <- reach %>%
  filter(cut_time %in% c("0-4", "5-9", "10-14")) %>%
  select(type, cut_time, agegrp, byvar, qx) %>%
  rename(tips = cut_time) %>%
  mutate(source = "hal")

df_demog <- demog %>%
  filter(tips %in% c("0-4", "5-9", "10-14")) %>%
  select(type, tips, agegrp, byvar, est) %>%
  rename(qx = est) %>%
  mutate(source = "demogsurv")

dat <- rbind(df_reach, df_demog)

p1 <- dat %>%
  filter(type == "All") %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14")),
         agegrp = factor(agegrp, levels = c("Neonatal", "1to59m", "Under5"))) %>%
  ggplot() +
  geom_bar(aes(x=agegrp, y = qx, fill = source), position = "dodge", stat = "identity") +
  facet_wrap(~ tips) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("./gen/mort/audit/demog-all.png", p1, dpi = 300, width = 5, height = 3)

p2 <- dat %>%
  filter(type == "Residence") %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14")),
         agegrp = factor(agegrp, levels = c("Neonatal", "1to59m", "Under5"))) %>%
  ggplot() +
  geom_bar(aes(x=agegrp, y = qx, fill = source), position = "dodge", stat = "identity") +
  facet_grid(byvar ~ tips) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("./gen/mort/audit/demog-res.png", p2, dpi = 300, width = 5, height = 6)

p3 <- dat %>%
  filter(type == "Region") %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14")),
         agegrp = factor(agegrp, levels = c("Neonatal", "1to59m", "Under5"))) %>%
  ggplot() +
  geom_bar(aes(x=agegrp, y = qx, fill = source), position = "dodge", stat = "identity") +
  facet_grid(byvar ~ tips) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("./gen/mort/audit/demog-reg.png", p3, dpi = 300, width = 6, height = 8)

