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
reach <- readRDS("./gen/mort/output/reach-rates-ci.rds")
demog <- readRDS("./gen/mort/audit/demog-rates.rds")
dhs <- readRDS("./gen/mort/audit/dhs-rates.rds")
################################################################################

df_reach <- reach %>%
  filter(cut_time %in% c("0-4", "5-9", "10-14")) %>%
  select(type, cut_time, agegrp, byvar, qx, qx_lower, qx_upper) %>%
  rename(tips = cut_time,
         ci_l = qx_lower,
         ci_u = qx_upper) %>%
  mutate(source = "hal")

df_demog <- demog %>%
  filter(tips %in% c("0-4", "5-9", "10-14")) %>%
  select(type, tips, agegrp, byvar, est, ci_l, ci_u) %>%
  rename(qx = est) %>%
  mutate(source = "demogsurv")

df_dhs <- dhs %>%
  filter(tips %in% c("0-4", "5-9", "10-14")) %>%
  select(type, tips, agegrp, byvar, est) %>%
  rename(qx = est) %>%
  mutate(source = "dhs") %>%
  filter(agegrp %in% c("Neonatal", "Under5"))

# compare without ci
dat <- rbind(df_reach %>% select(-c(ci_l, ci_u)), 
             df_demog %>% select(-c(ci_l, ci_u)), 
             df_dhs)
dat$agegrp[dat$agegrp == "Postneonatal"] <- "1to59m"

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


# Compare with CI ---------------------------------------------------------

df_reach <- reach %>%
  filter(cut_time %in% c("0-4", "5-9", "10-14")) %>%
  select(type, cut_time, agegrp, byvar, qx, qx_lower, qx_upper) %>%
  rename(tips = cut_time,
         ci_l = qx_lower,
         ci_u = qx_upper) %>%
  mutate(source = "hal")

df_demog <- demog %>%
  filter(tips %in% c("0-4", "5-9", "10-14")) %>%
  select(type, tips, agegrp, byvar, est, ci_l, ci_u) %>%
  rename(qx = est) %>%
  mutate(source = "demogsurv")

# compare without ci
dat <- rbind(df_reach, df_demog)

dat %>%
  filter(type == "All") %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14")),
         agegrp = factor(agegrp, levels = c("Neonatal", "Postneonatal", "Under5"))) %>%
  ggplot(aes(x=agegrp, y = qx, fill = source)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin= ci_l, ymax = ci_u), position = "dodge") +
  facet_wrap(~ tips) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dat %>%
  filter(type == "Residence") %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14")),
         agegrp = factor(agegrp, levels = c("Neonatal", "Postneonatal", "Under5"))) %>%
  ggplot(aes(x=agegrp, y = qx, fill = source)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin= ci_l, ymax = ci_u), position = "dodge") +
  facet_grid(byvar ~ tips) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dat %>%
  filter(type == "Strata") %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14")),
         agegrp = factor(agegrp, levels = c("Neonatal", "Postneonatal", "Under5"))) %>%
  ggplot(aes(x=agegrp, y = qx, fill = source)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin= ci_l, ymax = ci_u), position = "dodge") +
  facet_grid(byvar ~ tips) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dat %>%
  filter(type == "Area") %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14")),
         agegrp = factor(agegrp, levels = c("Neonatal", "Postneonatal", "Under5"))) %>%
  ggplot(aes(x=agegrp, y = qx, fill = source)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin= ci_l, ymax = ci_u), position = "dodge") +
  facet_grid(byvar ~ tips) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
