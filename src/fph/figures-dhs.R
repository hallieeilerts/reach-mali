################################################################################
#' @description Figures comparing with DHS
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(cowplot)
library(viridis)
#' Inputs
dat <- readRDS("./gen/fph/output/fph-tips.rds")
dhs_dob <- read.csv("./gen/dhs/output/dhs-dob-heaping.csv")
dhs_aad <- read.csv("./gen/dhs/output/dhs-aad-heaping.csv")
dhs_sr <- read.csv("./gen/dhs/output/dhs-sex-ratios.csv")
dhs_flag <- read.csv("./gen/dhs/output/dhs-flags.csv")
################################################################################

viridis(n = 4, option = "C")

# Day of DOB heaping plots ------------------------------------------------

# calculate deviance for reach
# All days in all months
calendar_days <- expand.grid(
  day = 1:31,
  month = 1:12)
# Filter out invalid dates (e.g., April 31st, February 30th)
valid_dates <- calendar_days %>%
  mutate(date = as.Date(paste(2024, month, day, sep = "-"), format = "%Y-%m-%d")) %>%
  filter(!is.na(date))  # Keep only valid dates
# Count how many times each day occurs
day_counts <- valid_dates %>%
  count(day, name = "frequency") %>%
  mutate(per = frequency/sum(frequency))
# count frequency of reporting
# only keep live births as comparing with dhs that are fbh
freqdayb <- dat %>%
  filter(tips != ">15") %>%
  filter(q216 == "Né vivant") %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  mutate(dobd = day(dob)) %>%
  group_by(dobd, tips) %>%
  summarise(n = n()) %>%
  group_by(tips) %>%
  mutate(total = sum(n))
# merge on expected frequencies
reldifdob <- merge(freqdayb, day_counts, by.x = "dobd", by.y = "day")
reldifdob <- reldifdob %>%
  group_by(tips) %>%
  mutate(exp = per*total,
         dev = (n-exp)/exp*100)
length(unique(dhs_dob$SurveyId)) #52

filter_days <- c(1, 15, 31)
p <- dhs_dob %>%
  mutate(dobd = b3_dd)%>%
  filter(dobd %in% filter_days) %>%
  filter(tips != ">15") %>% 
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  ggplot() +
  geom_histogram(aes(x=dev), bins = 30, fill = "#0D0887FF") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(dat = subset(reldifdob, dobd %in% filter_days), 
             aes(xintercept = dev), color = "#DE4968") +
  labs(y = "n", x = "Relative deviation (x100)", title = "Distribution of relative deviation of day of birth from expected") +
  facet_grid(dobd ~ tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/fph/figures/dhs-heaping-dob.png", p, dpi = 300, width = 6, height = 6)
  
# all days
dhs_dob %>%
  mutate(dobd = b3_dd)%>%
  filter(tips != ">15") %>% 
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  ggplot() +
  geom_histogram(aes(x=dev), bins = 30, fill = "#0D0887FF") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(dat = reldifdob, 
             aes(xintercept = dev), color = "#DE4968") +
  labs(y = "n", x = "Relative deviation (x100)", title = "Distribution of relative deviation of day of birth from expected") +
  facet_grid(dobd ~ tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))


# AOD heaping plots ----------------------------------------------------------

# calculate deviance for REACH
reldif7d <- dat %>%
  filter(event == 1 & aadd >= 5 & aadd <= 9  & tips != ">15") %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  group_by(aadd, tips) %>%
  summarise(n = n()) %>%
  group_by(tips) %>%
  mutate(exp = sum(n)/length(5:9),
         dev = (n-exp)/exp*100) %>%
  filter(aadd == 7) 

p1 <- dhs_aad %>%
  filter(variable == "reldif7d" & tips != ">15") %>% 
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  ggplot() +
  geom_histogram(aes(x=dev), bins = 30, fill = "#0D0887FF") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(dat = reldif7d, aes(xintercept = dev), color = "#DE4968") +
  geom_text(data = reldif7d, aes(x = dev, y = 7, label = sprintf("%0.1f",round(dev, 1))), hjust = -.1, color = "#DE4968") +
  labs(y = "n", x = "Relative deviation (x100)", title = "Distribution of relative deviation of observed deaths at 7d from expected") +
  facet_wrap(~tips, nrow = 1) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/fph/figures/dhs-heaping-aod-7d.png", p1, dpi = 300, width = 6, height = 3)


reldif12m <- dat %>%
  filter(event == 1 & aadm >= 10 & aadm <= 14 & tips != ">15") %>%
  mutate(tips = factor(tips, levels =  c("0-4", "5-9", "10-14"))) %>%
  group_by(aadm, tips) %>%
  summarise(n = n()) %>%
  group_by(tips) %>%
  mutate(exp = sum(n)/length(10:14),
         dev = (n-exp)/exp*100) %>%
  filter(aadm == 12) 

p2 <- dhs_aad %>%
  filter(variable == "reldif12m" & tips != ">15") %>% 
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  ggplot() +
  geom_histogram(aes(x=dev), bins = 30, fill = "#0D0887FF") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(dat = reldif12m, aes(xintercept = dev), color = "#DE4968") +
  geom_text(data = reldif12m, aes(x = dev, y = 5.5, label = sprintf("%0.1f",round(dev, 1))), hjust = -.1, color = "#DE4968") +
  labs(y = "n", x = "Relative deviation (x100)", title = "Distribution of relative deviation of observed deaths at 12m from expected") +
  facet_wrap(~tips, nrow = 1) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/fph/figures/dhs-heaping-aod-12m.png", p2, dpi = 300, width = 6, height = 3)

# Sex ratio plots ---------------------------------------------------------

# sex ratio at birth
# live births only
srb <- dat %>%
  filter(tips != ">15") %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  mutate(q223_num = as.numeric(q223)) %>% # live births only
  filter(q223_num == 1) %>%
  mutate(q219 = ifelse(q219 == 1, "Garçon", "Fille")) %>%
  group_by(tips, q219) %>%
  summarise(n = n()) %>%
  pivot_wider(
    id_cols = tips,
    names_from = q219,
    values_from = n
  ) %>%
  mutate(srb = Garçon/Fille,
         dev = (srb-103/100)/(103/100)*100)

# sex ratio of neonatal deaths
# live births only
srd <- dat %>%
  filter(tips != ">15") %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  mutate(q223_num = as.numeric(q223)) %>% # live births only
  filter(q223_num == 1) %>%
  filter(q224 == 2 & aadm >= 0 & aadm < 1) %>% # no longer alive
  mutate(q219 = ifelse(q219 == 1, "Garçon", "Fille")) %>%
  group_by(tips, q219) %>%
  summarise(n = n()) %>%
  pivot_wider(
    id_cols = tips,
    names_from = q219,
    values_from = n
  ) %>%
  mutate(srd = Garçon/Fille,
         dev = (srd-150/100)/(150/100)*100)

p3 <- dhs_sr %>%
  filter(variable == "SRB" & tips != ">15") %>% 
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  ggplot() +
  geom_histogram(aes(x=dev), bins = 30, fill = "#0D0887FF") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(data = srb, aes(xintercept = dev), color = "#DE4968") +
  geom_text(data = srb, aes(x = dev, y = 9, label = sprintf("%0.1f",round(dev, 1))), hjust = -.1, color = "#DE4968") +
  labs(y = "n", x = "Relative deviation (x100)", title = "Distribution of relative deviation of sex ratio at birth from expected") +
  facet_wrap(~tips, nrow = 1) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/fph/figures/dhs-srb-dev.png", p3, dpi = 300, width = 6, height = 3)

p4 <- dhs_sr %>%
  filter(variable == "SRD" & tips != ">15") %>% 
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14"))) %>%
  filter(dev < 200) %>%
  ggplot() +
  geom_histogram(aes(x=dev), bins = 30, fill = "#0D0887FF") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(data = srd, aes(xintercept = dev), color = "#DE4968") +
  geom_text(data = srd, aes(x = dev, y = 11, label = sprintf("%0.1f",round(dev, 1))), hjust = -.1, color = "#DE4968") +
  labs(y = "n", x = "Relative deviation (x100)", title = "Distribution of relative deviation of sex ratio of neonatal deaths from expected") +
  facet_wrap(~tips, nrow = 1) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/fph/figures/dhs-srd-dev.png", p4, dpi = 300, width = 6, height = 3)

