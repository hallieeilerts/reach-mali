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

# Visit range -------------------------------------------------------------

# Visit range, multiple views
visrange <- range(dat$qintdate, dat$qvdate1, dat$qvdate2, dat$qvdate3, na.rm = TRUE)
visrange

# Visit dates ---------------------------------------------------

p <- dat %>%
  select(level_1_id, qvdate1, qvdate2, qvdate3) %>% 
  pivot_longer(
    cols = -level_1_id
  ) %>%
  filter(!is.na(value)) %>%
  group_by(value) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=value, y = n), fill = "#0D0887FF", stat = "identity") +
  labs(x = "", title = "Visit dates") + 
  theme_bw() + theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/sec01/figures/visit-dates.png", p, dpi = 300, width = 4, height = 2)


# Visit dates by region ---------------------------------------------------

p <- dat %>%
  select(qlregion, level_1_id, qvdate1, qvdate2, qvdate3) %>% 
  pivot_longer(
    cols = -c(qlregion, level_1_id)
  ) %>%
  filter(!is.na(value)) %>%
  group_by(qlregion, value) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=value, y = n, fill = qlregion), stat = "identity") +
  labs(x = "", title = "Visit dates by region") + 
  theme_bw() + theme(text = element_text(size = 10), title = element_text(size = 8), legend.title = element_blank())
ggsave("./gen/sec01/figures/visit-dates-reg.png", p, dpi = 300, width = 5, height = 2)


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

# Uncategorized
p3 <- dat %>%
  group_by(q111_comb) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = q111_comb, y = n), stat = "identity", fill = "#0D0887FF") + #  fill = barfill
  labs(y = "n", x = "Years", title = "Age of respondents in mortality survey") +
  scale_x_continuous(breaks = c(15, 25, 35, 45)) +
  guides(fill="none") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/sec01/figures/respondent-age.png", p3, dpi = 300, width = 3, height = 2)

# categorized
p3 <- dat %>%  
  group_by(agecat_resp) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = agecat_resp, y = n), stat = "identity", fill = "#0D0887FF") + #  fill = barfill
  labs(y = "n", x = "Years", title = "Age of respondents in mortality survey") +
  #scale_x_continuous(breaks = c(15, 25, 35, 45)) +
  guides(fill="none") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        axis.text.x = element_text(angle = 30, hjust = .75))
#ggsave("./gen/sec01/figures/respondent-age.png", p3, dpi = 300, width = 3, height = 2)

