################################################################################
#' @description
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
#' Inputs
denomb <- readRDS("./gen/denomb/output/qhsec01-denomb.rds")
sec01 <- readRDS("./gen/sec01/output/qsecover-qwsec01.rds")
################################################################################



# Population pyramid ------------------------------------------------------

denomb %>%
  filter(!is.na(age) & !is.na(sex)) %>%
  group_by(age, sex) %>%
  summarise(n = n()) %>%
  mutate(n = ifelse(sex == "Male", -n, n)) %>%
  ggplot() +
  geom_bar(aes(x=age, y = n, fill = sex), stat = "identity") +
  coord_flip() +
  scale_y_continuous(breaks = seq(-3000, 3000, 1000),
                     labels = c(-seq(-3000, 0, 1000), seq(1000, 3000, 1000))) +
  labs(title = "Individuals in mortality denombrement", x = "age",y = "") +
  theme_bw()  +
  theme(text = element_text(size = 8))


# Population pyramid age cat ----------------------------------------------

denomb %>%
  filter(!is.na(age) & !is.na(sex)) %>%
  mutate(agecat = cut(age, breaks = c(-Inf, seq(5, 80, 5), Inf),
                      labels = c(seq(0, 75, 5), "80+"))) %>%
  group_by(sex, agecat) %>%
  summarise(n = n()) %>%
  mutate(n = ifelse(sex == "Male", -n, n)) %>% 
  ggplot() +
  geom_bar(aes(x=agecat, y = n, fill = sex), stat = "identity") +
  annotate(geom="text", y=-7500, x=12, label="Male", color="#0D0887FF") +
  annotate(geom="text", y=7500, x=12, label="Female", color="#D8576BFF") +
  coord_flip() +
  labs(title = "Individuals in mortality denombrement", x = "Age",y = "") +
  scale_fill_manual(values = c("#D8576BFF", "#0D0887FF")) +
  theme_bw()  +
  theme(text = element_text(size = 8), legend.position = "none")


# Combined denombrement and mort survey pop pyramid -----------------------

all_ages <- sort(unique(denomb$age))

# Mortality survey
mort_female <- sec01 %>%
  group_by(q111_comb) %>%
  summarise(n = n(), .groups = "drop") %>%
  rename(age = q111_comb) %>%
  mutate(sex = "Female",
         source = "Mortality survey")

# Expand to all sex/age combinations so that bar plot aligns
dat1 <- tidyr::expand_grid(
  age = all_ages,
  sex = c("Female", "Male")
) %>%
  left_join(mort_female, by = c("age", "sex")) %>%
  mutate(n = replace_na(n, 0),
         source = "Mortality survey",
         n = ifelse(sex == "Male", -n, n))

# Census data
dat2 <- denomb %>%
  filter(!is.na(age) & !is.na(sex)) %>%
  group_by(age, sex) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(n = ifelse(sex == "Male", -n, n),
         source = "Census")

# Combine and plot
p <- bind_rows(dat1, dat2) %>%
  ggplot() +
  geom_bar(aes(x = age, y = n, fill = source), stat = "identity",
    position = position_dodge(preserve = "single"), width = 2) +
  geom_hline(aes(yintercept=1)) +
  annotate(geom="text", y=-1500, x=55, label="Male") +
  annotate(geom="text", y=1500, x=55, label="Female") +
  coord_flip() +
  scale_y_continuous(breaks = seq(-3000, 3000, 1000),
    labels = c(-seq(-3000, 0, 1000), seq(1000, 3000, 1000))) +
  scale_fill_manual(values = c("#0D0887FF", "#D8576BFF"), name = "Source") +
  labs(title = "Age and sex distribution in census and mortality survey", x = "Age", y = "n") +
  theme_bw() +
  theme(text = element_text(size = 8))
ggsave("./gen/denomb/figures/pyramid-combo-singleyear.png", p, dpi = 300, width = 5, height = 4)


# Age of females in mortality denombrement --------------------------------

p <- denomb %>%
  filter(sex == "Female") %>%
  group_by(age) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = age, y = n), stat = "identity", fill = "#0D0887FF") +
  geom_vline(aes(xintercept = 14.5), color = "#DE4968", linetype = "dashed", linewidth = .5) + 
  geom_vline(aes(xintercept = 49.5), color = "#DE4968", linetype = "dashed", linewidth = .5) + 
  labs(title = "Age of women in census prior to mortality survey",x = "Age", y = "n") +
  theme_bw()  +
  theme(text = element_text(size = 10), title = element_text(size = 8))

ggsave("./gen/denomb/figures/n-females.png", p, dpi = 300, width = 4, height = 2)


