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
dat <- readRDS("./gen/denomb/output/qhsec01-denomb.rds")
################################################################################

# Age of females in mortality denombrement --------------------------------

p <- dat %>%
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

ggsave("./gen/denomb-mort/figures/n-females.png", p, dpi = 300, width = 4, height = 2)


