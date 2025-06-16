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
qhhsecover <- read.csv("./data/reach_mortalite_denomb_qhhsecover.csv")
qhsec01 <- read.csv("./data/reach_mortalite_denomb_qhsec01xx.csv")
################################################################################

nrow(qhhsecover) # 10230
nrow(qhsec01) #  119027

dat_hhdid <- qhhsecover %>%
  select(level_1_id, qhhi1, qhhli1, qhhi2, qhhli2, qhhdistrict, qhhli3, qhhi3, idse, qhhi4, qhhi5, hhi7) %>%
  rename(hhd_id      = level_1_id,
         region_code = qhhli1,
         region = qhhli1,
         cercle_code = qhhi2, 
         cercle      = qhhli2,
         commune_code = qhhi3,
         commune      = qhhli3,
         se           = idse,
         village      = qhhi4,
         health_area  = qhhi5,
         team         = hhi7
  )

dat_hhdrost <- qhsec01 %>%
  select(level_1_id, qhh01x, qhh03x, qhh04x, qhh05x) %>%
  rename(hhd_id    = level_1_id,
         member_id = qhh01x,
         sex = qhh03x,
         age = qhh04x,
         hhd_hd_rel = qhh05x) %>%
  mutate(sex = case_when(
    sex == 1 ~ "Male",
    sex == 2 ~ "Female",
    TRUE ~ NA 
  ),
  hhd_hd_rel = case_when(
    hhd_hd_rel == 1 ~ "head",
    hhd_hd_rel == 2 ~ "wife",
    hhd_hd_rel == 3 ~ "child",
    hhd_hd_rel == 4 ~ "son or daughter in-law",
    hhd_hd_rel == 5 ~ "grandchild",
    hhd_hd_rel == 6 ~ "grandparent",
    hhd_hd_rel == 7 ~ "step-parent",
    hhd_hd_rel == 8 ~ "sibling",
    hhd_hd_rel == 9 ~ "other",
    hhd_hd_rel == 10 ~ "adopted",
    hhd_hd_rel == 11 ~ "orphan",
    hhd_hd_rel == 98 ~ "unknown",
    TRUE ~ NA
  ))

dat <- merge(dat_hhdid, dat_hhdrost, by="hhd_id", all.y=T)



p <- dat %>%
  filter(sex == "Female") %>%
  group_by(age) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = age, y = n), stat = "identity", fill = "#0D0887FF") +
  geom_vline(aes(xintercept = 14.5), color = "#DE4968", linetype = "dashed", linewidth = .5) + 
  geom_vline(aes(xintercept = 49.5), color = "#DE4968", linetype = "dashed", linewidth = .5) + 
  labs(title = "Age of women in mortality dénombrement",x = "Age", y = "n") +
  theme_bw()  +
  theme(text = element_text(size = 10), title = element_text(size = 8))

ggsave("./gen/denomb-mort/figures/n-females.png", p, dpi = 300, width = 4, height = 2)


