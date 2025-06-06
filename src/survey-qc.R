################################################################################
#' @description
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
qhhsecover <- read.csv("./data/reach_mortalite_denomb_qhhsecover.csv")
qhsec01 <- read.csv("./data/reach_mortalite_denomb_qhsec01xx.csv")
################################################################################

# Here's some help with variable names since they're not in the DD yet. 
# to get region information I merge the reach_mortalite_denomb_qhhsecover dataset (only the region info and the level_1_id linking variable) to the reach_mortalite_denomb_qhsec01xx dataset 

names(qhhsecover)
#View(qhhsecover)
names(qhsec01)
#View(qhsec01)

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
    sex == 1 ~ "male",
    sex == 2 ~ "female",
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


write.csv(dat, "./gen/hhd-surveyed.csv", row.names = FALSE)

# We want a summary, grouped by region and then by SE
#	Average number of total persons in a household by SE
#	Average total number of men in household (Age >=15) by SE
#	Average total number of women in household (Age >=15) by SE
#	Average number of women age 15-49 in the household
#	Average age of women age 15-49 in household

# households per region
dat %>%
  group_by(region) %>%
  distinct(hhd_id) %>%
  summarise(n_hhd = n())
# households per se
dat %>%
  group_by(region, se) %>%
  distinct(hhd_id) %>%
  summarise(n_hhd = n())  %>%
  ggplot() +
  geom_histogram(aes(n_hhd)) +
  facet_wrap(~region, nrow = 1) +
  labs(title = "Number of households per SE",x = "n",y = "count")

# average total number of people per household by se
dat %>%
  select(se, region, hhd_id, ind_id, name) %>%
  group_by(region, se, hhd_id) %>%
  summarise(n_ind = n()) %>%
  group_by(se, region) %>%
  summarise(avg_n_ind = mean(n_ind)) %>%
  ggplot() +
  geom_histogram(aes(avg_n_ind)) +
  facet_wrap(~region, nrow = 1) +
  labs(title = "Average individuals per households per SE",x = "n",y = "count") 

#	Average total number of men in household (Age >=15) by SE
dat %>%
  filter(age_y >= 15 & sex == "male") %>%
  select(se, region, hhd_id, ind_id, name) %>%
  group_by(region, se, hhd_id) %>%
  summarise(n_ind = n()) %>%
  group_by(se, region) %>%
  summarise(avg_n_ind = mean(n_ind)) %>%
  ggplot() +
  geom_histogram(aes(avg_n_ind)) +
  facet_wrap(~region, nrow = 1) +
  labs(title = "Average men >=15y per households per SE",x = "n",y = "count")

#	Average total number of women in household (Age >=15) by SE
dat %>%
  filter(age_y >= 15 & sex == "female") %>%
  select(se, region, hhd_id, ind_id, name) %>%
  group_by(region, se, hhd_id) %>%
  summarise(n_ind = n()) %>%
  group_by(se, region) %>%
  summarise(avg_n_ind = mean(n_ind)) %>%
  ggplot() +
  geom_histogram(aes(avg_n_ind)) +
  facet_wrap(~region, nrow = 1) +
  labs(title = "Average number of women >=15y per household per SE",x = "n",y = "count")
  
# plots with theme_bw are good


# Average number of women age 15-49 in the household per SE, grouped by region
dat %>%
  filter(age_y >= 15 & age_y <= 49 & sex == "female") %>%
  select(se, region, hhd_id, ind_id, name) %>%
  group_by(region, se, hhd_id) %>%
  summarise(n_ind = n()) %>%
  group_by(se, region) %>%
  summarise(avg_n_ind = mean(n_ind)) %>%
  ggplot(aes(x = region, y = avg_n_ind)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, alpha = 0.5, color = "blue", size = 1.5) +
  labs(title = "Average number of women 15-49y per household",x = "n",y = "count") +
  theme_bw()

# Number of women age 15-49 in the household, grouped by region
mean_df <- dat %>%
  filter(age_y >= 15 & age_y <= 49 & sex == "female") %>%
  select(se, region, hhd_id, ind_id, name) %>%
  group_by(region, se, hhd_id) %>%
  summarise(n_ind = n()) %>%
  group_by(region) %>%
  summarise(mean_value = mean(n_ind))
dat %>%
  filter(age_y >= 15 & age_y <= 49 & sex == "female") %>%
  select(se, region, hhd_id, ind_id, name) %>%
  group_by(region, se, hhd_id) %>%
  summarise(n_ind = n()) %>%
  ggplot() +
  geom_histogram(aes(n_ind)) +
  geom_vline(data = mean_df, aes(xintercept = mean_value), 
             color = "red", linetype = "dashed", size = 1) + 
  geom_text(data = mean_df, aes(x = mean_value, y = 500, label = round(mean_value, 2)),
            color = "red", hjust = -0.25) +
  facet_wrap(~region, nrow = 1) +
  labs(title = "Women 15-49y per household",x = "n",y = "count") +
  theme_bw()


#	Average age of women age 15-49 per household, grouped by region
mean_df <- dat %>%
  filter(age_y >= 15 & age_y <= 49 & sex == "female") %>%
  select(se, region, hhd_id, ind_id, name, age_y) %>%
  group_by(region, se, hhd_id) %>%
  summarise(mean_value = mean(age_y)) %>%
  group_by(region) %>%
  summarise(mean_value = mean(mean_value))
dat %>%
  filter(age_y >= 15 & age_y <= 49 & sex == "female") %>%
  select(se, region, hhd_id, ind_id, name, age_y) %>%
  group_by(region, se, hhd_id) %>%
  summarise(avg_age = mean(age_y)) %>%
  ggplot() +
  geom_histogram(aes(avg_age)) +
  geom_vline(data = mean_df, aes(xintercept = mean_value), 
             color = "red", linetype = "dashed", size = 1) + 
  geom_text(data = mean_df, aes(x = mean_value, y = 200, label = round(mean_value, 2)),
            color = "red", hjust = -0.25) +
  facet_wrap(~region, nrow = 1) +
  labs(title = "Average age of women 15-49y per household",x = "n",y = "count") +
  theme_bw()







