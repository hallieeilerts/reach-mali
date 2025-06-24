################################################################################
#' @description 
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(purrr)
library(survival)
library(readxl)
#' Inputs
source("./src/utils.R")
dat <- readRDS("./gen/fph/output/fph-tips.rds")
# sampling weights
wt1 <- read_excel("./data/instat/Poids_Enquete_Base_Mortalite.xlsx", sheet = "Poidsformule")
wt2 <- read_excel("./data/instat/Poids_Enquete_Base_Mortalite.xlsx", sheet = "Poidsvf")
################################################################################

wt1 <- wt1[, c("GRAPPE", "CStrate")]
wt2 <- wt2[, c("GRAPPE", "Poids normalisé des femmes de 15-49 ans")]
wt <- merge(wt1, wt2, by = "GRAPPE")
names(wt) <- c("grappe", "strate", "wt")
wt$strate <- as.numeric(wt$strate)
wt$grappe <- as.numeric(wt$grappe)

# Merge on weights --------------------------------------------------------

nrow(dat)
dat <- merge(dat, wt, by = "grappe")
nrow(dat)
# all have weights

#  Set exposure -----------------------------------------------------------

# Only keep live births
dat <- subset(dat, q223_aug == 1)

# There are no NA's event
table(dat$event, useNA = "always")
# When event == 1, aad_unit and aad_value always reported
nrow(subset(dat, event == 1 & (is.na(aad_unit) | is.na(aad_value))))

# Exposure when date of birth = date of survey 
dat$select <- dat$event == 0 & dat$dob_dec == dat$v008_dec
set.seed(1)
dat$expo <- NA
dat$expo[dat$select == T] <-  0

# Exposure when date of birth != date of survey 
dat$select <- dat$event == 0 & dat$dob_dec != dat$v008_dec
dat$expo[dat$select == T] <-  dat$v008_dec[dat$select == T] - dat$dob_dec[dat$select == T] 

# Exposure for non-surviving children

# set a random number between 0 and 1 for uniform imputation of days
set.seed(777)
dat$random <- NA
dat$random[dat$event == 1] <- runif(nrow(dat[dat$event == 1,]), min = 0,  max = 1)

# Turn discreate age at death values into continuous values to reduce effects of digit preference
# adds less than 1 day to ages at death reported in days, adds somewhere within 1 month to month, places year death within the same year
# Age at death in person-years with imputation for random days
dat$expo[dat$event == 1 & dat$aad_unit == 1] <- (as.numeric(dat$aad_value[dat$event == 1 & dat$aad_unit == 1]) + dat$random[dat$event == 1 & dat$aad_unit == 1])/365.25   
dat$expo[dat$event == 1 & dat$aad_unit == 2] <- (as.numeric(dat$aad_value[dat$event == 1 & dat$aad_unit == 2]) + dat$random[dat$event == 1 & dat$aad_unit == 2])/12          
dat$expo[dat$event == 1 & dat$aad_unit == 3] <- (as.numeric(dat$aad_value[dat$event == 1 & dat$aad_unit == 3]) + dat$random[dat$event == 1 & dat$aad_unit == 3])

gapu5m_age <- c(7,14, 21, 28,
                seq(60.8750, 365.25,  30.4375),
                seq(365.25 + 365.25/4 , 365.25*2,  365.25/4),
                seq(365.25*3 , 365.25*5,  365.25))

reach_age <- c(28, 365.25*5)

# exposure is never missing
nrow(subset(dat, is.na(expo))) # 0

# Entire pop --------------------------------------------------------------

res <- fn_jackknife_mort(dat, ages = gapu5m_age, tips = c(0, 5, 10, 15),
                         cluster_var = "grappe", strata_var = "strate",
                         return_replicates = TRUE, verbose = TRUE)
gapu5m_rates_all_tips <- res$estimates
gapu5m_plot_all_tips <- res$estimates %>%
  group_split(cut_time) %>% 
  map(fn_addrows) %>% 
  bind_rows() %>% 
  arrange(cut_time, age_y)

# REACH age groups
reach1 <- fn_jackknife_mort(dat, ages = c(365.25*5), tips = c(0, 5, 10, 15),
                      cluster_var = "grappe", strata_var = "strate",
                      return_replicates = TRUE, verbose = TRUE)
reach2 <- fn_jackknife_mort(dat, ages = c(28, 365.25*5), tips = c(0, 5, 10, 15),
                      cluster_var = "grappe", strata_var = "strate",
                      return_replicates = TRUE, verbose = TRUE)
reach_rates_all_tips <- rbind(reach1$estimates, reach2$estimates)
reach_rates_all_tips <- reach_rates_all_tips[order(reach_rates_all_tips$byvar, 
                                                   reach_rates_all_tips$cut_time,
                                                   reach_rates_all_tips$age_d,
                                                   reach_rates_all_tips$age_d_up),]

# Residence ---------------------------------------------------------------

# gapu5m age groups
groups <- dat %>% group_by(qtype) %>% group_split()
qtypes <- dat %>% group_by(qtype) %>% group_keys() %>% pull(qtype)
# Apply the function by qtype
results_byvar <- map(groups, ~ fn_jackknife_mort(.x, ages = gapu5m_age, tips = c(0, 5, 10, 15),
                                                 cluster_var = "grappe", strata_var = "strate",
                                                 return_replicates = TRUE, verbose = TRUE))
names(results_byvar) <- qtypes
# Extract and label
gapu5m_rates_res_tips <- imap_dfr(results_byvar, ~ mutate(.x$estimates, byvar = .y))
gapu5m_plot_res_tips <- imap_dfr(results_byvar, ~ {
  .x$estimates %>%
    group_split(cut_time) %>%
    map(fn_addrows) %>%
    bind_rows() %>%
    arrange(cut_time, age_y) %>%
    mutate(byvar = .y)
})

# REACH age groups
reach1 <- map(groups, ~ fn_jackknife_mort(.x, ages = c(365.25*5), tips = c(0, 5, 10, 15),
                                                 cluster_var = "grappe", strata_var = "strate",
                                                 return_replicates = TRUE, verbose = TRUE))
names(reach1) <- qtypes
reach1 <- imap_dfr(reach1, ~ mutate(.x$estimates, byvar = .y))
reach2 <- map(groups, ~ fn_jackknife_mort(.x, ages = c(28, 365.25*5), tips = c(0, 5, 10, 15),
                                          cluster_var = "grappe", strata_var = "strate",
                                          return_replicates = TRUE, verbose = TRUE))
names(reach2) <- qtypes
reach2 <- imap_dfr(reach2, ~ mutate(.x$estimates, byvar = .y))
reach_rates_res_tips <- rbind(reach1, reach2)
reach_rates_res_tips <- reach_rates_res_tips[order(reach_rates_res_tips$byvar, 
                                                   reach_rates_res_tips$cut_time,
                                                   reach_rates_res_tips$age_d,
                                                   reach_rates_res_tips$age_d_up),]

# Strata ------------------------------------------------------------------

groups <- dat %>% group_by(strate) %>% group_split()
qtypes <- dat %>% group_by(strate) %>% group_keys() %>% pull(strate)
# Apply the function by qtype
results_byvar <- map(groups, ~ fn_jackknife_mort(.x, ages = gapu5m_age, tips = c(0, 5, 10, 15),
                                                 cluster_var = "grappe", strata_var = "strate",
                                                 return_replicates = TRUE, verbose = TRUE))
names(results_byvar) <- qtypes
# Extract and label
gapu5m_rates_strata_tips <- imap_dfr(results_byvar, ~ mutate(.x$estimates, byvar = .y))
gapu5m_plot_strata_tips <- imap_dfr(results_byvar, ~ {
  .x$estimates %>%
    group_split(cut_time) %>%
    map(fn_addrows) %>%
    bind_rows() %>%
    arrange(cut_time, age_y) %>%
    mutate(byvar = .y)
})

# REACH age groups
reach1 <- map(groups, ~ fn_jackknife_mort(.x, ages = c(365.25*5), tips = c(0, 5, 10, 15),
                                          cluster_var = "grappe", strata_var = "strate",
                                          return_replicates = TRUE, verbose = TRUE))
names(reach1) <- qtypes
reach1 <- imap_dfr(reach1, ~ mutate(.x$estimates, byvar = .y))
reach2 <- map(groups, ~ fn_jackknife_mort(.x, ages = c(28, 365.25*5), tips = c(0, 5, 10, 15),
                                          cluster_var = "grappe", strata_var = "strate",
                                          return_replicates = TRUE, verbose = TRUE))
names(reach2) <- qtypes
reach2 <- imap_dfr(reach2, ~ mutate(.x$estimates, byvar = .y))
reach_rates_strata_tips <- rbind(reach1, reach2)
reach_rates_strata_tips <- reach_rates_strata_tips[order(reach_rates_strata_tips$byvar, 
                                                   reach_rates_strata_tips$cut_time,
                                                   reach_rates_strata_tips$age_d,
                                                   reach_rates_strata_tips$age_d_up),]


# Combine -----------------------------------------------------------------

# Combine all gapu5m rates
gapu5m_rates_all_tips$byvar <- "All"
gapu5m_rates_all_tips$type <- "All"
gapu5m_rates_res_tips$type <- "Residence"
gapu5m_rates_strata_tips$type <- "Strata"
gapu5m_rates <- rbind(gapu5m_rates_all_tips, gapu5m_rates_res_tips, gapu5m_rates_strata_tips)
gapu5m_rates <- gapu5m_rates %>%
  arrange(byvar, cut_time, age_d)

# Combine all gapu5m plot rates
gapu5m_plot_all_tips$byvar <- "All"
gapu5m_plot_all_tips$type <- "All"
gapu5m_plot_res_tips$type <- "Residence"
gapu5m_plot_strata_tips$type <- "Strata"
gapu5m_plot <- rbind(gapu5m_plot_all_tips, gapu5m_plot_res_tips, gapu5m_plot_strata_tips)
gapu5m_plot <- gapu5m_plot %>%
  arrange(byvar, cut_time, age_d)

# Combine all reach rates
reach_rates_all_tips$byvar <- "All"
reach_rates_all_tips$type <- "All"
reach_rates_res_tips$type <- "Residence"
reach_rates_strata_tips$type <- "Strata"
reach_rates <- rbind(reach_rates_all_tips, reach_rates_res_tips, reach_rates_strata_tips)
reach_rates$agegrp <- "Under5"
reach_rates$agegrp[reach_rates$age_y == 0 & reach_rates$age_y_up != 5] <- "Neonatal"
reach_rates$agegrp[reach_rates$age_d == 28] <- "1to59m"
reach_rates <- reach_rates %>%
  arrange(byvar, cut_time, age_y, age_y_up)

# Save output(s) ----------------------------------------------------------

saveRDS(gapu5m_rates, "./gen/mort/output/gapu5m-rates-ci.rds")
saveRDS(gapu5m_plot, "./gen/mort/output/gapu5m-for-plots-ci.rds")
saveRDS(reach_rates, "./gen/mort/output/reach-rates-ci.rds")


gapu5m_plot %>%
  filter(byvar == "All" & cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14"))) %>%
  ggplot() +
  geom_step(aes(x = age_y, y = mx*1000), color = "#0D0887FF") +
  geom_stepribbon(aes(x = age_y, ymin = mx_lower*1000, ymax = mx_upper*1000), 
                  fill = "#0D0887FF", alpha = 0.2) +
  labs(x = "", y = "mx (log scale)") +
  scale_y_log10() +
  facet_wrap(~tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        plot.margin = margin(0, 0, 0, 0),
        legend.position = "none",
        legend.box.background = element_blank(),
        legend.background = element_blank())

gapu5m_plot %>%
  filter(byvar == "All" & cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14"))) %>%
  ggplot() +
  geom_step(aes(x = age_y, y = Qx*1000), color = "#0D0887FF") +
  geom_stepribbon(aes(x = age_y, ymin = Qx_lower*1000, ymax = Qx_upper*1000), 
                  fill = "#0D0887FF", alpha = 0.2) +
  labs(x = "Age (years)", y = "Q(x)") +
  facet_wrap(~tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        plot.margin = margin(0, 0, 0, 0),
        legend.position = "none",
        legend.box.background = element_blank(),
        legend.background = element_blank())


gapu5m_plot %>%
  filter(type == "Residence" & cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14"))) %>% 
  ggplot() +
  geom_step(aes(x = age_y, y = Qx*1000, color = byvar)) +
  geom_stepribbon(aes(x = age_y, ymin = Qx_lower*1000, ymax = Qx_upper*1000, fill = byvar), alpha = 0.2) +
  labs(x = "Age (years)", y = "Q(x)") +
  facet_wrap(~tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank())
gapu5m_plot %>%
  filter(type == "Strata" & cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14"))) %>% 
  ggplot() +
  geom_step(aes(x = age_y, y = Qx*1000, color = byvar)) +
  geom_stepribbon(aes(x = age_y, ymin = Qx_lower*1000, ymax = Qx_upper*1000, fill = byvar), alpha = 0.2) +
  labs(x = "Age (years)", y = "Q(x)") +
  facet_wrap(~tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank())

