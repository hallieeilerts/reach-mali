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
#' Inputs
source("./src/utils.R")
dat <- readRDS("./gen/fph/output/fph-tips.rds")
################################################################################

#  Set exposure -----------------------------------------------------------

# Exposure when date of birth = date of survey 
dat$select <- dat$event == FALSE & dat$dob_dec == dat$v008_dec
set.seed(1)
dat$expo <- NA
dat$expo[dat$select == T] <-  0

# Exposure when date of birth != date of survey 
dat$select <- dat$event == FALSE & dat$dob_dec != dat$v008_dec
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


# Entire pop --------------------------------------------------------------

# Tips

res <- fn_calcmort(dat, ages = gapu5m_age, tips = c(0, 5, 10, 15))
gapu5m_rates_all_tips <- res$rates
gapu5m_plot_all_tips <- res$plot


# Period

res <- fn_calcmort(dat, ages = gapu5m_age, period = c(2010, 2015, 2020, 2025))
gapu5m_rates_all_period <- res$rates
gapu5m_plot_all_period <- res$plot

# REACH age groups
reach1 <- fn_calcmort(dat, ages = c(365.25*5), tips = c(0, 5, 10, 15))
reach2 <- fn_calcmort(dat, ages = c(28, 365.25*5), tips = c(0, 5, 10, 15))
reach_rates_all_tips <- rbind(reach1$rates, reach2$rates)
reach1 <- fn_calcmort(dat, ages = c(365.25*5), period = c(2010, 2015, 2020, 2025))
reach2 <- fn_calcmort(dat, ages = c(28, 365.25*5), period = c(2010, 2015, 2020, 2025))
reach_rates_all_period <- rbind(reach1$rates, reach2$rates)

# Residence ---------------------------------------------------------------

# Tips

groups <- dat %>% group_by(qtype) %>% group_split()
qtypes <- dat %>% group_by(qtype) %>% group_keys() %>% pull(qtype)
# Apply the function by qtype
results_byvar <- map(groups, ~ fn_calcmort(.x, ages = gapu5m_age, tips = c(0, 5, 10, 15)))
names(results_byvar) <- qtypes
# Extract and label
gapu5m_rates_res_tips <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
gapu5m_plot_res_tips <- imap_dfr(results_byvar, ~ mutate(.x$plot, byvar = .y))

# Period

# # Apply the function by qtype
# results_byvar <- dat %>%
#   group_split(qtype) %>%
#   set_names(unique(dat$qtype)) %>%  # name each group by its qtype
#   map(~ fn_calcmort(.x, ages = gapu5m_age, period = c(2010, 2015, 2020, 2025)))
# # Extract and label
# gapu5m_rates_res_period <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
# gapu5m_plot_res_period <- imap_dfr(results_byvar, ~ mutate(.x$plot, byvar = .y))
results_byvar <- map(groups, ~ fn_calcmort(.x, ages = gapu5m_age, c(2010, 2015, 2020, 2025)))
names(results_byvar) <- qtypes
gapu5m_rates_res_period <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
gapu5m_plot_res_period <- imap_dfr(results_byvar, ~ mutate(.x$plot, byvar = .y))

gapu5m_plot_res_period %>%
  ggplot() +
  geom_step(aes(x = age_y, y = Qx*1000, color = byvar)) +
  facet_wrap(~cut_time)
gapu5m_plot_res_period %>%
  ggplot() +
  geom_step(aes(x = age_y, y = mx, color = byvar)) +
  scale_y_log10() +
  facet_wrap(~cut_time)

# REACH

# results_byvar <- dat %>%
#   group_split(qtype) %>%
#   set_names(unique(dat$qtype)) %>%  # name each group by its qtype
#   map(~ fn_calcmort(.x, ages = c(365.25*5), tips = c(0, 5, 10, 15)))
# reach1 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
# results_byvar <- dat %>%
#   group_split(qtype) %>%
#   set_names(unique(dat$qtype)) %>%  # name each group by its qtype
#   map(~ fn_calcmort(.x, ages = c(28, 365.25*5), tips = c(0, 5, 10, 15)))
# reach2 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
# reach_rates_res_tips <- rbind(reach1, reach2)
results_byvar <- map(groups, ~ fn_calcmort(.x, ages = c(365.25*5), tips = c(0, 5, 10, 15)))
names(results_byvar) <- qtypes
reach1 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
results_byvar <- map(groups, ~ fn_calcmort(.x, ages = c(28, 365.25*5), tips = c(0, 5, 10, 15)))
names(results_byvar) <- qtypes
reach2 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
reach_rates_res_tips <- rbind(reach1, reach2)

# results_byvar <- dat %>%
#   group_split(qtype) %>%
#   set_names(unique(dat$qtype)) %>%  # name each group by its qtype
#   map(~ fn_calcmort(.x, ages = c(365.25*5), period = c(2010, 2015, 2020, 2025)))
# reach1 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
# results_byvar <- dat %>%
#   group_split(qtype) %>%
#   set_names(unique(dat$qtype)) %>%  # name each group by its qtype
#   map(~ fn_calcmort(.x, ages = c(28, 365.25*5), period = c(2010, 2015, 2020, 2025)))
# reach2 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
# reach_rates_res_period <- rbind(reach1, reach2)
results_byvar <- map(groups, ~ fn_calcmort(.x, ages = c(365.25*5), period = c(2010, 2015, 2020, 2025)))
names(results_byvar) <- qtypes
reach1 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
results_byvar <- map(groups, ~ fn_calcmort(.x, ages = c(28, 365.25*5), period = c(2010, 2015, 2020, 2025)))
names(results_byvar) <- qtypes
reach2 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
reach_rates_res_period <- rbind(reach1, reach2)

# Region ------------------------------------------------------------------

# Tips

# results_byvar <- dat %>%
#   group_split(qlregion) %>%
#   set_names(unique(dat$qlregion)) %>%  # name each group by its qtype
#   map(~ fn_calcmort(.x, ages = gapu5m_age, tips = c(0, 5, 10, 15)))
# # Extract and label
# gapu5m_rates_reg_tips <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
# gapu5m_plot_reg_tips  <- imap_dfr(results_byvar, ~ mutate(.x$plot, byvar = .y))
groups <- dat %>% group_by(qlregion) %>% group_split()
qlregions <- dat %>% group_by(qlregion) %>% group_keys() %>% pull(qlregion)
# Apply the function by qlregion
results_byvar <- map(groups, ~ fn_calcmort(.x, ages = gapu5m_age, tips = c(0, 5, 10, 15)))
names(results_byvar) <- qlregions
# Extract and label
gapu5m_rates_reg_tips <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
gapu5m_plot_reg_tips <- imap_dfr(results_byvar, ~ mutate(.x$plot, byvar = .y))


# Period

# results_byvar <- dat %>%
#   group_split(qlregion) %>%
#   set_names(unique(dat$qlregion)) %>%  # name each group by its qtype
#   map(~ fn_calcmort(.x, ages = gapu5m_age, period = c(2010, 2015, 2020, 2025)))
# # Extract and label
# gapu5m_rates_reg_period <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
# gapu5m_plot_reg_period  <- imap_dfr(results_byvar, ~ mutate(.x$plot, byvar = .y))
results_byvar <- map(groups, ~ fn_calcmort(.x, ages = gapu5m_age, c(2010, 2015, 2020, 2025)))
names(results_byvar) <- qlregions
gapu5m_rates_reg_period <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
gapu5m_plot_reg_period <- imap_dfr(results_byvar, ~ mutate(.x$plot, byvar = .y))

gapu5m_plot_reg_period %>%
  ggplot() +
  geom_step(aes(x = age_y, y = Qx*1000, color = byvar)) +
  facet_wrap(~cut_time)

# REACH

# results_byvar <- dat %>%
#   group_split(qlregion) %>%
#   set_names(unique(dat$qlregion)) %>%  # name each group by its qlregion
#   map(~ fn_calcmort(.x, ages = c(365.25*5), tips = c(0, 5, 10, 15)))
# reach1 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
# results_byvar <- dat %>%
#   group_split(qlregion) %>%
#   set_names(unique(dat$qlregion)) %>%  # name each group by its qlregion
#   map(~ fn_calcmort(.x, ages = c(28, 365.25*5), tips = c(0, 5, 10, 15)))
# reach2 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
# reach_rates_reg_tips <- rbind(reach1, reach2)
results_byvar <- map(groups, ~ fn_calcmort(.x, ages = c(365.25*5), tips = c(0, 5, 10, 15)))
names(results_byvar) <- qlregions
reach1 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
results_byvar <- map(groups, ~ fn_calcmort(.x, ages = c(28, 365.25*5), tips = c(0, 5, 10, 15)))
names(results_byvar) <- qlregions
reach2 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
reach_rates_reg_tips <- rbind(reach1, reach2)

# results_byvar <- dat %>%
#   group_split(qlregion) %>%
#   set_names(unique(dat$qlregion)) %>%  # name each group by its qlregion
#   map(~ fn_calcmort(.x, ages = c(365.25*5), period = c(2010, 2015, 2020, 2025)))
# reach1 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
# results_byvar <- dat %>%
#   group_split(qlregion) %>%
#   set_names(unique(dat$qlregion)) %>%  # name each group by its qlregion
#   map(~ fn_calcmort(.x, ages = c(28, 365.25*5), period = c(2010, 2015, 2020, 2025)))
# reach2 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
# reach_rates_reg_period <- rbind(reach1, reach2)
results_byvar <- map(groups, ~ fn_calcmort(.x, ages = c(365.25*5), period = c(2010, 2015, 2020, 2025)))
names(results_byvar) <- qlregions
reach1 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
results_byvar <- map(groups, ~ fn_calcmort(.x, ages = c(28, 365.25*5), period = c(2010, 2015, 2020, 2025)))
names(results_byvar) <- qlregions
reach2 <- imap_dfr(results_byvar, ~ mutate(.x$rates, byvar = .y))
reach_rates_reg_period <- rbind(reach1, reach2)

# Combine -----------------------------------------------------------------

# Combine all gapu5m rates
gapu5m_rates_all_tips$byvar <- "All"
gapu5m_rates_all_period$byvar <- "All"
gapu5m_rates_all_tips$type <- "All"
gapu5m_rates_all_period$type <- "All"
gapu5m_rates_res_tips$type <- "Residence"
gapu5m_rates_res_period$type <- "Residence"
gapu5m_rates_reg_tips$type <- "Region"
gapu5m_rates_reg_period$type <- "Region"
gapu5m_rates <- rbind(gapu5m_rates_all_tips, gapu5m_rates_all_period, 
                      gapu5m_rates_res_tips, gapu5m_rates_res_period,
                      gapu5m_rates_reg_tips, gapu5m_rates_reg_period)
gapu5m_rates <- gapu5m_rates %>%
  arrange(byvar, cut_time, age_d)


# Combine all gapu5m plot rates
gapu5m_plot_all_tips$byvar <- "All"
gapu5m_plot_all_period$byvar <- "All"
gapu5m_plot_all_tips$type <- "All"
gapu5m_plot_all_period$type <- "All"
gapu5m_plot_res_tips$type <- "Residence"
gapu5m_plot_res_period$type <- "Residence"
gapu5m_plot_reg_tips$type <- "Region"
gapu5m_plot_reg_period$type <- "Region"
gapu5m_plot <- rbind(gapu5m_plot_all_tips, gapu5m_plot_all_period, 
                     gapu5m_plot_res_tips, gapu5m_plot_res_period,
                      gapu5m_plot_reg_tips, gapu5m_plot_reg_period)
gapu5m_plot <- gapu5m_plot %>%
  arrange(byvar, cut_time, age_d)


# Combine all reach rates
reach_rates_all_tips$byvar <- "All"
reach_rates_all_period$byvar <- "All"
reach_rates_all_tips$type <- "All"
reach_rates_all_period$type <- "All"
reach_rates_res_tips$type <- "Residence"
reach_rates_res_period$type <- "Residence"
reach_rates_reg_tips$type <- "Region"
reach_rates_reg_period$type <- "Region"
reach_rates <- rbind(reach_rates_all_tips, reach_rates_all_period, 
                     reach_rates_res_tips, reach_rates_res_period,
                     reach_rates_reg_tips, reach_rates_reg_period)
reach_rates$agegrp <- "Under5"
reach_rates$agegrp[reach_rates$age_y == 0 & reach_rates$age_y_up != 5] <- "Neonatal"
reach_rates$agegrp[reach_rates$age_d == 28] <- "1to59m"
reach_rates <- reach_rates %>%
  arrange(byvar, cut_time, age_y, age_y_up)

# Save output(s) ----------------------------------------------------------

saveRDS(gapu5m_rates, "./gen/mort/output/gapu5m-rates.rds")
saveRDS(gapu5m_plot, "./gen/mort/output/gapu5m-for-plots.rds")
saveRDS(reach_rates, "./gen/mort/output/reach-rates.rds")
