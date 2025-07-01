################################################################################
#' @description Calculate mortality rates for Mali DHS
#' @return 
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(haven)
library(tidyverse)
library(lubridate)
library(purrr)
library(demogsurv)
#' Inputs
source("./src/utils.R")
## Mali BR modules
l_br <- readRDS("./gen/dhs/temp/br-ml-prep.rds")
################################################################################

# drop early survey that is missing v025
l_br <- l_br[-1]

# selection variables
l_data <- lapply(l_br, function(x) x[c("caseid","v005", "v008","b3", "b5", "b6", "v024", "v025")]) 
l_data <- lapply(l_data, function(x) { x$event <- !is.na(x$b6)
                                       x$random <- NA 
                                       x$v024 <- as_factor(x$v024)
                                       x$v025 <- as_factor(x$v025) ; return(x)})

# estimation of the exposure for alive children (in person-YEARS)

# imputation of day for date of survey and date of birth
set.seed(111)
l_data <- lapply(l_data, function(x) { x$v008_d <- x$v008 - runif(nrow(x), min = 0, max = 1); return(x)})
set.seed(777)
l_data <- lapply(l_data, function(x) { x$b3_d <- x$b3 - runif(nrow(x), min = 0, max = 1); return(x)})
# these will both be over 12 (b3_d/12) when used later

# Exposure when date of birth = date of survey 
l_data <- lapply(l_data, function(x) { x$select <- x$event == FALSE & x$v008 == x$b3 ; return(x)})
set.seed(1)
l_data <- lapply(l_data, function(x) { x$expo <- NULL 
                                       x$expo[x$select == TRUE] <- runif(nrow(x[x$select == TRUE,]), min = 0, max = 1)/12 ; return(x)})
# Assigning exposure as random number of days between 0 and 1 month

# Exposure when date of birth != date of survey 
l_data <- lapply(l_data, function(x) { x$select <- x$event == FALSE & x$v008 != x$b3 ; return(x)})
l_data <- lapply(l_data, function(x) { x$expo[x$select == TRUE] <- (x$v008_d[x$select == TRUE] - x$b3_d[x$select == TRUE])/12 ; return(x)})

# estimation of the exposition for dead children  (in person-YEARS)

# set a random number between 0 and 1 for uniform imputation of days
set.seed(777)
l_data <- lapply(l_data, function(x) { x$random[x$event == TRUE] <- runif(nrow(x[x$event == TRUE,]), min = 0,  max = 1) ; return(x)})
# if child has died, event == true; pick random number between 0 and 1

# use variable b6 to get age at death (in person-years -> /365.25 or /12)

# replace NA's with 0 (for those who haven't died)
l_data <- lapply(l_data, function(x) { x$b6[is.na(x$b6)] <- 0 ; return(x)})

# redistribute missing values
l_data <- lapply(l_data, function(x){x$b6[x$b6 >= 190 & x$b6 < 200] <- sample(100:130,1) ; return(x) }) # Unknown days
l_data <- lapply(l_data, function(x){x$b6[x$b6 >= 290 & x$b6 < 300] <- sample(200:223,1) ; return(x) }) # Unknown months
l_data <- lapply(l_data, function(x){x$b6[x$b6 >= 390 & x$b6 < 400] <- sample(300:334,1) ; return(x) }) # Unknown years
l_data <- lapply(l_data, function(x){x$b6[x$b6 >= 900] <- sample(c(100:130, 200:223, 300:334), 1) ; return(x) })
# recalculate exposure for redistributed values
l_data <- lapply(l_data, function(x){x$expo[x$b6 >= 100 & x$b6 < 200] <- (as.numeric(x$b6[x$b6 >= 100 & x$b6 < 200]) - 100 + x$random[x$b6 >= 100 & x$b6 < 200])/365.25 ; return(x)  }) 
l_data <- lapply(l_data, function(x){x$expo[x$b6 >= 200 & x$b6 < 300] <- (as.numeric(x$b6[x$b6 >= 200 & x$b6 < 300]) - 200 + x$random[x$b6 >= 200 & x$b6 < 300])/12 ; return(x) }) 
l_data <- lapply(l_data, function(x){x$expo[x$b6 >= 300] <- (as.numeric(x$b6[x$b6 >= 300]) - 300 + x$random[x$b6 >= 300]) ; return(x) }) 

# converting date of birth in years
l_data <- lapply(l_data, function(x) { x$dob_y <- x$b3_d/12 + 1900 ; return(x)})

# converting date of survey (just for clarity)
l_data <- lapply(l_data, function(x) { x$dos_y <- x$v008_d/12 + 1900 ; return(x)})

gapu5m_age <- c(7,14, 21, 28,
                seq(60.8750, 365.25,  30.4375),
                seq(365.25 + 365.25/4 , 365.25*2,  365.25/4),
                seq(365.25*3 , 365.25*5,  365.25))

l_res <- lapply(l_data, function(x) fn_calcmort_dhs(x, ages = gapu5m_age, tips = c(0, 5, 10, 15)) )

df_plot_nat <- plyr::ldply(l_res, function(x) x$plot, .id = "source")

l_plot_reg <- list()
l_plot_res <- list()
for(i in 1:length(l_data)){
  
  dat <- l_data[[i]]
  
  groups <- dat %>% group_by(v024) %>% group_split()
  qlregions <- dat %>% group_by(v024) %>% group_keys() %>% pull(v024)
  results_byvar <- map(groups, ~ fn_calcmort_dhs(.x, ages = gapu5m_age, tips = c(0, 5, 10, 15)))
  names(results_byvar) <- qlregions
  gapu5m_plot_reg_tips <- imap_dfr(results_byvar, ~ mutate(.x$plot, byvar = .y))
  
  groups <- dat %>% group_by(v025) %>% group_split()
  qlres <- dat %>% group_by(v025) %>% group_keys() %>% pull(v025)
  results_byvar <- map(groups, ~ fn_calcmort_dhs(.x, ages = gapu5m_age, tips = c(0, 5, 10, 15)))
  names(results_byvar) <- qlres
  gapu5m_plot_res_tips <- imap_dfr(results_byvar, ~ mutate(.x$plot, byvar = .y))
  
  l_plot_reg[[i]] <- gapu5m_plot_reg_tips
  l_plot_res[[i]] <- gapu5m_plot_res_tips
  
}
names(l_plot_reg) <- names(l_data)
names(l_plot_res) <- names(l_data)
df_plot_reg <- plyr::ldply(l_plot_reg, .id = "source")
df_plot_res <- plyr::ldply(l_plot_res, .id = "source")

df_plot_nat %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14"))) %>%
  ggplot() +
  geom_step(aes(x = age_y, y = Qx*1000, color = source)) +
  labs(x = "Age (years)", y = "Q(x)") +
  facet_wrap(~tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank())

# Save output(s) ----------------------------------------------------------

saveRDS(df_plot_nat, "./gen/dhs/output/mldhs-plot-nat.rds")
saveRDS(df_plot_reg, "./gen/dhs/output/mldhs-plot-reg.rds")
saveRDS(df_plot_res, "./gen/dhs/output/mldhs-plot-res.rds")
