################################################################################
#' @description In this version, i separate cumulation of Qx and summarizing events and exposure. 
#' so mx can be calculated directly instead of derived from Qx
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
dat <- readRDS("./gen/fph/output/fph-tips.rds")
################################################################################

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

# Age at death in person-years
#dat$expo[dat$event == 1] <- dat$aady[dat$event == 1]
# Age at death in person-years with imputation for random days
dat$expo[dat$event == 1 & dat$aad_unit == 1] <- (as.numeric(dat$aad_value[dat$event == 1 & dat$aad_unit == 1]) + dat$random[dat$event == 1 & dat$aad_unit == 1])/365.25   
dat$expo[dat$event == 1 & dat$aad_unit == 2] <- (as.numeric(dat$aad_value[dat$event == 1 & dat$aad_unit == 2]) + dat$random[dat$event == 1 & dat$aad_unit == 2])/12          
dat$expo[dat$event == 1 & dat$aad_unit == 3] <- (as.numeric(dat$aad_value[dat$event == 1 & dat$aad_unit == 3]) + dat$random[dat$event == 1 & dat$aad_unit == 3])

l_df <- split(dat, dat$qtype)
l_resrates <- list()
l_resplot <- list()

for(i in 1:length(l_df)){
  
  dat <- l_df[[i]]
  
  # Provide the exposition and the life status at the moment of right truncation
  surv <- with(dat, Surv(time = dat$expo, event = dat$event, type = "right"))
  
  # define year cuts (and labels)
  yearcut <- c(2010, 2015, 2020, 2025)
  yearcutlab <- c("[2010, 2015)", "[2015, 2020)", "[2020, 2025)")
  
  # give also date of birth
  year <- tcut(dat$dob_dec, yearcut, labels = yearcutlab)     
  
  # "startage" sets the age at entry time (that is birth), so it is 0
  dat$startage <- 0
  
  # define age groups in weeks and months (but expressed in years) and labels (expressed in days)
  agecut <-  c(0,7,14, 21, 28, seq(60.8750, 365.25*5,  30.4375))/365.25
  agecutlab <- as.character(agecut[1:(length(agecut)-1)]*365.25)
  age <- tcut(dat$startage, agecut, labels = agecutlab)
  
  # function that computes deaths and person-years for the defined periods and age group (use weight variable)
  # !!!! weights?
  pyears <- pyears(surv ~ year + age, scale = 1, data.frame = TRUE)
  PY <- pyears[[2]]
  PY <- PY[order(PY$year, PY$age),]
  
  # Estimate mx (PY$event/PY$pyears)
  PY$mx     <- PY$event/PY$pyears
  PY$age_d_low  <- as.numeric(paste(PY$age))
  PY$age <- as.numeric(as.character(PY$age))
  PY$n_d    <- rep(c(rep(7,4), 32.8750, rep(30.4375,58)), length(yearcutlab))
  PY$age_d_up  <- PY$age_d_low + PY$n_d 
  
  # Define age groups 
  age_vec <- c(7,14, 21, 28,
                  seq(60.8750, 365.25,  30.4375),
                  seq(365.25 + 365.25/4 , 365.25*2,  365.25/4),
                  seq(365.25*3 , 365.25*5,  365.25))
  age_ints <- c(age_vec[1], age_vec[-1] - age_vec[-length(age_vec)])

  # Calculate cumlative Qx
  df_Qx <- map_dfr(age_vec, function(x) {
    PY %>%
      filter(age_d_up <= x) %>%
      group_by(year) %>%
      summarize(
        age_d_up = x,
        Qx = 1 - exp(-sum(mx * n_d/365.25)),
        .groups = "drop"
      )
  })
  # Decumulate Qx and add age columns
  df_Qx <- df_Qx %>%
    group_by(year) %>%
    mutate(
      age_d = c(0, age_vec[-length(age_vec)]),
      n_d = age_ints,
      checkmx = -log(1 - c(Qx[1], 1 - (1 - Qx[-1]) / (1 - Qx[-length(Qx)]))) / (n_d / 365.25),
      qx = c(Qx[1], Qx[-1] - Qx[-length(Qx)])) %>%
    ungroup() %>%
    mutate(
      age_d_mid = age_d + n_d/2,
      age_d_up = age_d + n_d,
      age_y = age_d/365.25,
      age_y_up = age_d_up/365.25
    ) %>%
    arrange(year, age_d)
  
  # Calculate events, exposure, and mx
  age_lows <- c(0, age_vec[-length(age_vec)])
  df_events <- map2_dfr(age_lows, age_vec, function(low, high) {
    PY %>%
      filter(age_d_low >= low, age_d_up <= high) %>%
      group_by(year) %>%
      summarize(
        age_d = low,
        events = sum(event),
        pyears = sum(pyears),
        .groups = "drop"
      )
  })
  df_events <- df_events %>%
    mutate(mx = events/pyears)
  
  # Merge
  df_rates <- df_Qx %>%
    full_join(df_events, by = c("year", "age_d")) %>%
    arrange(year, age_d)

  # Add plot rows
  fn_addrows <- function(x){
    firstrow <- x[1, ]
    firstrow$Qx <- 0
    firstrow$age_y <- 0
    lastrow <- x[nrow(x),]
    lastrow$age_y <- 5
    x <- rbind(firstrow, x, lastrow)
    return(x)
  }
  # Apply plot rows, grouped by year
  df_plot <- df_rates %>%
    group_split(year) %>%       # split into list of data frames by year
    map(fn_addrows) %>%         # apply function to each group
    bind_rows() %>%             # recombine into one data frame
    arrange(year, age_y)        # optional: sort the result
  
  df_rates$byvar <- names(l_df)[i]
  df_plot$byvar <- names(l_df)[i]
  
  l_resrates[[i]] <- df_rates
  l_resplot[[i]] <- df_plot
}
  
df_rates <- do.call(rbind, l_resrates)
df_plot <- do.call(rbind, l_resplot)


ggplot(df_plot) +
  geom_step(aes(x = age_y, y = mx, col = byvar)) +
  scale_y_log10() +
  facet_wrap(~year)
ggplot(df_plot) +
  geom_step(aes(x = age_y, y = Qx*1000, col = byvar)) +
  facet_wrap(~year)
  
  