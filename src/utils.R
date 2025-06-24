

# Prepare DHS variables ---------------------------------------------------

fn_gen_tips <- function(x, module = "BR"){
  
  if(module == "GR"){
    x$b3 <- x$p3
    x$b17 <- x$p17
  }
  
  # convert y/m birth b3 from CMC
  x$b3_yyyy <- floor((x$b3-1)/12) + 1900
  x$b3_mm <- x$b3 - (x$b3_yyyy - 1900)*12
  x$b3_date <- ymd(sprintf("%04d-%02d-15", x$b3_yyyy, x$b3_mm))
  x$b3_dec <- decimal_date(x$b3_date)
  
  # day of birth if reported
  if("b17" %in% names(x)){
    x$b3_dd <- x$b17
  }else{
    x$b3_dd <- NA
  }
  
  # Convert intdate from CMC
  x$intdate_yyyy <- floor((x$v008-1)/12) + 1900
  x$intdate_mm <- x$v008 - (x$intdate_yyyy - 1900)*12
  x$intdate_date <- ymd(sprintf("%04d-%02d-15", x$intdate_yyyy, x$intdate_mm))
  x$intdate_dec <- decimal_date(x$intdate_date)

  # create time prior to survey for dob
  #x$tips <- cut(x$b2_dec, breaks = c(-Inf, x$intdate_dec - 15, x$intdate_dec - 10, x$intdate_dec - 5, Inf),
  #              labels = c("<15", "10-14", "5-9", "0-4"))
  # rowwise for different breaks by row
  x <- x %>%
    rowwise() %>%
    mutate(
      tips = {
        # Check for missing values
        if (is.na(b3_dec) | is.na(intdate_dec)) {
          NA_character_
        } else {
          # Create custom breaks for this row
          breaks <- c(-Inf, intdate_dec - 15, intdate_dec - 10, intdate_dec - 5, Inf)
          
          as.character(cut(b3_dec, breaks = breaks, labels = c(">15", "10-14", "5-9", "0-4"), include.lowest = TRUE))
        }
      }
    ) %>%
    ungroup()
  
  # create period for dob
  # survey year breaks and labels
  surveyyear <- as.numeric(as.character(unique(x$SurveyYear)))
  surveyyear_breaks <- c(-Inf, surveyyear+1 - 15, surveyyear+1 - 10, surveyyear+1 - 5, surveyyear+1)
  surveyyear_labs <- c( paste0("<",surveyyear_breaks[2]),
                        paste0("[", surveyyear_breaks[2], ", ", surveyyear_breaks[3], ")"),
                        paste0("[", surveyyear_breaks[3], ", ", surveyyear_breaks[4], ")"),
                        paste0("[", surveyyear_breaks[4], ", ", surveyyear+1, ")") )
  x$period <- cut(x$b3_dec, breaks = surveyyear_breaks, labels = surveyyear_labs)

  
  #head(x[,c("b3", "b3_yyyy", "b3_mm", "b3_dd", "b3_date", "b3_dec", "intdate_dec", "tips", "period")])
  
  return(x)
  

}

fn_gen_aad <- function(x, module = "BR"){
  
  if(module == "GR"){
    x$b6 <- x$p6
  }
  
  x$b6_num <- as.numeric(as.character(x$b6))
  x$aad_unit <- trunc(x$b6_num/100)
  x$aad_value <- x$b6_num - x$aad_unit * 100
  # Transform into months
  x$aadm <- x$aad_value
  x$aadm[which(x$aad_unit == 1)] <- x$aadm[which(x$aad_unit == 1)] / 30.5
  x$aadm[which(x$aad_unit == 3)] <- x$aadm[which(x$aad_unit == 3)]*12
  x$aadm[which(x$aad_value %in% c(97,98,99))] <- "missing"
  # Transform into days
  x$aadd <- x$aad_value
  x$aadd[which(x$aad_unit == 2)] <- x$aadd[which(x$aad_unit == 2)] * 30.5
  x$aadd[which(x$aad_unit == 3)] <- x$aadd[which(x$aad_unit == 3)] * 365.25
  x$aadd[which(x$aad_value %in% c(97,98,99))] <- "missing"
  
  return(x)
}

fn_gen_childsex <- function(x, module = "BR"){
  
  if(module == "GR"){
    x$b4 <- x$p4
  }
  
  x <- x %>%
    mutate(childsex = case_when(
      b4 == 1 ~ "Garçon",
      b4 == 2 ~ "Fille",
      TRUE ~ NA_character_),    
      childsex = factor(
        childsex,
        levels = c(
          "Garçon", "Fille")
      )
    )
  return(x)
}

fn_gen_fph_var <- function(x){
  
  x <- x %>%
    mutate(pregout = case_when(
      p32 == 1 ~ "Né vivant",
      p32 == 2 ~ "Mort né",
      p32 == 3 ~ "Fausse-couche",
      p32 == 4 ~ "Avortement",
      TRUE ~ NA_character_),    
      pregout = factor(
        pregout,
        levels = c(
          "Né vivant", "Mort né", "Fausse-couche", "Avortement")
      )
    ) %>%
    mutate(b5 = p5, # child is still alive
           b10 = p10, # flag for dob
           b13 = p13) # flag for aad
  
  return(x)
  
  
}

# DHS quality check indicators --------------------------------------------

# day of birth heaping for all days
fn_reldifdob <- function(x, module = "BR"){
  
  # only keep live births for fph
  # good to do because will be comparing with lots of fbh
  if(module == "GR"){
    x <- x %>%
      mutate(pregoutval = as.numeric(pregout)) %>%
      filter(pregoutval == 1)
  }
  
  day_counts <- data.frame(day = 1:31,
                          freq = c(rep(12, 29), 11, 7))
  day_counts$per <- day_counts$freq/sum(day_counts$freq)
  
  x <- x %>%
    group_by(b3_dd, tips) %>%
    summarise(n = n()) %>%
    group_by(tips) %>%
    mutate(total = sum(n)) %>%
    left_join(day_counts, by = c("b3_dd" ="day")) %>%
    mutate(exp = total * per,
           dev = (n-exp)/exp*100) %>%
    filter(!is.na(b3_dd))
    

  return(x)
  
}


# aad heaping
fn_reldif7d <- function(x, module = "BR"){
  
  # Already only includes live births because filtering to aadd >=5
  
  # # only keep live births for fph
  # if(module == "GR"){
  #   x <- x %>%
  #     mutate(pregoutval = as.numeric(pregout)) %>%
  #     filter(pregoutval == 1)
  # }
  
  x <- x %>%
    mutate(aadd = as.numeric(aadd)) %>%
    filter(aadd >= 5 & aadd <= 9) %>%
    group_by(aadd, tips) %>%
    summarise(n = n()) %>%
    group_by(tips) %>%
    mutate(exp = sum(n)/length(5:9),
           dev = (n-exp)/exp*100) %>%
    filter(aadd == 7) 
  return(x)
}

fn_reldif12m <- function(x, module = "BR"){
  
  # Already only includes live births because filtering to aadm >= 10
  
  # # only keep live births for fph
  # if(module == "GR"){
  #   x <- x %>%
  #     mutate(pregoutval = as.numeric(pregout)) %>%
  #     filter(pregoutval == 1)
  # }
  
  x <- x %>%
    mutate(aadm = as.numeric(aadm)) %>%
    filter(aadm >= 10 & aadm <= 14) %>%
    group_by(aadm, tips) %>%
    summarise(n = n()) %>%
    group_by(tips) %>%
    mutate(exp = sum(n)/length(10:14),
           dev = (n-exp)/exp*100) %>%
    filter(aadm == 12)
  
  return(x)
  
}

# Sex ratio at birth
fn_srb <- function(x, module = "BR"){
  
  # only live births
  if(module == "GR"){
    x <- x %>%
      mutate(pregoutval = as.numeric(pregout)) %>%
      filter(pregoutval == 1)
  }
  
  x <- x %>%
    mutate(wt = v005/1000000) %>%
    group_by(tips, childsex) %>%
    summarise(n = sum(wt)) %>%
    pivot_wider(
      id_cols = tips,
      names_from = childsex,
      values_from = n
    ) %>%
    mutate(srb = Garçon/Fille
           )
  return(x)
}

# Sex ratio of neonatal deaths
fn_srd <- function(x, module = "BR"){
  
  # only live births
  if(module == "GR"){
    x <- x %>%
      mutate(pregoutval = as.numeric(pregout)) %>%
      filter(pregoutval == 1)
  }
  
  x <- x %>%
    filter(b5 == 0 & aadm < 1) %>%
    mutate(wt = v005/1000000) %>%
    group_by(tips, childsex) %>%
    summarise(n = sum(wt)) %>%
    pivot_wider(
      id_cols = tips,
      names_from = childsex,
      values_from = n
    ) %>%
    mutate(srd = Garçon/Fille)
  
  return(x)
  
}

# Percent of infant deaths that are neonatal
fn_pneo <- function(x, module = "BR"){
  
  # only live births
  if(module == "GR"){
    x <- x %>%
      mutate(pregoutval = as.numeric(pregout)) %>%
      filter(pregoutval == 1)
  }
  
  x <- x %>%
    filter(b5 == 0 & aadm <= 12) %>%
    mutate(wt = v005/1000000,
           isneo = ifelse(aadd <= 28, "neo", "post")) %>%
    group_by(tips, isneo) %>%
    summarise(n = sum(wt)) %>%
    pivot_wider(
      id_cols = tips,
      names_from = isneo,
      values_from = n
    ) %>%
    mutate(pneo = sprintf("%0.1f",round(neo/(neo+post)*100, 1)))
  
  return(x)
  
}

fn_dob_flag <- function(x, module = "BR"){
  
  # why not keep for all births?
  # # only live births
  # if(module == "GR"){
  #   x <- x %>%
  #     mutate(pregoutval = as.numeric(pregout)) %>%
  #     filter(pregoutval == 1)
  # }
  
  x <- x %>%
    group_by(tips, b10) %>%
    summarise(n = n()) %>%
    mutate(
      flag = case_when(
        b10 == 0 ~ "Month, year and day",
        b10 == 1 ~ "Month and year - information complete",
        b10 == 2 ~ "Month and age - year imputed",
        b10 == 3 ~ "Year and age - month imputed",
        b10 == 4 ~ "Year and age - year ignored",
        b10 == 5 ~ "Year - age/month imputed",
        b10 == 6 ~ "Age - year/month imputed",
        b10 == 7 ~ "Month - age/year imputed",
        b10 == 8 ~ "None - all imputed",
        TRUE ~ NA_character_
      )
    ) 
  
  return(x)
  
}

fn_aad_flag <- function(x, module = "BR"){
  
  # only live births
  if(module == "GR"){
    x <- x %>%
      mutate(pregoutval = as.numeric(pregout)) %>%
      filter(pregoutval == 1)
  }
  
  x <- x %>%
    group_by(tips, b13) %>%
    summarise(n = n()) %>%
    mutate(
      flag = case_when(
        b13 == 0 ~ "No flag",
        b13 == 1 ~ "> interview",
        b13 == 2 ~ "< breastfeeding",
        b13 == 3 ~ "< age supplemented",
        b13 == 4 ~ "< first breastfed",
        b13 == 5 ~ "< last vaccination",
        b13 == 6 ~ "Outside range",
        b13 == 7 ~ "Imputed, units given",
        b13 == 8 ~ "Imputed, no units",
        is.na(b13) ~ "Not applicable",
        TRUE ~ NA_character_
      )
    )
    
  return(x)
  
}

# Mortality calculations --------------------------------------------------

# Add rows for plotting Qx and mx
fn_addrows <- function(x, uncertainty = FALSE){
  firstrow <- x[1, ]
  firstrow$Qx <- 0
  if(uncertainty){
    firstrow$Qx_lower <- 0
    firstrow$Qx_upper <- 0
  }
  firstrow$age_y <- 0
  lastrow <- x[nrow(x),]
  lastrow$age_y <- 5
  x <- rbind(firstrow, x, lastrow)
  return(x)
}

fn_calcmort <- function(dat, ages, period = NULL, tips = NULL){
  
  # x data with dob_dec, event, expo
  # ages vector of ages for which mortality rates to be estimate (days)
  # period vector with periods of interest
  # tips with time prior to survey
  
  if(!is.null(tips)){
    dat$cut_time <- tcut(dat$dob_dec - dat$v008_dec, -rev(tips), labels = rev(.epis_labels(tips)))
  }
  if(!is.null(period)){
    dat$cut_time <- tcut(dat$dob_dec, period, labels = rev(.epis_labels(period)))
  }
  
  # "startage" sets the age at entry time (that is birth), so it is 0
  dat$startage <- 0
  
  # define age groups in weeks and months (but expressed in years) and labels (expressed in days)
  agecut <-  c(0,7,14, 21, 28, seq(60.8750, 365.25*5,  30.4375))/365.25
  agecutlab <- as.character(agecut[1:(length(agecut)-1)]*365.25)
  dat$cut_age <- tcut(dat$startage, agecut, labels = agecutlab)
  
  # function that computes deaths and person-years for the defined periods and age group (use weight variable)
  # pyears(surv ~ year + age, weights = tmp$v005/1000000, scale = 1, data.frame = TRUE)
  calcpyears <- pyears(Surv(time = expo, event = event, type = "right") ~ cut_time + cut_age, weights = wt, scale = 1, dat, data.frame = TRUE)
  PY <- calcpyears[[2]]
  PY <- PY[order(PY$cut_time, PY$cut_age),]
  
  # Estimate mx (PY$event/PY$pyears)
  PY$mx     <- PY$event/PY$pyears
  PY$age_d_low  <- as.numeric(paste(PY$cut_age))
  PY$age <- as.numeric(as.character(PY$cut_age))
  PY <- PY %>%
    group_by(cut_time) %>%
    arrange(cut_time, age) %>%
    mutate(n_d = c(rep(7,4), 32.8750, rep(30.4375,58)),
           age_d_up = age_d_low + n_d)
  
  # Define age groups for estimation
  age_ints <- c(ages[1], ages[-1] - ages[-length(ages)])
  
  # Calculate cumulative Qx for desired age groups
  df_Qx <- map_dfr(ages, function(x) {
    PY %>%
      filter(age_d_up <= x) %>%
      group_by(cut_time) %>%
      summarize(
        age_d_up = x,
        Qx = 1 - exp(-sum(mx * n_d/365.25)),
        .groups = "drop"
      )
  })
  # Decumulate Qx to get qx, and add age columns
  df_Qx <- df_Qx %>%
    group_by(cut_time) %>%
    mutate(
      age_d = c(0, ages[-length(ages)]),
      n_d = age_ints,
      # alternative way of deriving mx
      #checkmx = -log(1 - c(Qx[1], 1 - (1 - Qx[-1]) / (1 - Qx[-length(Qx)]))) / (n_d / 365.25),
      qx = c(Qx[1], Qx[-1] - Qx[-length(Qx)])) %>%
    ungroup() %>%
    mutate(
      age_d_mid = age_d + n_d/2,
      age_d_up = age_d + n_d,
      age_y = age_d/365.25,
      age_y_up = age_d_up/365.25
    ) %>%
    arrange(cut_time, age_d)
  
  # Calculate events, exposure, and mx
  age_lows <- c(0, ages[-length(ages)])
  df_events <- map2_dfr(age_lows, ages, function(low, high) {
    PY %>%
      filter(age_d_low >= low, age_d_up <= high) %>%
      group_by(cut_time) %>%
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
    full_join(df_events, by = c("cut_time", "age_d")) %>%
    arrange(cut_time, age_d)
  
  # Apply plot rows, grouped by year
  df_plot <- df_rates %>%
    group_split(cut_time) %>%       # split into list of data frames by year
    map(fn_addrows) %>%         # apply function to each group
    bind_rows() %>%             # recombine into one data frame
    arrange(cut_time, age_y)        # optional: sort the result
  
  l_res <- list(df_rates, df_plot)
  names(l_res) <- c("rates", "plot")
  
  return(l_res)
  
}




# Demogsurv ---------------------------------------------------------------

.epis_labels <- function(x){
  if("labels" %in% attributes(x))
    return(labels(x)[-length(x)])
  lower <- x[-length(x)]
  upper <- x[-1]-1
  val <- ifelse(lower==upper, lower, paste0(lower, "-", upper))
  gsub("-Inf", "+", val)
}

.mm_aggr <- function(mf, agegr){
  
  if(any(!vapply(mf, is.factor, logical(1)))){
    v <- !vapply(mf, is.factor, logical(1))
    stop(paste("Not all 'by' variables are factors:",
               paste(names(v)[v], collapse = ", ")))
  }
  
  if(!"agegr" %in% names(mf))
    stop("'agegr' variable not in mf")
  
  ## Calculate duration of each age group
  dur <- setNames(diff(agegr), levels(mf$agegr))
  dur <- dur[mf$agegr]
  
  mf$agegr <- NULL
  
  if(length(mf))
    mf$byf <- do.call(interaction, c(mf, drop=TRUE))
  
  if(!length(mf) || length(levels(mf$byf)) == 1)
    mf$byf <- 1
  
  mm <- model.matrix(~-1+byf, mf) * dur
  
  df <- mf[!duplicated(mf$byf), , drop=FALSE]
  df <- df[order(df$byf), , drop=FALSE]
  df["byf"] <- NULL
  rownames(df) <- NULL
  
  list(df = df, mm = mm)
}

my_calc_nqx <- function(data,
                        by = NULL,
                        agegr = c(0, 1, 3, 5, 12, 24, 36, 48, 60)/12,
                        period = NULL,
                        cohort = NULL,
                        tips = c(0, 5, 10, 15),
                        clusters=~v021,
                        strata=~v024+v025,
                        weight= "v005",
                        dob="dob_dec",
                        dod="dod_dec",
                        death="death",
                        intv = "v008_dec",
                        varmethod = "lin",
                        origin=1900,
                        scale=1){
  
  data$tstop <- ifelse(data[[death]], data[[dod]], data[[intv]])
  
  data$dob <- data[[dob]]
  data$death <- data[[death]]
  data$intv <- data[[intv]]
  data$weights <- data[[weight]] / mean(data[[weight]])
  
  if(is.null(by))
    by <- ~1
  
  vars <- unique(unlist(lapply(c(by, strata, clusters), all.vars)))
  f <- tryCatch(
    formula(paste("~", paste(vars, collapse = "+"))),
    error = function(e) ~1
  )
  mf <- model.frame(f, data=data, na.action=na.pass, death=death,
                    weights=weights, dob=dob, intv=intv, tstop=tstop)
  
  aggr <- demog_pyears(f, mf, period=period, agegr=agegr, tips=tips, event="(death)",
                       tstart="(dob)", tstop="(tstop)", weights="(weights)",
                       origin=origin, scale=scale)$data
  
  ## All values of factor combinations that appear
  byvar <- intersect(c(all.vars(by), "agegr", "period", "cohort", "tips"),
                     names(aggr))
  aggr$byf <- interaction(aggr[byvar], drop=TRUE)
  
  ## prediction for all factor levels that appear
  pred <- data.frame(aggr[c(byvar, "byf")])[!duplicated(aggr$byf),]
  pred <- pred[order(pred$byf), ]
  pred$pyears <- 1
  
  ## Matrix to aggregate piecewise-constant rates to cumulative hazards
  dfmm <- .mm_aggr(pred[byvar], agegr)
  mm <- dfmm$mm
  
  #if(varmethod == "lin") {
  #des <- survey::svydesign(ids=clusters, strata=strata, data=aggr, weights=~1)
  
  ## fit model
  #f <- if(length(levels(aggr$byf)) == 1)
  #  event ~ offset(log(pyears))
  #else
  #  event ~ -1 + byf + offset(log(pyears))
  
  f <- event ~ -1 + byf + offset(log(pyears))
  
  mod <- glm(f, aggr, family=quasipoisson)
  
  mx <- predict(mod, pred, type="response", vcov=TRUE)
  
  lest <- drop(mx %*% mm)
  #lv <- t(mm) %*% vcov(mx) %*% mm
  #est <- 
  #  dF <- diag(exp(-lest), length(lest))
  #v <- dF %*% lv %*% dF
  
  estdf <- data.frame(est  = 1 - exp(-lest) #,
                      #se   = sqrt(diag(v)),
                      #ci_l = 1 - exp(-(lest - qnorm(0.975)*sqrt(diag(lv)))),
                      #ci_u = 1 - exp(-(lest + qnorm(0.975)*sqrt(diag(lv))))
  )
  #attr(estdf, "var") <- v
  
  #} else if(varmethod %in% c("jkn", "jk1")) {
  
  ## Convert to array with events and PYs for each cluster
  ## reshape2::acast is MUCH faster than stats::reshape
  #events_clust <- reshape2::acast(aggr, update(clusters, byf ~ .), value.var="event")
  # pyears_clust <- reshape2::acast(aggr, update(clusters, byf ~ .), value.var="pyears")
  
  #if(varmethod == "jkn"){
  #  aggr$strataid <- as.integer(interaction(aggr[all.vars(strata)], drop=TRUE))
  #  strataid <- drop(reshape2::acast(unique(aggr[c(all.vars(clusters), "strataid")]),
 # update(clusters,  1 ~ .), value.var="strataid"))
#} else
#  strataid <- NULL

#estdf <- jackknife(events_clust, pyears_clust, strataid, t(dfmm$mm), function(x) 1 - exp(-x))
#} else
#stop(paste0("varmethod = \"", varmethod, "\" is not recognized."))

val <- data.frame(dfmm$df, estdf)
#attr(val, "var") <- vcov(estdf)

#rownames(val) <- NULL

val
}

# Jackknife ---------------------------------------------------------------

fn_jackknife_mort <- function(dat, ages, period = NULL, tips = NULL, 
                                     cluster_var = "grappe", strata_var = "strate",
                                     return_replicates = FALSE, verbose = TRUE) {
  # ages = gapu5m_age
  # ages = gapu5m_age
  # tips = c(0, 5, 10, 15)
  # cluster_var = "grappe"
  # strata_var = "strate"
  # period = NULL
  # return_replicates = TRUE
  # verbose = TRUE
  
  dat <- dat %>%
    mutate(cluster = !!sym(cluster_var),
           strata = !!sym(strata_var))
  
  strata_clusters <- dat %>%
    distinct(strata, cluster) %>%
    group_by(strata) %>%
    summarise(clusters = list(unique(cluster)), .groups = "drop")
  
  if (verbose) message("Calculating full-sample estimates...")
  
  point_est <- fn_calcmort(dat, ages = ages, period = period, tips = tips)$rates %>%
    select(cut_time, age_y, age_y_up, age_d, age_d_up, n_d, mx, qx, Qx) %>%
    rename(mx_full = mx, qx_full = qx, Qx_full = Qx)
  
  if (verbose) message("Running jackknife replicates...")
  
  jack_estimates <- map_dfr(1:nrow(strata_clusters), function(i) {
    stratum <- strata_clusters$strata[i]
    clusts <- strata_clusters$clusters[[i]]
    n_clust <- length(clusts)
    
    map_dfr(seq_along(clusts), function(j) {
      clust_drop <- clusts[j]
      if (verbose) message("Stratum: ", stratum, " | Dropping cluster: ", clust_drop)
      
      dat_jk <- dat %>% filter(!(strata == stratum & cluster == clust_drop))
      
      jk_rates <- tryCatch({
        fn_calcmort(dat_jk, ages = ages, period = period, tips = tips)$rates %>%
          select(cut_time, age_y, age_y_up, age_d, age_d_up, n_d, mx, qx, Qx) %>%
          mutate(strata = stratum, replicate = j, n_clust = n_clust, cluster_dropped = clust_drop)
      }, error = function(e) {
        if (verbose) message("Error in replicate (stratum: ", stratum, ", cluster: ", clust_drop, ")")
        NULL
      })
      
      return(jk_rates)
    })
  })
  
  if (verbose) message("Merging replicate estimates with full-sample estimates...")
  
  jack_combined <- jack_estimates %>%
    left_join(point_est, by = c("cut_time", "age_y", "age_y_up", "age_d", "age_d_up", "n_d"))
  
  # Check which groups are missing mx_full after join
  missing_full <- jack_combined %>%
    filter(is.na(mx_full) | is.na(qx_full) | is.na(Qx_full))
  
  if (nrow(missing_full) > 0) {
    warning("Some replicates did not match full-sample estimates. Removing ", nrow(missing_full), " unmatched rows.")
  }
  
  # Remove rows where full-sample values are missing
  jack_combined <- jack_combined %>%
    filter(!is.na(mx_full) & !is.na(qx_full) & !is.na(Qx_full))
  
  if (verbose) message("Calculating jackknife variances...")
  
  jack_var <- jack_combined %>%
    group_by(cut_time, age_y, age_y_up, age_d, age_d_up, n_d, strata) %>%
    summarise(
      mx_full = first(mx_full),
      qx_full = first(qx_full),
      Qx_full = first(Qx_full),
      var_mx = (first(n_clust) - 1) / first(n_clust) * sum((mx - mx_full)^2, na.rm = TRUE),
      var_qx = (first(n_clust) - 1) / first(n_clust) * sum((qx - qx_full)^2, na.rm = TRUE),
      var_Qx = (first(n_clust) - 1) / first(n_clust) * sum((Qx - Qx_full)^2, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(cut_time, age_y, age_y_up, age_d, age_d_up, n_d) %>%
    summarise(
      mx = first(mx_full),
      qx = first(qx_full),
      Qx = first(Qx_full),
      mx_se = sqrt(sum(var_mx, na.rm = TRUE)),
      qx_se = sqrt(sum(var_qx, na.rm = TRUE)),
      Qx_se = sqrt(sum(var_Qx, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    mutate(
      mx_lower = pmax(0, mx - 1.96 * mx_se),
      mx_upper = mx + 1.96 * mx_se,
      qx_lower = pmax(0, qx - 1.96 * qx_se),
      qx_upper = pmin(1, qx + 1.96 * qx_se),
      Qx_lower = pmax(0, Qx - 1.96 * Qx_se),
      Qx_upper = pmin(1, Qx + 1.96 * Qx_se)
    )
  
  if (return_replicates) {
    return(list(estimates = jack_var, replicates = jack_combined))
  } else {
    return(list(estimates = jack_var, replicates = NULL))
  }
}
