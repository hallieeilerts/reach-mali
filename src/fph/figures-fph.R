################################################################################
#' @description Quality checks
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(cowplot)
library(viridis)
library(lubridate)
#' Inputs
dat_filename <- list.files("./gen/fph/audit")
dat_filename <- dat_filename[grepl("dat_aud-dob1", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename),1) # Most recent
aud1 <- read.csv(paste0("./gen/fph/audit/", dat_filename))
dat_filename <- list.files("./gen/fph/audit")
dat_filename <- dat_filename[grepl("dat_aud-dob2", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename),1) # Most recent
aud2 <- read.csv(paste0("./gen/fph/audit/", dat_filename))
dat_filename <- list.files("./gen/fph/audit")
dat_filename <- dat_filename[grepl("dat_aud-aad", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename),1) # Most recent
aud3 <- read.csv(paste0("./gen/fph/audit/", dat_filename))
aud4tab <- readRDS("./gen/fph/audit/tab_sbh-bth-agreement.rds")
aud4dat <- readRDS("./gen/fph/audit/dat_aud-sbh-bth-agreement.rds")
aud5tab <- readRDS("./gen/fph/audit/tab_sbh-dth-agreement.rds")
aud5dat <- readRDS("./gen/fph/audit/dat_aud-sbh-dth-agreement.rds")
dat <- readRDS("./gen/fph/output/fph-tips.rds")
mldhs <- readRDS("./gen/dhs/output/mldhs-bd-tips.rds")
################################################################################

# Missing dob -------------------------------------------------------------

# non-mutually exclusive table
total <- sum(subset(aud1, variable %in% c("total"))$n)
nomiss <- sum(subset(aud1, variable %in% c("dob complete"))$n)
miss_y <- sum(subset(aud1, variable %in% c("dob missing ymd", "dob missing ym", "dob missing yd", "dob missing y"))$n)
miss_m <- sum(subset(aud1, variable %in% c("dob missing ymd", "dob missing ym", "dob missing md", "dob missing m"))$n)
miss_d <- sum(subset(aud1, variable %in% c("dob missing ymd", "dob missing yd", "dob missing md", "dob missing d"))$n)
tab1 <- data.frame(nomiss = nomiss, miss_y = miss_y, miss_m = miss_m, miss_d = miss_d, total = total)
tab1

# mutually exclusive table
tab2 <- subset(aud1, variable != "impossible aad")
tab2$variable <- gsub( "dob ", "", tab2$variable)
tab2$variable <- gsub( "missing ", "", tab2$variable)
tab2$group <- ifelse(tab2$variable == "complete", "Complete", "Incomplete")
tab2$group[tab2$variable == "total"] <- ""
tab2 <- tab2[, c("group", "variable", "n")]
tab2$ord <- 1:nrow(tab2)
tab2$ord[tab2$variable == "complete"] <- 0
tab2$ord[tab2$variable == "total"] <- 100
tab2 <- tab2[order(tab2$ord),]
tab2$variable[tab2$group == "Complete"] <- ""
tab2$per <- sprintf("%0.1f", round(tab2$n/total*100, 1))
tab2$ord <- NULL
# drop those with 0 n
tab2 <- subset(tab2, n != 0)
tab2$variable[tab2$variable == "ymd"] <- "Year, month, day"
tab2$variable[tab2$variable == "md"] <- "Month, day"
tab2$variable[tab2$variable == "y"] <- "Year"
tab2$variable[tab2$variable == "m"] <- "Month"
tab2$variable[tab2$variable == "d"] <- "Day"
tab2$group[tab2$variable == "total"] <- "Total"
tab2$variable[tab2$variable == "total"] <- ""
tab2 <- tab2[order(tab2$group, tab2$variable),]
kbl(tab2, format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
    format.args = list(big.mark = ",", scientific = FALSE), 
    col.names = c("DOB", "Missing", "N", "%"), 
    caption = "Reporting completeness for DOB")


# Missing dob by pregnancy outcome ----------------------------------------

subset(aud2, is.na(q223))

tab3 <- aud2 %>%
  mutate(q223 = case_when(
    q223 == 1 ~ "Live birth",
    q223 == 2 ~ "Stillbirth",
    q223 == 3 ~ "Miscarriage",
    q223 == 4 ~ "Abortion",
    TRUE ~ NA_character_)) %>%
  group_by(q223, dob_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  bind_rows(
    aud2 %>%
      group_by(dob_type) %>%
      summarise(n = n(), .groups = "drop") %>%
      mutate(q223 = "Total")
  ) %>%
  pivot_wider(
    names_from = q223,
    values_from = n
  )
tab3[is.na(tab3)] <- 0
# Calculate column totals (excluding the Total column)
col_totals <- colSums(tab3[ , -1], na.rm = TRUE)  # assuming dob_type is the first column
# Add percentage columns
tab3_pct <- tab3
for (col in names(col_totals)) {
  tab3_pct[[paste0(col, "_pct")]] <-  round(tab3[[col]] / col_totals[[col]] * 100)
}
# Add total row
total_row <- as.list(col_totals)
names(total_row) <- names(col_totals)
# Create 100.0% for each percentage column
for (col in names(col_totals)) {
  total_row[[paste0(col, "_pct")]] <- 100
}
total_row$dob_type <- "Total"
# Bind total row to the bottom
tab3_pct <- bind_rows(tab3_pct, total_row)

tab3_pct %>%
  mutate(DOB = ifelse(dob_type == "complete", "Complete", "Incomplete")) %>%
  mutate(DOB = ifelse(dob_type == "Total", "Total", DOB)) %>%
  mutate(Missing = recode(dob_type,
                          "missing_ymd" = "Year, month, day",
                          "missing_md" = "Month, day",
                          "missing_y" = "Year",
                          "missing_m" = "Month",
                          "missing_d" = "Day",
                          "complete" = "",
                          "Total" = "")) %>%              
  select(DOB, Missing, `Live birth`, `Live birth_pct`, Stillbirth, Stillbirth_pct, Miscarriage, Miscarriage_pct,
         Abortion, Abortion_pct, `NA`, NA_pct, Total, Total_pct) %>% 
  kbl(format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
      format.args = list(big.mark = ",", scientific = FALSE), 
      col.names = c("DOB", "Missing", "N", "%", "N", "%", "N", "%", "N", "%","N", "%", "N", "%"), 
      align = c("l","l", "r", "l","r", "l","r", "l","r", "l","r", "l","r", "l","r", "l"),
      caption = "Reporting completeness for DOB by pregnancy outcome.")  %>%
  kable_styling(font_size = 8) %>%
  add_header_above(c(" " = 2, 
                     "Live birth" = 2, 
                     "Stillbirth" = 2, 
                     "Miscarriage" = 2, 
                     "Abortion" = 2, 
                     "NA" = 2,
                     "Total" = 2))
 
# Missing dob by pregnancy outcome (figure) -------------------------------

p <- aud2 %>%
  group_by(q223, dob_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q223) %>%
  mutate(total = sum(n),
         prop = n/total) %>%
  mutate(q223 = case_when(
      q223 == 1 ~ "Live birth",
      q223 == 2 ~ "Stillbirth",
      q223 == 3 ~ "Miscarriage",
      q223 == 4 ~ "Abortion",
      TRUE ~ NA_character_),
      q223 = factor(
        q223,
        levels = c(
          "Live birth",
          "Stillbirth",
          "Miscarriage",
          "Abortion")
      )
    ) %>%
  mutate(dob_type = factor(dob_type, levels = c("complete", "missing_d", "missing_m", "missing_y","missing_md", "missing_ymd"),
                           labels = c("Complete", "Missing day",  "Missing month", "Missing year","Missing month & day", "Missing month, year, day"))) %>%
  ggplot() +
  geom_bar(aes(x=q223, y = prop, fill = q223), stat = "identity") +
  labs(y = "Proportion", x = "Pregnancy outcome", title = "Pregnancy outcomes by DOB reporting completeness") +
  scale_fill_viridis_d(option = "C", na.value = "grey80") +
  facet_wrap(~dob_type) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = .75))
ggsave("./gen/fph/figures/dob-reporting-tally-pregout.png", p, dpi = 300, width = 6, height = 4)

# alternative: faceted by pregnancy outcome
aud2 %>%
  group_by(q223, dob_type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(q223) %>%
  mutate(total = sum(n),
         prop = n/total) %>%
  mutate(q223 = factor(q223, levels = c(1,2,3,4),
                       labels = c("Live birth", "Stillbirth", "Miscarriage", "Abortion"))) %>%
  mutate(dob_type = factor(dob_type, levels = c("complete", "missing_d", "missing_m", "missing_y","missing_md", "missing_ymd"),
                           labels = c("Complete", "Missing day",  "Missing month", "Missing year","Missing month & day", "Missing month, year, day"))) %>%
  ggplot() +
  geom_bar(aes(x=dob_type, y = prop, fill = dob_type), stat = "identity") +
  labs(y = "Proportion", x = "DOB reporting completeness", title = "DOB reporting completeness by pregnancy outcome") +
  scale_fill_viridis_d(option = "C", na.value = "grey80") +
  facet_wrap(~q223) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        legend.position = "none",
        axis.text.x = element_text(angle = 30, hjust = .75))


# Missing dob by year -----------------------------------------------------

nrow(subset(aud2, is.na(q220y))) # 50
nrow(subset(aud2, !(q216 == 1))) # 4823
nrow(subset(aud2, !(q216 == 1) & is.na(q220y))) # 1

p <- aud2 %>%
  # approximation of tips
  mutate(tips = cut(q220y, breaks = c(-Inf, 2025-15, 2025-10, 2025-5, 2026), 
                    labels = c(">15", "10-14", "5-9", "0-4"))) %>%
  # drops those where tips couldn't be calculated (missing year)
  filter(!is.na(tips)) %>%
  # drop non-live births 
  filter(q223 == 1) %>%
  mutate(tips =factor(tips, levels = c("0-4", "5-9", "10-14", ">15"))) %>%
  group_by(tips, dob_type) %>%
  summarise(n=n()) %>%
  group_by(tips) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(dob_type = factor(dob_type, levels = c("complete", "missing_d", "missing_m", "missing_md"),
                           labels = c("Complete", "Missing day",  "Missing month", "Missing month & day"))) %>%
  ggplot() +
  geom_bar(aes(x=dob_type, y = per, fill = dob_type), stat = "identity") +
  labs(x = "", y = "Percent", fill="DOB reporting", title = "DOB reporting completeness by years prior to survey") +
  facet_wrap(~tips, nrow = 1) +
  scale_y_continuous(limits = c(0,80), expand = c(0, 0)) +
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        text = element_text(size = 10), title = element_text(size = 8)) +
  scale_fill_viridis_d(option = "C")
ggsave("./gen/fph/figures/dob-reporting-tally.png", p, dpi = 300, width = 6, height = 2.5)



# Distribution of day of birth --------------------------------------------

p <- dat %>%  
  filter(tips != ">15") %>%
  mutate(tips = factor(tips, levels =  c("0-4", "5-9", "10-14"))) %>%
  mutate(dobd = day(dob)) %>%
  mutate(barfill = ifelse(dobd %in% c(1, 15,31), "a", "b")) %>%
  group_by(tips, dobd) %>% # barfill, 
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = dobd, y = n), stat = "identity", fill = "#0D0887FF") +
  labs(y = "n", x = "Days", title = "Day of birth heaping") +
  facet_wrap(~tips, nrow=1) +
  #scale_fill_manual(values = c("#DE4968", "#0D0887FF")) +
  scale_x_continuous(breaks = c(1, 15, 30)) +
  guides(fill="none") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/fph/figures/heaping-dobd.png", p, dpi = 300, width = 6, height = 3)

# Distribution of day of birth by child age -------------------------------

subset(dat, is.na(v008_dec))

dat %>%  
  filter(tips != ">15") %>%
  mutate(tips = factor(tips, levels =  c("0-4", "5-9", "10-14")),
        dobd = day(dob),
        age_chld = v008_dec - dob_dec) %>%
  mutate(age_grp = case_when(
    age_chld >= 0  & age_chld < 1    ~ "0-1",
    age_chld >= 1  & age_chld < 5   ~ "1-4",
    age_chld >= 5  & age_chld < 10   ~ "5-9",
    age_chld >= 10 & age_chld < 15  ~ "10-14",
    age_chld >= 15 & age_chld < 20  ~ "15-19",
    age_chld >= 20 & age_chld < 25  ~ "20-24",
    age_chld >= 25 & age_chld < 30  ~ "25-29",
    age_chld >= 30 & age_chld < 35  ~ "30-34",
    age_chld >= 35 & age_chld < 40  ~ "35+",
    TRUE ~ NA_character_
  )) %>%
  mutate(age_grp = factor(age_grp, levels = c(
    "0-1", "1-4", "5-9", "10-14", "15-19", "20-24",
    "25-29", "30-34", "35"
  ))) %>% 
  group_by(tips, age_grp, dobd) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = dobd, y = n), stat = "identity", fill = "#0D0887FF") +
  labs(y = "n", x = "Days", title = "Day of birth heaping") +
  facet_wrap(~age_grp) +
  #scale_fill_manual(values = c("#DE4968", "#0D0887FF")) +
  scale_x_continuous(breaks = c(1, 15, 30)) +
  guides(fill="none") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))


# Heat map of day/month of birth ------------------------------------------

p <- dat %>%  
  filter(tips != ">15") %>%
  mutate(tips = factor(tips, levels =  c("0-4", "5-9", "10-14"))) %>%
  mutate(dobd = day(dob),
         dobm = month(dob))  %>%
  count(tips, dobd, dobm) %>%
  group_by(tips) %>%
  mutate(per = n/sum(n)) %>%
  ggplot(aes(x = dobd, y = dobm, fill = per)) +
  geom_tile(color = "white") +
  facet_wrap(~ tips, nrow = 1) +
  scale_fill_viridis_c(option = "C", name = "Percent") +
  scale_y_continuous(breaks = 1:12) +
  labs(
    title = "Day and month of birth combinations",
    x = "Day of Birth",
    y = "Month of Birth"
  ) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/fph/figures/heatmap-dob-md.png", p, dpi = 300, width = 6, height = 3)

# Distribution of month of birth ------------------------------------------

p <- dat %>%  
  filter(tips != ">15") %>%
  mutate(tips = factor(tips, levels =  c("0-4", "5-9", "10-14"))) %>%
  mutate(dobm = month(dob)) %>%
  mutate(barfill = ifelse(dobm %in% c(1, 15,31), "a", "b")) %>%
  group_by(tips, dobm) %>% # barfill, 
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = dobm, y = n), stat = "identity", fill = "#0D0887FF") +
  labs(y = "n", x = "Months", title = "Month of birth heaping") +
  facet_wrap(~tips, nrow=1) +
  #scale_fill_manual(values = c("#DE4968", "#0D0887FF")) +
  scale_x_continuous(breaks = seq(1, 12, 2)) +
  guides(fill="none") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/fph/figures/heaping-dobm.png", p, dpi = 300, width = 6, height = 3)



# Table reported deaths ---------------------------------------------------

tabdeaths <- dat %>%
  filter(event == 1) %>%
  group_by(tips) %>%
  summarise(n = n())
tabdeaths <- rbind(tabdeaths, c("Total", sum(tabdeaths$n)))
tabdeaths <- tabdeaths %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14", ">15", "Total"))) %>%
  arrange(tips)
kbl(tabdeaths, format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
    format.args = list(big.mark = ",", scientific = FALSE), 
    col.names = c("Years prior to survey", "N"), 
    caption = "Number of reported deaths in REACH-Mali FPH.")


# Table deaths and stillbirths --------------------------------------------

tab4 <- dat %>%
  mutate(q223_aug = case_when(
    q223_aug == 1 ~ "Live birth",
    q223_aug == 2 ~ "Stillbirth",
    q223_aug == 3 ~ "Miscarriage",
    q223_aug == 4 ~ "Abortion",
    TRUE ~ NA_character_),
    q223_aug = factor(
      q223_aug,
      levels = c(
        "Live birth",
        "Stillbirth",
        "Miscarriage",
        "Abortion")
    )
  ) %>%
  filter(q223_aug %in% c("Live birth", "Stillbirth")) %>%
  group_by(tips, q223_aug, event) %>%
  summarise(n = n()) %>%
  filter(!(q223_aug == "Live birth" & event == 0)) %>%
  select(-event) %>%
  pivot_wider(
    names_from = q223_aug,
    values_from = n
  )
# Calculate column totals (excluding the Total column)
col_totals <- colSums(tab4[ , -1], na.rm = TRUE)  # assuming dob_type is the first column
# Add percentage columns
tab4_pct <- tab4
for (col in names(col_totals)) {
  tab4_pct[[paste0(col, "_pct")]] <-  round(tab4[[col]] / col_totals[[col]] * 100)
}
# Add total row
total_row <- as.list(col_totals)
names(total_row) <- names(col_totals)
# Create 100.0% for each percentage column
for (col in names(col_totals)) {
  total_row[[paste0(col, "_pct")]] <- 100
}
total_row$tips <- "Total"
# Bind total row to the bottom
tab4_pct <- bind_rows(tab4_pct, total_row)

tab4_pct %>%
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14", ">15", "Total"))) %>%
  arrange(tips) %>%
  select(tips, `Live birth`, `Live birth_pct`, Stillbirth, Stillbirth_pct) %>% 
  kbl(format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
      format.args = list(big.mark = ",", scientific = FALSE), 
      col.names = c("Years prior to survey", "N", "%", "N", "%"), 
      align = c("l", "r", "l","r", "l"),
      caption = "Number of reported deaths and stillbirths in REACH-Mali FPH")  %>%
  #kable_styling(font_size = 8) %>%
  add_header_above(c(" " = 1, 
                     "Deaths" = 2, 
                     "Stillbirths" = 2))


# Distribution of AOD -----------------------------------------------------

# Main plot
main_plot <- dat %>%  
  filter(event == 1) %>%
  ggplot() +
  geom_histogram(aes(aadd), fill = "#0D0887FF", bins = 500) +
  scale_x_continuous(
    breaks = c(0, 30.5*6, 365.25, 365.25*2, 365.25*3, 365.25*4, 365.25*5),
    labels = c(0, 0.5, 1:5)
  ) +
  coord_cartesian(xlim = c(0, 365.25 * 5)) +
  labs(y = "n", x = "Years", title = "Age of death") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
# Inset plot: zoom on 0–365 days
inset_plot <- dat %>%  
  filter(event == 1) %>%
  filter(aadd <= 31) %>%
  ggplot() +
  geom_histogram(aes(aadd), fill = "#0D0887FF", bins = 31) +
  labs(y = "", x = "Days") +
  scale_x_continuous(breaks = seq(0, 28, 7)) +
  theme_minimal(base_size = 8) +
  theme(
    #axis.text = element_text(size = 6),
    #axis.title.y = element_blank(),
    plot.background = element_rect(color = "black", fill = "white", linewidth = 0.3)
  )
# Combine with inset in upper right
p <- ggdraw() +
  draw_plot(main_plot) +
  draw_plot(inset_plot, x = 0.53, y = 0.45, width = 0.45, height = 0.45) # with title x = 0.53, y = 0.46

ggsave("./gen/fph/figures/aod.png", p, dpi = 300, width = 6, height = 3)

# AOD heaping plots ----------------------------------------------------------

p1 <- dat %>%  
  filter(event == 1 & tips != ">15") %>%
  mutate(tips = factor(tips, levels =  c("0-4", "5-9", "10-14"))) %>%
  mutate(barfill = ifelse(aadd == 7, "a", "b")) %>%
  filter(aadd >= 0 & aadd <= 14) %>% 
  group_by(barfill, tips, aadd) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = aadd, y = n), stat = "identity", fill = "#0D0887FF") + #  fill = barfill
  #geom_text(data = reldif7d, aes(x = 11, y = 235, label = sprintf("%0.1f",round(heap, 1)))) +
  labs(y = "n", x = "Days", title = "Age of death heaping - 7 days") +
  facet_wrap(~tips, nrow=1) +
  #scale_fill_manual(values = c("#DE4968",  "#0D0887FF")) +
  scale_x_continuous(breaks = c(0, 7, 14)) +
  guides(fill="none") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/fph/figures/heaping-aod-7d.png", p1, dpi = 300, width = 6, height = 3)

# heaping of age at death at 12 months, measured by the relative
# difference of the number of deaths at 12 months from the average for months 10-14, multiplied by 100.
p2 <- dat %>% 
  filter(event == 1 & tips != ">15") %>%
  mutate(tips = factor(tips, levels =  c("0-4", "5-9", "10-14"))) %>%
  filter(aadm >= 6 & aadm <= 18) %>% 
  mutate(barfill = ifelse(aadm == 12, "a", "b")) %>%
  group_by(barfill,tips, aadm) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = aadm, y = n), stat = "identity",  fill = "#0D0887FF") + # , fill = barfill
  #geom_text(data = reldif12m, aes(x = 16, y = 215, label = sprintf("%0.1f",round(heap, 1)))) +
  labs(y = "n", x = "Months", title = "Age of death heaping - 12 months") +
  facet_wrap(~tips, nrow=1) +
  #scale_fill_manual(values = c("#DE4968",  "#0D0887FF")) +
  scale_x_continuous(breaks = c(6, 12, 18)) +
  guides(fill="none") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/fph/figures/heaping-aod-12m.png", p2, dpi = 300, width = 6, height = 3)

# Sex ratios ---------------------------------------------------------------

# Observed sex ratio at birth (expected = 1.03)
# negative deviation means fewer boys than expected
# positive deviation means fewer girls than expected
dat %>%
  mutate(q219 = ifelse(q219 == 1, "Garçon", "Fille")) %>%
  group_by(tips, q219) %>%
  summarise(n = n()) %>%
  pivot_wider(
    id_cols = tips,
    names_from = q219,
    values_from = n
  ) %>%
  mutate(SRB = sprintf("%0.2f",round(Garçon/Fille,2))) %>% 
  mutate(tips = factor(tips, levels = c("0-4", "5-9", "10-14", ">15"))) %>%
  arrange(tips) %>%
  kbl(format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
      col.names = c("Years prior to survey", "Female", "Male", "Missing", "SRB"),
      caption = "Sex ratio at birth in REACH-Mali FPH.")

         
# Percent of deaths that are neonatal -------------------------------------

dat %>%
  filter(event == 1 & aadm < 12) %>%
  mutate(tips = cut(q220y, breaks = c(-Inf, 2025-15, 2025-10, 2025-5, 2026), 
                    labels = c("<2010", "[2010, 2014)", "[2015, 2020)", "[2020, 2025]")),
         isneo = ifelse(aadd <= 28, "neo", "post")) %>%
  group_by(tips, isneo) %>%
  summarise(n = n()) %>%
  pivot_wider(
    id_cols = tips,
    names_from = isneo,
    values_from = n
  ) %>%
  mutate(pneo = sprintf("%0.2f",round(neo/(neo+post)*100, 2)))

# calculate expected from Hill and Choi formula: b0 + b1*log(IMR)
# (intercept 1.37 and slope -.214)


# Heaping in DOB
# Drop imputed dobs
dobcomp <- subset(dat, q220m_imp_ind == 0 & q220d_imp_ind == 0)

# Pregnancy outcome reporting ---------------------------------------------

tab <- dat %>%
  mutate(q223_aug = case_when(
    q223_aug == 1 ~ "Live birth",
    q223_aug == 2 ~ "Stillbirth",
    q223_aug == 3 ~ "Miscarriage",
    q223_aug == 4 ~ "Abortion",
    TRUE ~ NA_character_),
    q223_aug = factor(
      q223_aug,
      levels = c(
        "Live birth",
        "Stillbirth",
        "Miscarriage",
        "Abortion")
    )
  ) %>%
  group_by(q223_aug) %>%
  summarise(n=n())
total <- sum(tab$n)
tab <- rbind(tab, data.frame(q223_aug = "Total", n = total))
tab$per <- sprintf("%0.1f",round(tab$n/total*100, 1))

dat %>%
  group_by(q223_aug) %>%
  summarise(n=n()) %>%
  filter(is.na(q223_aug))  # 289
317 - 289

kbl(tab, format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
    format.args = list(big.mark = ",", scientific = FALSE), 
    #col.names = c("Résultat de la grossesse", "N", "%"), 
    col.names = c("Pregnancy outcome", "N", "%"), 
    caption = "Reporting completeness for pregnancy outcomes")

# Pregnancy outcomes by age distribution of mother ------------------------

p <- dat %>%
  mutate(q223_aug = case_when(
    q223_aug == 1 ~ "Live birth",
    q223_aug == 2 ~ "Stillbirth",
    q223_aug == 3 ~ "Miscarriage",
    q223_aug == 4 ~ "Abortion",
    TRUE ~ NA_character_),
    q223_aug = factor(
      q223_aug,
      levels = c(
        "Live birth",
        "Stillbirth",
        "Miscarriage",
        "Abortion")
    )
  ) %>%
  group_by(agecat_resp, q223_aug) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=q223_aug, y = n, fill = agecat_resp), position = "dodge", stat = "identity") +
  scale_fill_viridis_d(option = "C", name = "Âge")  +
  labs(
    #title = "Résultat de la grossesse selon l’âge des enquêtées",
    #x = "Résultat de la grossesse",
    title = "Pregnancy outcome by age of respondent",
    x = "Pregnancy outcome",
    y = "n"
  ) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        axis.text.x = element_text(angle = 30, hjust = .75))
ggsave("./gen/fph/figures/pregout-byage.png", p, dpi = 300, width = 5, height = 3)

# Births by year of birth -------------------------------------------------

p <- dat %>%
  mutate(yob = floor(dob_dec)) %>%
  group_by(yob) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = yob, y = n), stat = "identity", fill = "#0D0887FF") +
  labs(y = "n", x = "Year", title = "Year of birth") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/fph/figures/births-yob.png", p, dpi = 300, width = 3, height = 2)

# Deaths by year of death -------------------------------------------------

p <- dat %>%
  filter(event == 1) %>%
  mutate(yod = year(dod)) %>%
  group_by(yod) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = yod, y = n), stat = "identity", fill = "#0D0887FF") +
  labs(y = "n", x = "Year", title = "Year of death") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/fph/figures/deaths-yod.png", p, dpi = 300, width = 3, height = 2)


# Combined plots for births and deaths by year ----------------------------

# births
p1 <- dat %>%
  mutate(yob = floor(dob_dec)) %>%
  group_by(yob) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = yob, y = n), stat = "identity", fill = "#0D0887FF") +
  labs(y = "n", x = "Year of birth", title = "Births") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        legend.position = "bottom",
        #panel.border = element_blank(),
        #plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank())


# deaths
p2 <- dat %>%
  filter(event == 1) %>%
  mutate(yod = year(dod)) %>%
  group_by(yod) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = yod, y = n), stat = "identity", fill = "#0D0887FF") +
  labs(y = "n", x = "Year of death", title = "Deaths") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        #panel.border = element_blank(),
        #plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank())

# remove lines that appear between ggarrange
#p1 <- p1 + theme(plot.margin = margin(0, 0, 0, 0))
#p2 <- p2 + theme(plot.margin = margin(0, 0, 0, 0))

# Final combined plot
final_plot <- ggarrange(
  ggarrange(p1, p2, ncol = 2),
  nrow = 1,
  heights = c(1)
)

# Save the final figure
ggsave("./gen/fph/figures/fph-births-deaths-byyear.png",
       final_plot, dpi = 300, width = 6, height = 3)


# Births/deaths by years prior to survey Mali DHS ---------------------

p <- mldhs %>%
  filter(!is.na(yob)) %>%
  group_by(SurveyId, tips_yob) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = tips_yob, y = n), stat = "identity", fill = "#0D0887FF") +
  facet_wrap(~SurveyId) +
  labs(y = "n", x = "Year of birth", title = "Births") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        legend.box.background = element_blank(),
        legend.background = element_blank())
ggsave("./gen/fph/figures/mldhs-births-byyear.png", p, dpi = 300, width = 6, height = 4)

p <- mldhs %>%
  filter(!is.na(yod) & tips_yod <= 0) %>%
  group_by(SurveyId, tips_yod) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x = tips_yod, y = n), stat = "identity", fill = "#0D0887FF") +
  facet_wrap(~SurveyId) +
  labs(y = "n", x = "Year of death", title = "Deaths") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        legend.box.background = element_blank(),
        legend.background = element_blank())
ggsave("./gen/fph/figures/mldhs-deaths-byyear.png", p, dpi = 300, width = 6, height = 4)


# Agreement between SBH and FPH -------------------------------------------

plota <- aud4dat %>%
  filter(dif_bth != 0) %>%
  mutate(facet = "Live births") %>%
  rename(dif = dif_bth) %>%
  select(dif, facet)
plotb <- aud4dat %>%
  filter(dif_preg != 0) %>%
  mutate(facet = "Pregnancies") %>%
  rename(dif = dif_preg) %>%
  select(dif, facet)
plot <- rbind(plota, plotb)
p <- plot %>%
  ggplot() +
  geom_histogram(aes(x=dif), fill = "#0D0887FF") +
  facet_wrap(~facet) +
  labs(y = "n", x = "Difference, sbh minus fph", title = "Disagreement in SBH and FPH: live births, pregnancies") +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        legend.box.background = element_blank(),
        legend.background = element_blank())
ggsave("./gen/fph/figures/fph-sbh-bth-disagreement.png", p, dpi = 300, width = 6, height = 3)

p <- aud5dat %>%
  filter(dif_dth != 0) %>%
  ggplot() +
  geom_histogram(aes(x=dif_dth), fill = "#0D0887FF") +
  labs(y = "n", x = "Difference, sbh minus fph", title = "Disagreement in SBH and FPH: deaths") +
  scale_x_continuous(breaks = -5:5) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        legend.box.background = element_blank(),
        legend.background = element_blank())
ggsave("./gen/fph/figures/fph-sbh-dth-disagreement.png", p, dpi = 300, width = 3, height = 3)

# Table of agreement between SBH and FPH ----------------------------------

tabtotal <- data.frame(dif = "Total",
                       `n_lb` = unique(aud4tab$total),
                       `per_lb` = "100.0",
                       n_Pregnancies = unique(aud4tab$total),
                       per_Pregnancies = "100.0")
tabaud4 <- aud4tab %>%
  select(-total) %>%
  pivot_wider(
    id_cols = dif,
    names_from = var,
    values_from = c(n, per)
  ) %>%
  select(dif, `n_Live births`, `per_Live births`, n_Pregnancies, per_Pregnancies) %>%
  rename(n_lb = `n_Live births`, per_lb = `per_Live births`)
tabaud4 <- rbind(tabaud4, tabtotal)
tabaud4 %>%
  kbl(format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
    format.args = list(big.mark = ",", scientific = FALSE), 
    col.names = c("Difference (ref: sbh)", "N", "%", "N", "%"), 
    caption = "Agreement number of live births and pregnancies between SBH and FPH. Difference is calculated with respect to SBH. Negative values indicate more events reported in the FPH than SBH, positive values indicate the reverse. Denominator is all women who took part in the mortality survey. The FPH missing category does not include cases where 0 pregnancies/births were reported in SBH and thus a FPH was not conducted-- these cases were considered to agree.", label = "fph-sbh-bth-agreement") %>%
    add_header_above(c(" " = 1, 
                     "Live births" = 2, 
                     "Pregnancies" = 2))


tabtotal <- data.frame(dif = "Total",
                       n_Deaths = unique(aud5tab$total),
                       per_Deaths = "100.0")
tabaud5 <- aud5tab %>%
  select(-total) %>%
  pivot_wider(
    id_cols = dif,
    names_from = var,
    values_from = c(n, per)
  )
tabaud5 <- rbind(tabaud5, tabtotal)
tabaud5 <- rbind(tabaud5, tabtotal)
tabaud5 %>%
  kbl(format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
      format.args = list(big.mark = ",", scientific = FALSE), 
      col.names = c("Difference (ref: sbh)", "N", "%"), 
      caption = "Agreement number of deaths between SBH and FPH. Difference is calculated with respect to SBH. Negative values indicate more events reported in the FPH than SBH, positive values indicate the reverse. Denominator is all women who took part in the mortality survey. The FPH missing category does not include cases where 0 pregnancies/births were reported in SBH and thus a FPH was not conducted-- these cases were considered to agree.", label = "fph-sbh-dth-agreement") %>%
  add_header_above(c(" " = 1, 
                     "Deaths" = 2))