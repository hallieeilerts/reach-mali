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
dat <- readRDS("./gen/fph/output/fph-tips.rds")
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


# Missing dob by lb and stb -----------------------------------------------

aud2 %>%
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
  ) %>%
  mutate(across(c(`Avortement`, `Fausse-couche`, `Mort né`, `Né vivant`, `NA`),
                ~ sprintf("%0.1f", round(.x / Total * 100, 1)), 
                .names = "{.col}_pct"))
 


# Missing dob by year -----------------------------------------------------

nrow(subset(aud2, is.na(q220y))) # 50
nrow(subset(aud2, !(q216 == "Né vivant"))) # 4823
nrow(subset(aud2, !(q216 == "Né vivant") & is.na(q220y))) # 1

p <- aud2 %>%
  # approximation of tips
  mutate(tips = cut(q220y, breaks = c(-Inf, 2025-15, 2025-10, 2025-5, 2026), 
                    labels = c(">15", "10-14", "5-9", "0-4"))) %>%
  # drops those where tips couldn't be calculated (missing year)
  filter(!is.na(tips)) %>%
  # drop non-live births 
  filter(q216 == "Né vivant") %>%
  mutate(tips =factor(tips, levels = c("0-4", "5-9", "10-14", ">15"))) %>%
  group_by(tips, dob_type) %>%
  summarise(n=n()) %>%
  group_by(tips) %>%
  mutate(total = sum(n),
         per = n/total*100) %>%
  mutate(dob_type = factor(dob_type, levels = c("complete", "missing_md", "missing_m", "missing_d"),
                           labels = c("Complete", "Missing month & day", "Missing month", "Missing day"))) %>%
  ggplot() +
  geom_bar(aes(x=dob_type, y = per, fill = dob_type), stat = "identity") +
  labs(x = "", y = "Percent", fill="DOB reporting", title = "DOB reporting completeness") +
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


# Impossible aad ----------------------------------------------------------

aud3

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
  geom_histogram(aes(aadd), fill = "#0D0887FF", bins = 31, fill = "grey50") +
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
  group_by(q223) %>%
  summarise(n=n())
total <- sum(tab$n)
tab <- rbind(tab, data.frame(q223 = "Total", n = total))
tab$per <- sprintf("%0.1f",round(tab$n/total*100, 1))

dat %>%
  group_by(q223_aug) %>%
  summarise(n=n()) %>%
  filter(is.na(q223_aug))  # 289
317 - 289

kbl(tab, format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
    format.args = list(big.mark = ",", scientific = FALSE), 
    col.names = c("Résultat de la grossesse", "N", "%"), 
    caption = "Reporting completeness for pregnancy outcomes")

# Pregnancy outcomes by age distribution of mother ------------------------

p <- dat %>%
  group_by(agecat_resp, q223_aug) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=q223_aug, y = n, fill = agecat_resp), position = "dodge", stat = "identity") +
  scale_fill_viridis_d(option = "C", name = "Âge")  +
  labs(
    title = "Résultat de la grossesse selon l’âge des enquêtées",
    x = "Résultat de la grossesse",
    y = "n"
  ) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        axis.text.x = element_text(angle = 30, hjust = .75))
ggsave("./gen/fph/figures/pregout-byage.png", p, dpi = 300, width = 5, height = 3)



