################################################################################
#' @description Figures for mortality estimates
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(viridis)
library(scales)
library(cowplot)
#' Inputs
gap <- readRDS("./gen/mort/output/gapu5m-for-plots.rds")
reach <- readRDS("./gen/mort/output/reach-rates.rds")
################################################################################


# Combined plot for rates and qx ------------------------------------------

gap %>%
  filter(type == "All" &
           cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14"))) %>%
  ggplot() +
  geom_step(aes(x = age_y, y = mx), color = "#0D0887FF") +
  labs(x = "Age (years)", y = "mx") +
  scale_y_log10() +
  facet_wrap(~tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))

gap %>%
  filter(type == "All" &
           cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14"))) %>%
  ggplot() +
  geom_step(aes(x = age_y, y = Qx*100), color = "#0D0887FF") +
  labs(x = "Age (years)", y = "Qx") +
  facet_wrap(~tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))

#ggsave("./gen/fph/figures/pregout-byage.png", p, dpi = 300, width = 5, height = 3)


# Faceted plot ------------------------------------------------------------

gap %>%
  filter(type == "All" &
           cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14")),
         Qx = Qx*100) %>%
  pivot_longer(
    cols = c(qx, mx),
    names_to = "mort",
    values_to = "value"
  ) %>%
  ggplot() +
  geom_step(aes(x = age_y, y = value), color = "#0D0887FF") +
  labs(x = "Age (years)", y = "") +
  facet_grid(mort~tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))


# Combined plot for mx and Qx ---------------------------------------------

# mx
p1 <- gap %>%
  filter(type == "All" &
           cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14"))) %>%
  ggplot() +
  geom_step(aes(x = age_y, y = mx*1000), color = "#0D0887FF") +
  labs(x = "", y = "mx (log scale)") +
  scale_y_log10() +
  facet_wrap(~tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        plot.margin = margin(0, 0, 0, 0),
        legend.position = "none",
        legend.box.background = element_blank(),
        legend.background = element_blank())

# qx
p2 <- gap %>%
  filter(type == "All" &
           cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14"))) %>%
  ggplot() +
  geom_step(aes(x = age_y, y = Qx*1000), color = "#0D0887FF") +
  labs(x = "Age (years)", y = "Q(x)") +
  facet_wrap(~tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        plot.margin = margin(0, 0, 0, 0),
        legend.position = "none",
        legend.box.background = element_blank(),
        legend.background = element_blank())

# Title as a plot
title_plot <- as_ggplot(text_grob(
  "Mortality estimates",
  size = 10, just = "center"
)) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )

# remove lines that appear between ggarrange
p1 <- p1 + theme(plot.margin = margin(0, 0, 0, 0))
p2 <- p2 + theme(plot.margin = margin(0, 0, 0, 0))

# Final combined plot
final_plot <- ggarrange(
  title_plot,
  ggarrange(p1, p2, ncol = 1),
  nrow = 3,
  heights = c(0.1, 1, 0.15)
)

# gets rid of thin grey line at bottom of plot
final_plot <- final_plot +
  theme(plot.background = element_rect(fill = "white", color = NA))

# Save the final figure
ggsave("./gen/mort/figures/mort-all-mx-qx.png",
       final_plot, dpi = 300, width = 6, height = 6)


# Residence ---------------------------------------------------------------

# Get 10-color palette
vir_colors <- viridis(10)
show_col(vir_colors)
show_col(viridis(10, option = "plasma"))

p <- gap %>%
  filter(type == "Residence" &
           cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14"))) %>%
  ggplot() +
  geom_step(aes(x = age_y, y = Qx*1000, color = byvar)) +
  scale_color_manual(values = c("#0D0887FF", "#D8576BFF"), name = "Residence") +
  labs(x = "Age (years)", y = "Q(x)") +
  facet_wrap(~tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))

ggsave("./gen/mort/figures/mort-res-qx.png", p, dpi = 300, width = 6, height = 3)

# Region ------------------------------------------------------------------

# extracting colors so i can replace the light yellow with a slightly darker yellow
viridis_colors <- viridisLite::viridis(n = 5, option = "plasma")
print(viridis_colors)
show_col(viridis_colors)
# ordering regions by mortality rate in legend
order_byvar <- gap %>%
  filter(type == "Region", cut_time == "0-4") %>%
  group_by(byvar) %>%
  summarize(max_Qx = max(Qx), .groups = "drop") %>%
  arrange(desc(max_Qx)) %>%
  pull(byvar)

p <- gap %>%
  filter(type == "Region" &
           cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14")),
         byvar = factor(byvar, levels = order_byvar)) %>%
  ggplot() +
  geom_step(aes(x = age_y, y = Qx*1000, color = byvar)) +
  scale_color_manual(values = c("#0D0887FF", "#7E03A8FF", "#CC4678FF", "#F89441FF", "#FDC926FF"), name = "Region") +
  labs(x = "Age (years)", y = "Q(x)") +
  facet_wrap(~tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))

ggsave("./gen/mort/figures/mort-reg-qx.png", p, dpi = 300, width = 6, height = 3)

# Table with mortality estimates ------------------------------------------

# All
reach %>%
  filter(cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(agegrp = factor(agegrp, levels = c("Neonatal", "1to59m", "Under5")),
         cut_time = factor(cut_time, levels = c("0-4", "5-9", "10-14")),
         type = factor(type, levels = c("All", "Residence", "Region")),
         agegrp = factor(agegrp, levels = c("Neonatal", "1to59m", "Under5"),
                         labels = c("Neonatal", "1-59m", "Under-5"))) %>%
  filter(type == "All") %>%
  select(cut_time, agegrp, events, pyears, mx, qx) %>%
  arrange(cut_time, agegrp) %>% 
  mutate(pyears = sprintf("%0.1f",round(pyears, 1)),
         mx = sprintf("%0.1f",round(mx * 1000, 1)),
         qx = sprintf("%0.1f",round(qx * 1000, 1))) %>% 
  kbl(format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
      format.args = list(big.mark = ",", scientific = FALSE), 
      longtable = TRUE, escape = FALSE,
      col.names = c("Years prior to survey", "Age", "Deaths", "Person-years", "$m_x$", "$q(x)$"),
      label = "mortrates",
      caption = "Mortality rates and probabilities of dying for neonatal, 1-59m and under-5 age groups, expressed per 1,000.") %>%
  collapse_rows(columns = 1, valign = "middle")

# Region and res
reach %>%
  filter(cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(agegrp = factor(agegrp, levels = c("Neonatal", "1to59m", "Under5")),
         cut_time = factor(cut_time, levels = c("0-4", "5-9", "10-14")),
         type = factor(type, levels = c("All", "Residence", "Region")),
         agegrp = factor(agegrp, levels = c("Neonatal", "1to59m", "Under5"),
                         labels = c("Neonatal", "1-59m", "Under-5"))) %>%
  filter(type != "All") %>%
  select(type, byvar, cut_time, agegrp, events, pyears, mx, qx) %>%
  arrange(type, byvar, cut_time, agegrp) %>% 
  mutate(pyears = sprintf("%0.1f",round(pyears, 1)),
         mx = sprintf("%0.1f",round(mx * 1000, 1)),
         qx = sprintf("%0.1f",round(qx * 1000, 1))) %>% 
  kbl(format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
      format.args = list(big.mark = ",", scientific = FALSE), 
      longtable = TRUE, escape = FALSE,
      label = "appendix-fph",
      col.names = c("Variable", "Value","Years prior to survey", "Age", "Deaths", "Person-years", "$m_x$", "$q(x)$"),
      caption = "Mortality rates and probabilities of dying for neonatal, 1-59m and under-5 age groups, expressed per 1,000.") %>%
  collapse_rows(columns = 1:3, valign = "middle")


# urban rural
subset(reach, cut_time == "0-4" & byvar == "Rural" & age_y == 0 & age_y_up == 5)$qx
subset(reach, cut_time == "0-4" & byvar == "Urbain" & age_y == 0 & age_y_up == 5)$qx

subset(reach, cut_time == "10-14" & byvar == "Rural" & age_y == 0 & age_y_up == 5)$qx
subset(reach, cut_time == "10-14" & byvar == "Urbain" & age_y == 0 & age_y_up == 5)$qx
