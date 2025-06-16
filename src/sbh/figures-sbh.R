################################################################################
#' @description 
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(viridis)
library(cowplot)
library(ggpubr)
#' Inputs
source("./src/utils.R")
dat <- readRDS("./gen/sbh/temp/sbh-qsecover-qwsec01.rds")
################################################################################


# Children born by respondent age -----------------------------------------

dat %>%
  group_by(agecat_resp, q208) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=q208, y = n), stat = "identity") +
  facet_wrap(~agecat_resp)

# Children born by age of mother ----------------------------------------------------

p <- dat %>%
  group_by(agecat_resp, q208) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=q208, y = n, fill = agecat_resp), position = "dodge", stat = "identity") +
  scale_fill_viridis_d(option = "C", name = "Âge")  +
  labs(
    title = "Nombre total d'enfants nés selon l’âge des enquêtées",
    x = "Nombre total d'enfants nés",
    y = "n"
  ) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/sbh/figures/sbh-children-born-byage.png", p, dpi = 300, width = 5, height = 3)

dat %>%
  group_by(agecat_resp, q212) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=q212, y = n, fill = agecat_resp), position = "dodge", stat = "identity") +
  scale_fill_viridis_d(option = "C", name = "Âge")  +
  labs(
    title = "Nombre total de grossesses selon l’âge des enquêtées",
    x = "Nombre total de grossesses",
    y = "n"
  ) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))

# Figure~\ref{fig:sbh-children-born-byage} shows the total number of children born by respondent age. In the summary birth history, this is a sum of reported children living with the respondent, living elsewhere, and deceased. It does not include pregnancy losses or stillbirths (unless reported as part of children deceased). The distribution looks reasonable with the vast majority of respondents aged 15-19 having only 1 birth, and the distributions gradually shifting rightward with age, reflecting higher parity among older respondents.


# Proportion of children ever born/died by age of mother ------------------

# q208 is total children born
# q205_comb is total living at home
# q205_comb is total living elsewhere
dat %>%
  filter(q208 != 0) %>%
  mutate(propsurv = (q203_comb + q205_comb)/q208) %>%
  group_by(agecat_resp) %>%
  summarise(avgsurv = sprintf("%0.1f", round(mean(propsurv)*100, 1))) %>%
  kbl(format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
      format.args = list(big.mark = ",", scientific = FALSE), 
      col.names = c("Mother age", "%"), 
      caption = "Average proportion children surviving by age of mother.") 

# q208 is total children born
# q207_comb is total died
dat %>%
  filter(q208 != 0) %>%
  mutate(propsurv = (q207_comb)/q208) %>%
  group_by(agecat_resp) %>%
  summarise(avgsurv = sprintf("%0.1f", round(mean(propsurv)*100, 1))) %>%
  kbl(format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
      format.args = list(big.mark = ",", scientific = FALSE), 
      col.names = c("Mother age", "%"), 
      label = c("sbh-propdied"),
      caption = "Average proportion children died by age of mother.") 

# histogram
dat %>%
  filter(q208 != 0) %>%
  mutate(propdied = q207_comb / q208) %>%
  filter(propdied <= 1) %>%
  ggplot(aes(x = propdied)) +
  geom_histogram(fill = "#0D0887FF") +
  facet_wrap(~agecat_resp) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))
#ggsave("./gen/fph/figures/cebcd-matage.png", p, dpi = 300, width = 6, height = 6)

# density plot
dat %>%
  filter(q208 != 0) %>%
  mutate(propdied = q207_comb / q208) %>%
  filter(propdied <= 1) %>%
  ggplot(aes(x = propdied)) +
  geom_density(fill = "#0D0887FF") +
  facet_wrap(~agecat_resp, scales = "free_y") +
  labs(x = "", y = "Density") +
  theme_minimal()

# Combined plot for pregnancies by age of mother ------------------------------------

# Bar plot with legend
p1_full <- dat %>%
  group_by(agecat_resp, q212) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot() +
  geom_bar(aes(x = q212, y = n, fill = agecat_resp),
           position = "dodge", stat = "identity") +
  scale_fill_viridis_d(option = "C", name = "Âge") +
  labs(
    x = "Nombre total de grossesses",
    y = "n"
  ) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        legend.position = "bottom",
        #panel.border = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank()) +
  guides(fill = guide_legend(nrow = 1))

# Extract and wrap legend for ggarrange
legend <- ggpubr::as_ggplot(ggpubr::get_legend(p1_full)) +  
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )


# Remove legend from plots
p1 <- p1_full + theme(legend.position = "none",
                      #panel.border = element_blank(),
                      plot.margin = margin(0, 0, 0, 0),
                      legend.box.background = element_blank(),
                      legend.background = element_blank())

# Density plot
p2 <- dat %>%
  ggplot() +
  geom_density(aes(x = q212, color = agecat_resp), adjust = 4) +
  scale_color_viridis_d(option = "C", guide = "none") +
  labs(
    x = "Nombre total de grossesses",
    y = "Densité"
  ) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        #panel.border = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank())

# Title as a plot
title_plot <- as_ggplot(text_grob(
  "Nombre total de grossesses selon l’âge des enquêtées",
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
  ggarrange(p1, p2, ncol = 2),
  legend,
  nrow = 3,
  heights = c(0.1, 1, 0.15)
)

# Save the final figure
ggsave("./gen/sbh/figures/sbh-pregnancies-byage-combined.png",
       final_plot, dpi = 300, width = 6, height = 3)


# Combined plot for number of births by age of mother ---------------------




# Combined plot for children born by age ----------------------------------

# Bar plot with legend
p1_full <- dat %>%
  group_by(agecat_resp, q208) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot() +
  geom_bar(aes(x = q208, y = n, fill = agecat_resp),
           position = "dodge", stat = "identity") +
  scale_fill_viridis_d(option = "C", name = "Âge") +
  labs(
    x = "Nombre total d'enfants nés",
    y = "n"
  ) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        legend.position = "bottom",
        #panel.border = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank()) +
  guides(fill = guide_legend(nrow = 1))

# Extract and wrap legend for ggarrange
legend <- ggpubr::as_ggplot(ggpubr::get_legend(p1_full)) +  
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )


# Remove legend from plots
p1 <- p1_full + theme(legend.position = "none",
                      #panel.border = element_blank(),
                      plot.margin = margin(0, 0, 0, 0),
                      legend.box.background = element_blank(),
                      legend.background = element_blank())

# Density plot
p2 <- dat %>%
  ggplot() +
  geom_density(aes(x = q208, color = agecat_resp), adjust = 4) +
  scale_color_viridis_d(option = "C", guide = "none") +
  labs(
    x = "Nombre total d'enfants nés",
    y = "Densité"
  ) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        #panel.border = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank())

# Title as a plot
title_plot <- as_ggplot(text_grob(
  "Nombre total d'enfants nés selon l’âge des enquêtées",
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
  ggarrange(p1, p2, ncol = 2),
  legend,
  nrow = 3,
  heights = c(0.1, 1, 0.15)
)

# Save the final figure
ggsave("./gen/sbh/figures/sbh-children-born-byage-combined.png",
       final_plot, dpi = 300, width = 6, height = 3)

# Combined plot for children died by age ----------------------------------


# Bar plot with legend
p1_full <- dat %>%
  filter(!is.na(q207_comb) & q207_comb != 0) %>%
  group_by(agecat_resp, q207_comb) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=q207_comb, y = n, fill = agecat_resp), position = "dodge", stat = "identity") +
  scale_fill_viridis_d(option = "C", name = "Âge")  +
  scale_x_continuous(breaks = 1:10) +
  labs(
    #title = "Nombre total d'enfants décédés selon l’âge des enquêtées",
    subtitle = "a",
    x = "Nombre total d'enfants décédés",
    y = "n"
  ) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        legend.position = "bottom",
        #panel.border = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank()) +
  guides(fill = guide_legend(nrow = 1))

# Extract and wrap legend for ggarrange
legend <- ggpubr::as_ggplot(ggpubr::get_legend(p1_full)) +  
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    plot.margin = margin(0, 0, 0, 0),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )


# Remove legend from plots
p1 <- p1_full + theme(legend.position = "none",
                      #panel.border = element_blank(),
                      plot.margin = margin(0, 0, 0, 0),
                      legend.box.background = element_blank(),
                      legend.background = element_blank())

# Truncating plot because these high numbers don't show up in bars
dat %>%
  filter(!is.na(q211) & q211 != 0) %>%
  group_by(agecat_resp, q211) %>%
  summarise(n = n()) %>%
  filter(q211 > 6) %>%
  group_by(q211) %>%
  summarise(sum(n))

# other bar plot
p2 <- dat %>%
  filter(!is.na(q211) & q211 != 0) %>%
  group_by(agecat_resp, q211) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_bar(aes(x=q211, y = n, fill = agecat_resp), position=position_dodge(preserve = "single"), stat = "identity") +
  scale_fill_viridis_d(option = "C", name = "Âge")  +
  scale_x_continuous(breaks = 1:10) +
  labs(
    #title = "Nombre de naissances non vivantes selon l’âge des enquêtées",
    subtitle = "b",
    x = "Nombre de naissances non vivantes",
    y = "n"
  ) +
  coord_cartesian(x = c(0.75, 6.25)) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        #panel.border = element_blank(),
        plot.margin = margin(0, 0, 0, 0),
        legend.position = "none",
        legend.box.background = element_blank(),
        legend.background = element_blank())

# Title as a plot
title_plot <- as_ggplot(text_grob(
  "Nombre total d'enfants (a) décédés et (b) naissances non vivantes selon l’âge des enquêtées",
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
  ggarrange(p1, p2, ncol = 2),
  legend,
  nrow = 3,
  heights = c(0.1, 1, 0.15)
)

# Save the final figure
ggsave("./gen/sbh/figures/sbh-children-died-pregloss-combined.png",
       final_plot, dpi = 300, width = 6, height = 3)

