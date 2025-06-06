################################################################################
#' @description Figure to visualize tips
#' @return 
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(ggplot2)
#' Inputs
source("./src/utils.R")
################################################################################


# Define interview date
interview_date <- ymd("2025-05-01")

# Create periods
periods <- data.frame(
  period = factor(c("0–4 years before interview", "5–9 years before interview", "10–14 years before interview"),
                  levels = c("0–4 years before interview", "5–9 years before interview", "10–14 years before interview")),
  start = c(interview_date - years(5), interview_date - years(10), interview_date - years(15)),
  end   = c(interview_date, interview_date - years(5), interview_date - years(10))
)

# Yearly breaks
year_lines <- seq(from = ymd("2010-01-01"), to = ymd("2025-01-01"), by = "1 year")

# Define exact breakpoints: May 1 markers from 2010 to 2025
may_dates <- seq(from = interview_date - years(15), to = interview_date, by = "1 year")

# Years before survey labels: 0 at 2025-05-01, ..., 15 at 2010-05-01
label_data <- data.frame(
  date = rev(may_dates),  # Start from 2025 backward
  years_before = 0:15
)


# Plot
p <- ggplot() +
  # Colored bands for each period
  geom_rect(data = periods, aes(xmin = start, xmax = end, ymin = 0, ymax = 1, fill = period), alpha = 0.6) +
  geom_vline(xintercept = as.numeric(year_lines), color = "grey30", size = 0.3, linetype = "dotted") +
  # Vertical lines for April markers
  geom_vline(data = label_data, aes(xintercept = as.numeric(date)), color = "white", size = 0.5) +
  # Main survey interview marker
  geom_vline(xintercept = as.numeric(interview_date), color = "red", linetype = "dashed", size = 1) +
  # Labels inside bars
  geom_text(data = label_data, aes(x = date, y = 0.5, label = years_before), 
            size = 3, color = "black", fontface = "bold", vjust = 0.5, hjust = 0.5) +
  annotate("text", x = interview_date, y = 1.1, label = "Interview date", hjust = 1, color = "red") +
  # Axis formatting
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
  scale_y_continuous(limits = c(0, 1.1)) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Year", y = NULL, fill = "Reference\nPeriod",
       title = "Retrospective Reference Periods in Survey Estimates",
       subtitle = "Each period spans 5 years prior to the interview date in ~May 2025") +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "top", 
        text = element_text(size = 10), title = element_text(size = 8))
ggsave("./gen/dhs/output/tips-figure.png", p, dpi = 300, width = 6, height = 3)
