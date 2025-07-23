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
library(ggpubr)
library(pammtools) # geom_stepribbon
library(stringr) # str_wrap
#' Inputs
gap <- readRDS("./gen/mort/output/gapu5m-for-plots-ci.rds")
reach <- readRDS("./gen/mort/output/reach-rates-ci.rds")
################################################################################


# Combined table ----------------------------------------------------------

# all
reach_all <- reach %>%
  filter(type == "All" & cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(cut_time = factor(cut_time, levels = c("0-4", "5-9", "10-14")),
         agegrp = factor(agegrp, levels = c("Neonatal", "Postneonatal", "Under5"),
                         labels = c("Neonatal", "Postneonatal", "Under-5"))) %>%
  select(cut_time, agegrp, qx, qx_lower, qx_upper) %>% #  events, pyears, mx,
  mutate(qx = sprintf("%0.1f",round(qx * 1000, 1)),
         qx_lower = sprintf("%0.1f",round(qx_lower * 1000, 1)),
         qx_upper = sprintf("%0.1f",round(qx_upper * 1000, 1)),
         ci = paste0("(", qx_lower, ", ", qx_upper, ")")) %>%
  select(cut_time, agegrp, qx, ci) %>%
  pivot_longer(cols = c(qx, ci), names_to = "stat", values_to = "value") %>%
  unite(temp_col, agegrp, cut_time, sep = "_") %>%
  pivot_wider(names_from = temp_col, values_from = value) %>%
  mutate(type = "All",
         byvar = "All") %>%
  relocate(byvar, everything())

col_order <- expand.grid(
  agegrp = c("Neonatal", "Postneonatal", "Under-5"),
  cut_time = c("0-4", "5-9", "10-14")
) %>%
  mutate(name = paste(agegrp, cut_time, sep = "_")) %>%
  pull(name)
col_order <- c("type", "byvar", col_order)

# region
reach_reg <- reach %>%
  filter(type %in% c("Region") & cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(cut_time = factor(cut_time, levels = c("0-4", "5-9", "10-14")),
         agegrp = factor(agegrp, levels = c("Neonatal", "Postneonatal", "Under5"),
                         labels = c("Neonatal", "Postneonatal", "Under-5"))) %>%
  select(byvar, cut_time, agegrp, qx, qx_lower, qx_upper) %>% #  events, pyears, mx,
  arrange(byvar, cut_time, agegrp) %>%
  mutate(qx = sprintf("%0.1f",round(qx * 1000, 1)),
         qx_lower = sprintf("%0.1f",round(qx_lower * 1000, 1)),
         qx_upper = sprintf("%0.1f",round(qx_upper * 1000, 1)),
         ci = paste0("(", qx_lower, ", ", qx_upper, ")")) %>% 
  select(byvar, cut_time, agegrp, qx, ci) %>%
  pivot_longer(cols = c(qx, ci), names_to = "stat", values_to = "value") %>%
  unite(temp_col, agegrp, cut_time, sep = "_") %>%
  pivot_wider(names_from = temp_col, values_from = value) %>%
  mutate(type = "Region")

# strata
reach_strat <- reach %>%
  filter(type %in% c("Strata") & cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(cut_time = factor(cut_time, levels = c("0-4", "5-9", "10-14")),
         agegrp = factor(agegrp, levels = c("Neonatal", "Postneonatal", "Under5"),
                         labels = c("Neonatal", "Postneonatal", "Under-5")),
         byvar = case_when(
           byvar == "Urbain" ~ "Urban",
           byvar == 1 ~ "Regional capitals",
           byvar == 2 ~ "Small towns",
           byvar == 3 ~ "Rural w <40\\% living >5km from CSCOM",
           byvar == 4 ~ "Rural w >40\\% living >5km from CSCOM",
           TRUE ~ byvar
         )) %>%
  select(byvar, cut_time, agegrp, qx, qx_lower, qx_upper) %>% #  events, pyears, mx,
  arrange(byvar, cut_time, agegrp) %>%
  mutate(qx = sprintf("%0.1f",round(qx * 1000, 1)),
         qx_lower = sprintf("%0.1f",round(qx_lower * 1000, 1)),
         qx_upper = sprintf("%0.1f",round(qx_upper * 1000, 1)),
         ci = paste0("(", qx_lower, ", ", qx_upper, ")")) %>% 
  select(byvar, cut_time, agegrp, qx, ci) %>%
  pivot_longer(cols = c(qx, ci), names_to = "stat", values_to = "value") %>%
  unite(temp_col, agegrp, cut_time, sep = "_") %>%
  pivot_wider(names_from = temp_col, values_from = value) %>%
  mutate(type = "Strata")

# domain
reach_dom <- reach %>%
  filter(type %in% c("Area") & cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(cut_time = factor(cut_time, levels = c("0-4", "5-9", "10-14")),
         agegrp = factor(agegrp, levels = c("Neonatal", "Postneonatal", "Under5"),
                         labels = c("Neonatal", "Postneonatal", "Under-5")),
         byvar = gsub("Zones de comparaison", "Comparison", byvar),
         byvar = gsub("Zones du programme", "Treatment", byvar)) %>%
  select(byvar, cut_time, agegrp, qx, qx_lower, qx_upper) %>% #  events, pyears, mx,
  arrange(byvar, cut_time, agegrp) %>%
  mutate(qx = sprintf("%0.1f",round(qx * 1000, 1)),
         qx_lower = sprintf("%0.1f",round(qx_lower * 1000, 1)),
         qx_upper = sprintf("%0.1f",round(qx_upper * 1000, 1)),
         ci = paste0("(", qx_lower, ", ", qx_upper, ")")) %>% 
  select(byvar, cut_time, agegrp, qx, ci) %>%
  pivot_longer(cols = c(qx, ci), names_to = "stat", values_to = "value") %>%
  unite(temp_col, agegrp, cut_time, sep = "_") %>%
  pivot_wider(names_from = temp_col, values_from = value) %>%
  mutate(type = "Domain")

reach_comb <- reach_all %>%
  bind_rows(reach_reg) %>%
  bind_rows(reach_strat) %>%
  bind_rows(reach_dom) %>% 
  select(c(stat,all_of(col_order)))
reach_comb2 <- reach_comb %>%
  select(byvar, "Neonatal_0-4", "Neonatal_5-9", "Neonatal_10-14",
         "Postneonatal_0-4", "Postneonatal_5-9", "Postneonatal_10-14",
         "Under-5_0-4", "Under-5_5-9", "Under-5_10-14")
names(reach_comb2) <- c("byvar",rep(c("0-4", "5-9", "10-14"), times = 3))
reach_comb2 %>%
  kbl(booktabs = TRUE, row.names = FALSE, linesep = "", format = "latex",
      format.args = list(big.mark = ",", scientific = FALSE), 
      label = c("mortality"),
      caption = "Summary statistics for mortality.") %>%
  collapse_rows(1) %>%
  add_header_above(c(" " = 1, "NMR - q(28d)" =3, "PNMR - q(28d, 59m)" = 3, "U5MR - q(60m)" = 3)) %>%
  pack_rows(index = table(fct_inorder(reach_comb$type))) 


# Combined Word table -----------------------------------------------------

# Step 1: Prepare wide table with unique column names
reach_wide <- reach_comb %>%
  pivot_longer(cols = -c(stat, type, byvar), names_to = "group_age", values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  pivot_longer(cols = c(qx, ci), names_to = "stat", values_to = "value") %>%
  mutate(stat = factor(stat, levels = c("qx", "ci"))) %>%
  arrange(type, byvar, stat) %>%
  pivot_wider(names_from = group_age, values_from = value)

# Step 2: Keep column names unique
# No renaming to "0-4", "5-9", etc. — just keep full variable names like "Neonatal_0-4"

# Step 3: Create header mappings
pretty_labels <- c(
  "Neonatal_0-4" = "0-4", "Neonatal_5-9" = "5-9", "Neonatal_10-14" = "10-14",
  "Postneonatal_0-4" = "0-4", "Postneonatal_5-9" = "5-9", "Postneonatal_10-14" = "10-14",
  "Under-5_0-4" = "0-4", "Under-5_5-9" = "5-9", "Under-5_10-14" = "10-14"
)

main_header <- c("type", "byvar", 
                 rep("NMR - q(28d)", 3),
                 rep("PNMR - q(28d, 59m)", 3),
                 rep("U5MR - q(60m)", 3))

reach_wide <- reach_wide[,c("type", "byvar", "stat", "Neonatal_0-4", "Neonatal_5-9", "Neonatal_10-14",
              "Postneonatal_0-4", "Postneonatal_5-9", "Postneonatal_10-14",
              "Under-5_0-4", "Under-5_5-9", "Under-5_10-14")]

# Step 4: Build flextable
ft <- flextable(reach_wide) %>%
  set_header_labels(values = c(
    type = " ",
    byvar = "byvar"
  )) %>%
  merge_v(j = ~type + byvar) %>%
  valign(j = ~type + byvar, valign = "top") %>%
  align(align = "center", part = "all") %>%
  autofit() %>%
  theme_booktabs() %>%
  set_caption("Summary statistics for fertility in the Mali DHS and REACH-Mali by region.")

doc <- read_docx()
doc <- body_add_flextable(doc, ft)
print(doc, target = "fertility_table.docx")

# Table country-level mortality estimates ------------------------------------------

# All
reach %>%
  filter(type == "All" & cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(cut_time = factor(cut_time, levels = c("0-4", "5-9", "10-14")),
         agegrp = factor(agegrp, levels = c("Neonatal", "Postneonatal", "Under5"),
                         labels = c("Neonatal", "Postneonatal", "Under-5"))) %>%
  select(cut_time, agegrp, qx, qx_lower, qx_upper) %>% #  events, pyears, mx,
  arrange(cut_time, agegrp) %>% 
  mutate(qx = sprintf("%0.1f",round(qx * 1000, 1)),
         qx_lower = sprintf("%0.1f",round(qx_lower * 1000, 1)),
         qx_upper = sprintf("%0.1f",round(qx_upper * 1000, 1)),
         ci = paste0("(", qx_lower, ", ", qx_upper, ")")) %>% 
  select(cut_time, agegrp, qx, ci) %>%
  kbl(format = "latex", booktabs = TRUE, row.names = FALSE, linesep = "",
      format.args = list(big.mark = ",", scientific = FALSE), 
      escape = FALSE,
      col.names = c("Years prior to survey", "Age",  "$q(x)$", "95\\% CI"), # "Deaths", "Person-years", "$m_x$",
      label = "mortrates",
      caption = "Mortality rates ($q(x)$) for neonatal, postneonatal, and under-5 age groups, expressed per 1,000 live births.") %>%
  collapse_rows(columns = 1, valign = "middle") %>%
  footnote(
    general = "CI - confidence interval",
    threeparttable = TRUE
  )

# Table all other levels --------------------------------------------------

reach %>%
  filter(!(type %in% c("All", "Region-Area")) & cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(cut_time = factor(cut_time, levels = c("0-4", "5-9", "10-14")),
         agegrp = factor(agegrp, levels = c("Neonatal", "Postneonatal", "Under5"),
                         labels = c("Neonatal", "Postneonatal", "Under-5")),
         type = case_when(
           type == "Area" ~ "Domain",
           type == "Region-Area" ~ "Region-Domain",
           TRUE ~ type
         ),
         byvar = case_when(
            byvar == "Urbain" ~ "Urban",
            byvar == 1 ~ "Regional capitals",
            byvar == 2 ~ "Small towns",
            byvar == 3 ~ "Rural w <40% living >5km from CSCOM",
            byvar == 4 ~ "Rural w >40% living >5km from CSCOM",
            TRUE ~ byvar
          ),
         byvar = gsub("Zones de comparaison", "Comparison", byvar),
         byvar = gsub("Zones du programme", "Treatment", byvar),
         type = factor(type, levels = c("Residence", "Region", "Strata", "Domain"))) %>%
  select(type, byvar, cut_time, agegrp, qx, qx_lower, qx_upper) %>% #  events, pyears, mx,
  arrange(type, byvar, cut_time, agegrp) %>%
  mutate(qx = sprintf("%0.1f",round(qx * 1000, 1)),
         qx_lower = sprintf("%0.1f",round(qx_lower * 1000, 1)),
         qx_upper = sprintf("%0.1f",round(qx_upper * 1000, 1)),
         ci = paste0("(", qx_lower, ", ", qx_upper, ")")) %>% 
  select(type, byvar, cut_time, agegrp, qx, ci) %>% 
  kbl(
    format = "latex",
    label = "mortrate-allsubgroup",
    booktabs = TRUE,
    row.names = FALSE,
    #escape = FALSE,
    linesep = "",
    col.names = c("", "", "Years prior to survey", "Age", "$q(x)$", "95\\% CI"),
    longtable = TRUE, 
    threeparttable = TRUE, 
    caption = "Mortality estimates for residence, region, strata, and domains."
  ) %>%
  column_spec(2, width = "10em") %>%
  collapse_rows(columns = 1:3, valign = "middle",longtable_clean_cut = FALSE) %>%
  kable_styling(latex_options = c("repeat_header")) %>%
  footnote(
    general = "CI - confidence interval",
    footnote_as_chunk = TRUE
  )


# Table residence ---------------------------------------------------------

reach %>%
  filter(type %in% c("Residence") & cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(cut_time = factor(cut_time, levels = c("0-4", "5-9", "10-14")),
         agegrp = factor(agegrp, levels = c("Neonatal", "Postneonatal", "Under5"),
                         labels = c("Neonatal", "Postneonatal", "Under-5")),
         type = case_when(
           type == "Area" ~ "Domain",
           type == "Region-Area" ~ "Region-Domain",
           TRUE ~ type
         ),
         byvar = case_when(
           byvar == "Urbain" ~ "Urban",
           byvar == 1 ~ "Regional capitals",
           byvar == 2 ~ "Small towns",
           byvar == 3 ~ "Rural w <40% living >5km from CSCOM",
           byvar == 4 ~ "Rural w >40% living >5km from CSCOM",
           TRUE ~ byvar
         ),
         byvar = gsub("Zones de comparaison", "Comparison", byvar),
         byvar = gsub("Zones du programme", "Treatment", byvar)) %>%
  select(byvar, cut_time, agegrp, qx, qx_lower, qx_upper) %>% #  events, pyears, mx,
  arrange(byvar, cut_time, agegrp) %>%
  mutate(qx = sprintf("%0.1f",round(qx * 1000, 1)),
         qx_lower = sprintf("%0.1f",round(qx_lower * 1000, 1)),
         qx_upper = sprintf("%0.1f",round(qx_upper * 1000, 1)),
         ci = paste0("(", qx_lower, ", ", qx_upper, ")")) %>% 
  select(byvar, cut_time, agegrp, qx, ci) %>% 
  kbl(
    format = "latex",
    label = "mortrate-res",
    booktabs = TRUE,
    row.names = FALSE,
    escape = FALSE,
    linesep = "",
    col.names = c("", "Years prior to survey", "Age", "$q(x)$", "95\\% CI"),
    longtable = TRUE, 
    threeparttable = TRUE, 
    caption = "Mortality estimates for residence."
  ) %>%
  column_spec(1, width = "10em") %>%
  collapse_rows(columns = 1:2, valign = "middle",longtable_clean_cut = FALSE) %>%
  kable_styling(latex_options = c("repeat_header")) %>%
  footnote(
    general = "CI - confidence interval",
    footnote_as_chunk = TRUE
  )


# Table region ------------------------------------------------------------

reach %>%
  filter(type %in% c("Region") & cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(cut_time = factor(cut_time, levels = c("0-4", "5-9", "10-14")),
         agegrp = factor(agegrp, levels = c("Neonatal", "Postneonatal", "Under5"),
                         labels = c("Neonatal", "Postneonatal", "Under-5")),
         type = case_when(
           type == "Area" ~ "Domain",
           type == "Region-Area" ~ "Region-Domain",
           TRUE ~ type
         ),
         byvar = case_when(
           byvar == "Urbain" ~ "Urban",
           byvar == 1 ~ "Regional capitals",
           byvar == 2 ~ "Small towns",
           byvar == 3 ~ "Rural w <40% living >5km from CSCOM",
           byvar == 4 ~ "Rural w >40% living >5km from CSCOM",
           TRUE ~ byvar
         ),
         byvar = gsub("Zones de comparaison", "Comparison", byvar),
         byvar = gsub("Zones du programme", "Treatment", byvar)) %>%
  select(byvar, cut_time, agegrp, qx, qx_lower, qx_upper) %>% #  events, pyears, mx,
  arrange(byvar, cut_time, agegrp) %>%
  mutate(qx = sprintf("%0.1f",round(qx * 1000, 1)),
         qx_lower = sprintf("%0.1f",round(qx_lower * 1000, 1)),
         qx_upper = sprintf("%0.1f",round(qx_upper * 1000, 1)),
         ci = paste0("(", qx_lower, ", ", qx_upper, ")")) %>% 
  select(byvar, cut_time, agegrp, qx, ci) %>% 
  kbl(
    format = "latex",
    label = "mortrate-reg",
    booktabs = TRUE,
    row.names = FALSE,
    escape = FALSE,
    linesep = "",
    col.names = c("", "Years prior to survey", "Age", "$q(x)$", "95\\% CI"),
    longtable = TRUE, 
    threeparttable = TRUE, 
    caption = "Mortality estimates for region."
  ) %>%
  column_spec(1, width = "10em") %>%
  collapse_rows(columns = 1:2, valign = "middle",longtable_clean_cut = FALSE) %>%
  kable_styling(latex_options = c("repeat_header")) %>%
  footnote(
    general = "CI - confidence interval",
    footnote_as_chunk = TRUE
  )


# Table strata ------------------------------------------------------------

reach %>%
  filter(type %in% c("Strata") & cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(cut_time = factor(cut_time, levels = c("0-4", "5-9", "10-14")),
         agegrp = factor(agegrp, levels = c("Neonatal", "Postneonatal", "Under5"),
                         labels = c("Neonatal", "Postneonatal", "Under-5")),
         type = case_when(
           type == "Area" ~ "Domain",
           type == "Region-Area" ~ "Region-Domain",
           TRUE ~ type
         ),
         byvar = case_when(
           byvar == "Urbain" ~ "Urban",
           byvar == 1 ~ "Regional capitals",
           byvar == 2 ~ "Small towns",
           byvar == 3 ~ "Rural w <40\\% living >5km from CSCOM",
           byvar == 4 ~ "Rural w >40\\% living >5km from CSCOM",
           TRUE ~ byvar
         ),
         byvar = gsub("Zones de comparaison", "Comparison", byvar),
         byvar = gsub("Zones du programme", "Treatment", byvar)) %>%
  select(byvar, cut_time, agegrp, qx, qx_lower, qx_upper) %>% #  events, pyears, mx,
  arrange(byvar, cut_time, agegrp) %>%
  mutate(qx = sprintf("%0.1f",round(qx * 1000, 1)),
         qx_lower = sprintf("%0.1f",round(qx_lower * 1000, 1)),
         qx_upper = sprintf("%0.1f",round(qx_upper * 1000, 1)),
         ci = paste0("(", qx_lower, ", ", qx_upper, ")")) %>% 
  select(byvar, cut_time, agegrp, qx, ci) %>% 
  kbl(
    format = "latex",
    label = "mortrate-strata",
    booktabs = TRUE,
    row.names = FALSE,
    escape = FALSE,
    linesep = "",
    col.names = c("", "Years prior to survey", "Age", "$q(x)$", "95\\% CI"),
    longtable = TRUE, 
    threeparttable = TRUE, 
    caption = "Mortality estimates for strata."
  ) %>%
  column_spec(1, width = "10em") %>%
  collapse_rows(columns = 1:2, valign = "middle",longtable_clean_cut = FALSE) %>%
  kable_styling(latex_options = c("repeat_header")) %>%
  footnote(
    general = "CI - confidence interval",
    footnote_as_chunk = TRUE
  )

# Table domain ------------------------------------------------------------

reach %>%
  filter(type %in% c("Area") & cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(cut_time = factor(cut_time, levels = c("0-4", "5-9", "10-14")),
         agegrp = factor(agegrp, levels = c("Neonatal", "Postneonatal", "Under5"),
                         labels = c("Neonatal", "Postneonatal", "Under-5")),
         type = case_when(
           type == "Area" ~ "Domain",
           type == "Region-Area" ~ "Region-Domain",
           TRUE ~ type
         ),
         byvar = case_when(
           byvar == "Urbain" ~ "Urban",
           byvar == 1 ~ "Regional capitals",
           byvar == 2 ~ "Small towns",
           byvar == 3 ~ "Rural w <40% living >5km from CSCOM",
           byvar == 4 ~ "Rural w >40% living >5km from CSCOM",
           TRUE ~ byvar
         ),
         byvar = gsub("Zones de comparaison", "Comparison", byvar),
         byvar = gsub("Zones du programme", "Treatment", byvar)) %>%
  select(byvar, cut_time, agegrp, qx, qx_lower, qx_upper) %>% #  events, pyears, mx,
  arrange(byvar, cut_time, agegrp) %>%
  mutate(qx = sprintf("%0.1f",round(qx * 1000, 1)),
         qx_lower = sprintf("%0.1f",round(qx_lower * 1000, 1)),
         qx_upper = sprintf("%0.1f",round(qx_upper * 1000, 1)),
         ci = paste0("(", qx_lower, ", ", qx_upper, ")")) %>% 
  select(byvar, cut_time, agegrp, qx, ci) %>% 
  kbl(
    format = "latex",
    label = "mortrate-domain",
    booktabs = TRUE,
    row.names = FALSE,
    escape = FALSE,
    linesep = "",
    col.names = c("", "Years prior to survey", "Age", "$q(x)$", "95\\% CI"),
    longtable = TRUE, 
    threeparttable = TRUE, 
    caption = "Mortality estimates for domain."
  ) %>%
  column_spec(1, width = "10em") %>%
  collapse_rows(columns = 1:2, valign = "middle",longtable_clean_cut = FALSE) %>%
  kable_styling(latex_options = c("repeat_header")) %>%
  footnote(
    general = "CI - confidence interval",
    footnote_as_chunk = TRUE
  )



# Qx - all ----------------------------------------------------------------

p <- gap %>%
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
ggsave("./gen/mort/figures/mort-all-qx.png", p, dpi = 300, width = 6, height = 3)

# Combined plot for mx and Qx ---------------------------------------------

# mx
p1 <- gap %>%
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

# qx
p2 <- gap %>%
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
  filter(type == "Residence" & cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14"))) %>%
  ggplot() +
  geom_step(aes(x = age_y, y = Qx*1000, color = byvar)) +
  geom_stepribbon(aes(x = age_y, ymin = Qx_lower*1000, ymax = Qx_upper*1000, fill = byvar), alpha = 0.2) +
  labs(x = "Age (years)", y = "Q(x)", title = "Cumulative probability of dying by residence") +
  scale_color_manual(values = c("#0D0887FF", "#D8576BFF"), name = "Residence") +
  scale_fill_manual(values = c("#0D0887FF", "#D8576BFF"), name = "Residence") +
  facet_wrap(~tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank())

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
  geom_stepribbon(aes(x = age_y, ymin = Qx_lower*1000, ymax = Qx_upper*1000, fill = byvar), alpha = 0.1) +
  scale_color_manual(values = c("#0D0887FF", "#7E03A8FF", "#CC4678FF", "#F89441FF", "#FDC926FF"), name = "Region") +
  scale_fill_manual(values = c("#0D0887FF", "#7E03A8FF", "#CC4678FF", "#F89441FF", "#FDC926FF"), name = "Region") +
  labs(x = "Age (years)", y = "Q(x)", title = "Cumulative probability of dying by region") +
  facet_wrap(~tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8))

ggsave("./gen/mort/figures/mort-reg-qx.png", p, dpi = 300, width = 6, height = 3)

# Strata ------------------------------------------------------------------

# extracting colors so i can replace the light yellow with a slightly darker yellow
viridis_colors <- viridisLite::viridis(n = 5, option = "plasma")
print(viridis_colors)
show_col(viridis_colors)
# ordering regions by mortality rate in legend
order_byvar <- gap %>%
  filter(type == "Strata", cut_time == "0-4") %>%
  group_by(byvar) %>%
  summarize(max_Qx = max(Qx), .groups = "drop") %>%
  arrange(desc(max_Qx)) %>%
  pull(byvar)

p <- gap %>%
  filter(type == "Strata" &
           cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14")),
         #byvar = factor(byvar, levels = order_byvar),
         byvar = factor(byvar, levels = c(4,3,2,1),
                        labels = c("Rural w >40% living >5km from CSCOM",
                                   "Rural w <40% living >5km from CSCOM",
                                   "Small towns",
                                   "Regional capitals"))) %>%
  ggplot() +
  geom_step(aes(x = age_y, y = Qx*1000, color = byvar)) +
  geom_stepribbon(aes(x = age_y, ymin = Qx_lower*1000, ymax = Qx_upper*1000, fill = byvar), alpha = 0.2) +
  #scale_color_manual(values = c("#0D0887FF", "#7E03A8FF", "#CC4678FF", "#F89441FF", "#FDC926FF"), name = "Region") + # 
  #scale_color_manual(values = c("#0D0887FF",  "#CC4678FF", "#F89441FF", "#FDC926FF"), name = "Region") + 
  #scale_fill_manual(values = c("#0D0887FF", "#CC4678FF", "#F89441FF", "#FDC926FF"), name = "Region") +
  scale_color_manual(values = c("#0D0887FF",  "#7E03A8FF", "#CC4678FF", "#F89441FF"), name = "Strata",
                     labels = function(x) str_wrap(x, width = 20)) + 
  scale_fill_manual(values = c("#0D0887FF", "#7E03A8FF", "#CC4678FF", "#F89441FF"), name = "Strata",
                    labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(breaks = c(0,25,50,75,100,125)) +
  labs(x = "Age (years)", y = "Q(x)", title = "Cumulative probability of dying by strata") +
  facet_wrap(~tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8)) 

ggsave("./gen/mort/figures/mort-strata-qx.png", p, dpi = 300, width = 6, height = 3)


# Area --------------------------------------------------------------------

p <- gap %>%
  filter(type == "Area" & cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14")),
         byvar = factor(byvar, levels = c("Zones du programme", "Zones de comparaison"),
                        labels = c("Treatment", "Comparison"))) %>%
  ggplot() +
  geom_step(aes(x = age_y, y = Qx*1000, color = byvar)) +
  geom_stepribbon(aes(x = age_y, ymin = Qx_lower*1000, ymax = Qx_upper*1000, fill = byvar), alpha = 0.2) +
  labs(x = "Age (years)", y = "Q(x)", title = "Cumulative probability of dying by domain") +
  scale_color_manual(values = c("#0D0887FF", "#D8576BFF"), name = "Domain") +
  scale_fill_manual(values = c("#0D0887FF", "#D8576BFF"), name = "Domain") +
  facet_wrap(~tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank())

ggsave("./gen/mort/figures/mort-area-qx.png", p, dpi = 300, width = 6, height = 3)


# Region-area -------------------------------------------------------------

p <- gap %>%
  filter(type == "Region-Area" & cut_time %in% c("0-4", "5-9", "10-14")) %>%
  mutate(tips = factor(cut_time, levels = c("0-4", "5-9", "10-14")),
         region = str_extract(byvar, "^[^-]+"),
         area = str_extract(byvar, "(?<=-).+$")) %>% 
  mutate(area = factor(area, levels = c(" Zones du programme", " Zones de comparaison"),
                       labels = c("Treatment", "Comparison"))) %>%
  ggplot() +
  geom_step(aes(x = age_y, y = Qx*1000, color = area)) +
  geom_stepribbon(aes(x = age_y, ymin = Qx_lower*1000, ymax = Qx_upper*1000, fill = area), alpha = 0.2) +
  labs(x = "Age (years)", y = "Q(x)", title = "Cumulative probability of dying by region and domain") +
  scale_color_manual(values = c("#0D0887FF", "#D8576BFF"), name = "Domain") +
  scale_fill_manual(values = c("#0D0887FF", "#D8576BFF"), name = "Domain") +
  facet_grid(region~tips) +
  theme_bw() +
  theme(text = element_text(size = 10), title = element_text(size = 8),
        plot.margin = margin(0, 0, 0, 0),
        legend.box.background = element_blank(),
        legend.background = element_blank())

ggsave("./gen/mort/figures/mort-regionarea-qx.png", p, dpi = 300, width = 6, height = 6)

