################################################################################
#' @description
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(sf)
#' Inputs
qhhsecover <- read.csv("./data/reach_mortalite_denomb_qhhsecover.csv")
qhsec01 <- read.csv("./data/reach_mortalite_denomb_qhsec01xx.csv")
gps <- read.csv( "./data/reach_concessions_str_gps.csv")
mali2 <- st_read("./data/mli_adm_1m_dnct_2021_shp", 
                 layer = "mli_admbnda_adm2_1m_gov_20211110b")
mali3 <- st_read("./data/mli_adm_1m_dnct_2021_shp", 
                        layer = "mli_admbnda_adm3_1m_gov_20211110b")
################################################################################

dat_gps <- gps[, c("level_1_id","strlatitude","strlongitude")]

dat <- merge(qhhsecover[c("level_1_id", "qhhli1", "qhhli2", "qhhdistrict","qhhli3", "qhhi3", "qhhsecover_id")], 
             qhsec01, by="level_1_id", all.y=T)

dat <- merge(dat, dat_gps, by = "level_1_id", all.x = TRUE)


# most common lat/long of the qhhli3
mode_coord <- dat %>%
  select(qhhli3, strlatitude, strlongitude) %>%
  group_by(qhhli3, strlatitude, strlongitude) %>%
  filter(!is.na(strlatitude) & !is.na(strlongitude)) %>%
  group_by(qhhli3, strlatitude, strlongitude) %>%
  mutate(n = n()) %>% 
  arrange(qhhli3, -n) %>%
  group_by(qhhli3) %>%
  slice(1L) %>%
  rename(
    mode_lat = strlatitude,
    mode_long = strlongitude
  )

# merge on and replace missing values with most common for that qhhli3
dat <- dat %>%
  left_join(mode_coord, by = "qhhli3") %>%
  mutate(lat = ifelse(!is.na(strlatitude), strlatitude, mode_lat),
         lon = ifelse(!is.na(strlongitude), strlongitude, mode_long))

# lots of qhhli3 don't have any coordinates
nrow(subset(dat, is.na(lat) | is.na(lon)))   #  14919
nrow(subset(dat, !is.na(lat) & !is.na(lon))) # 93500

dat %>%
  group_by(qhhli3) %>%
  mutate(n = ifelse(!is.na(lat), 1, 0)) %>%
  summarise(sum(n))

# drop missing
dat <- subset(dat, !is.na(lat) & !is.na(lon))
points_sf <- st_as_sf(dat, coords = c("lon", "lat"), crs = 4326)

unique(mali3$admin3Name)
unique(dat$qhhli3)

ggplot() +
  geom_sf(data = mali2, fill = NA, color = "black") +
  geom_sf(data = points_sf, color = "red", size = 1) +
  theme_minimal()

ggplot() +
  geom_sf(data = mali3, fill = NA, color = "black")  +
  geom_sf(data = points_sf, color = "red", size = 1) +
  theme_minimal()

