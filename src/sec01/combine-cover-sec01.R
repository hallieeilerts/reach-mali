################################################################################
#' @description Combine qsecover and qwsec01
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyr)
library(dplyr)
#' Inputs
qsecover <- readRDS("./gen/sec01/temp/qsecover-clean.rds")
qwsec01 <- readRDS("./gen/sec01/temp/qwsec01-age.rds")
################################################################################

head(qsecover)
head(qwsec01)
# There is qsecover_id and level_1_id in qsecover

# the id_data_cover variable in qwsec01 stays in sequence
# whereas qsecover level_1_id changes at 1000 to 1500
#qwsec01$id_data_cover[990:1020]
#qsecover$level_1_id[990:1020]
#qsecover$qsecover_id[990:1020]

# decision: merge on qsecover_id

dat <- merge(qsecover, qwsec01, by.x = "qsecover_id", by.y = "id_data_cover")
nrow(dat) == nrow(qsecover)
nrow(dat) == nrow(qwsec01)

# Save --------------------------------------------------------------------

saveRDS(dat, "./gen/sec01/output/qsecover-qwsec01.rds")
