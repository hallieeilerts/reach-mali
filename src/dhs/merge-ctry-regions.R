################################################################################
#' @description Merge on country codes and regions
#' @return 
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(dplyr)
library(tidyr)
library(countrycode)
#' Inputs
## Analytical sample
sample <- read.csv("./gen/dhs/temp/surveys.csv")
## DHS country codes and ISO3
dhs_cc <- read.csv("./data/dhs/dhs-country-codes.csv")
## Regions
key_region_u20_IGME <- read.csv("./data/keys/RegionClassIGME_20210407.csv")
################################################################################

# Sample
sample$country_code <- substr(sample$SurveyId, 1, 2)

# Merge on ISO3 codes
dat <- merge(sample, dhs_cc, by = "country_code", all = TRUE)

# Add country name
dat$country_name <- countrycode(dat$iso3, origin = "iso3c", destination = "country.name")

# Process regions
key <- key_region_u20_IGME
key <- key[, names(key) %in% c("ISO3Code", "UNICEFReportRegion1", "UNICEFReportRegion2")]
names(key)[names(key) == "ISO3Code"] <- "iso3"
key$Region <- key$UNICEFReportRegion1
# If report region 2 is not missing, use it instead
key$Region[which(key$UNICEFReportRegion2 != "")] <- key$UNICEFReportRegion2[which(key$UNICEFReportRegion2 != "")]

# Merge on regions
dat <- merge(dat, key, by = "iso3", all.x = TRUE)

nrow(subset(dat, is.na(UNICEFReportRegion1))) # 0
nrow(subset(dat, is.na(UNICEFReportRegion2))) # 0
nrow(subset(dat, is.na(Region))) # 0

# Save output(s) ----------------------------------------------------------

write.csv(dat, "./gen/dhs/output/surveys-ctry-regions.csv", row.names = FALSE)


