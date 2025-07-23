################################################################################
#' @description Download dhs datasets using rdhs
#' @return Separate rds files for individual, birth, and pregnancy recode modules
################################################################################
#' Clear environment
rm(list=ls())
#' Libraries
library(rdhs)
#' Inputs
## Set credentials
set_rdhs_config(email = "hallieeilerts@gmail.com",
                password_prompt =  TRUE,
                project = "maternal denominators", 
                config_path = "rdhs.json",
                global=FALSE)
## Permissions
Sys.setenv("rdhs_RENVIRON_PERMISSION"=1)
## return API requests as data.table rather than data.frame.
Sys.setenv(rdhs_DATA_TABLE = "TRUE")
################################################################################

# Individual recode -------------------------------------------------------

# Information on available surveys
surveys <- dhs_surveys(surveyYearStart=1980)

# Individual recode metadata
ir_meta <- dhs_datasets(fileType = "IR", fileFormat = "flat")

# Exclude subnational surveys from India because don't have permission to download them via rdhs
ir_meta$subnational <- ifelse(substr(ir_meta$SurveyId,1,2) != substr(ir_meta$FileName,1,2), TRUE, FALSE)
ir_meta <- subset(ir_meta, subnational == FALSE)

# Exclude subnational surveys that conflict with national names
ir_meta <- subset(ir_meta, !(FileName %in% c("KEIR03FL.ZIP", "SNIR02FL.ZIP")) & CountryName == "Mali")

# Identify modules of interest corresponding to surveys of interest
ird <- ir_meta[which(ir_meta$SurveyId %in% surveys$SurveyId),]

# Limit to surveys which have IR module
surveys_ir <- dhs_surveys(surveyYearStart=1980)
surveys_ir <- subset(surveys_ir, SurveyId %in% ird$SurveyId)

# Load all of the datasets into R as a list
ird$path <- unlist(get_datasets(ird$FileName))
ir <- list()
for(survid in ird$SurveyId){
  print(survid)
  dat <- readRDS(ird[which(ird$SurveyId == survid),]$path)
  dat <- dat[grep("caseid|^v0|^v1|^v2|awfactt|^vcal_1|^b|^mm", names(dat))]
  ir[[survid]] <- dat
}
ir <- Map(data.frame,
          SurveyId = surveys_ir$SurveyId,
          CountryName = surveys_ir$CountryName,
          SurveyYear = surveys_ir$SurveyYear,
          ir)
saveRDS(ir, file = "./data/ir-ml.rds")
rm(ir)
