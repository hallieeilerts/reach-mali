
'~~~~~~~~~~~~~~~~~~~~~~~
  Project Name: REACH
  Program Name: REACH Data Import.R
  Programmer: Jason Bailey
  Date: 16FEB2025
  Purpose: Pull in REACH Data from INSTAT CS Pro database
           
    
  Edited by:
  Edited date:
  Current Version: 0.1
  Other necessary files: none

  Output files: none
  ~~~~~~~~~~~~~~~~~~~~~~~'

rm(list = ls())

library(DBI)
library(RMySQL)  # Use RMariaDB if necessary
library(ggplot2)
#library(ggmap)
library("sf")
library(dplyr)
library(lubridate)
library("leaflet")
library("leaftime")
library("leaflet.extras")
library("leaflet.extras2")
library("geojsonsf")
library("writexl")
library("knitr")
library("officer")
library("magrittr")
library("forcats")
library("stringr")
library("gt")
library("kableExtra")
library("flextable")
library("scales")
library("readxl")
library("foreign")
library(tidyr)
library("RMariaDB")

mypath <- "C:/Users/HEilerts/Institute of International Programs Dropbox/Hallie Eilerts-Spinelli/REACH/reach-mali/data/"
username <- "uom_user"
pword <- "6VQ6T7^L#V$$ig"

#REACH VIEWS (NOT RAW DATA)
# Establish connection
db <- dbConnect(
  RMySQL::MySQL(),
  dbname = "reach",
  host = "db.reach.instat.ml",  # Usually "localhost" or an IP
  port = 3307,  # Change if needed
  user = username,
  password = pword
)

# Test connection
dbListTables(db)

knitr::opts_chunk$set(connection = db, max.print = 30)
reach_tables_list <- dbGetQuery(db, "show tables")

# Get the list of table names from the database
reach_tables_list <- dbGetQuery(db, "SHOW TABLES") 

table_names <- reach_tables_list[[1]]  

reach_tables <- list()

reach_accueil_vue  <- dbGetQuery(db,
                                 "SELECT * FROM accueil_vue") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_accueil_vue downloaded")
print(Sys.time()) 

reach_qwsec01_vue  <- dbGetQuery(db,
                                 "SELECT * FROM qwsec01_vue") %>% rename_with(tolower)%>%
  rename_with(~ gsub("-", "_", .x))
print("reach_qwsec01_vue downloaded")
print(Sys.time())

reach_qwsec2a_vue  <- dbGetQuery(db,
                                 "SELECT * FROM qwsec2a_vue") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_qwsec2a_vue downloaded")
print(Sys.time())

reach_qwsec6a_vue  <- dbGetQuery(db,
                                 "SELECT * FROM qwsec6a_vue") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_qwsec6a_vue downloaded")
print(Sys.time())

reach_qwsec2b_vue  <- dbGetQuery(db,
                                 "SELECT * FROM qwsec2b_vue") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_qwsec2b_vue downloaded")
print(Sys.time())

reach_naiss_enfant_vue  <- dbGetQuery(db,
                                      "SELECT * FROM naiss_enfant_vue") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_naiss_enfant_vue downloaded")
print(Sys.time())

reach_traitement_surv_vue  <- dbGetQuery(db,
                                         "SELECT * FROM traitement_surv_vue") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_traitement_surv_vue downloaded")
print(Sys.time())

#WRite to data folder for dashboard
write.csv(reach_accueil_vue, paste0(mypath, "reach_accueil_vue.csv"),row.names=F)
write.csv(reach_qwsec01_vue, paste0(mypath, "reach_qwsec01_vue.csv"),row.names=F)
write.csv(reach_qwsec2a_vue, paste0(mypath, "reach_qwsec2a_vue.csv"),row.names=F)
write.csv(reach_qwsec2b_vue, paste0(mypath, "reach_qwsec2b_vue.csv"),row.names=F)
write.csv(reach_qwsec6a_vue, paste0(mypath, "reach_qwsec6a_vue.csv"),row.names=F)
write.csv(reach_naiss_enfant_vue, paste0(mypath, "reach_naiss_enfant_vue.csv"),row.names=F)
write.csv(reach_traitement_surv_vue, paste0(mypath, "reach_traitement_surv_vue.csv"),row.names=F)

# concessions forms -------------------------------------------------------

#REACH CONCESSIONS FORMS
# Establish connection
db <- dbConnect(
  RMySQL::MySQL(),
  dbname = "reach_concessions",
  host = "db.reach.instat.ml",  # Usually "localhost" or an IP
  port = 3307,  # Change if needed
  user = username,
  password = pword
)

# Test connection
dbListTables(db)

knitr::opts_chunk$set(connection = db, max.print = 30)
reach_tables_list <- dbGetQuery(db, 
                                "show tables")


# Get the list of table names from the database
reach_tables_list <- dbGetQuery(db, "SHOW TABLES") 

table_names <- reach_tables_list[[1]]  

reach_tables <- list()

reach_concessions_level1  <- dbGetQuery(db, "SELECT * FROM `level-1`") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_conc_level1 downloaded")
print(Sys.time())

reach_concessions_str_rec  <- dbGetQuery(db, "SELECT * FROM str_rec") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_conc_str_rec downloaded")
print(Sys.time())

reach_concessions_str_gps  <- dbGetQuery(db, "SELECT * FROM str_gps") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_conc_str_gps downloaded")
print(Sys.time())

write.csv(reach_concessions_level1, paste0(mypath, "reach_concessions_level1.csv"),row.names=F)
write.csv(reach_concessions_str_rec, paste0(mypath, "reach_concessions_str_rec.csv"),row.names=F)
write.csv(reach_concessions_str_gps, paste0(mypath, "reach_concessions_str_gps.csv"),row.names=F)


# household forms ---------------------------------------------------------

#REACH HOUSEHOLD FORMS
# Establish connection
db <- dbConnect(
  RMySQL::MySQL(),
  dbname = "reach_hh",
  host = "db.reach.instat.ml",  # Usually "localhost" or an IP
  port = 3307,  # Change if needed
  user = username,
  password = pword
)

knitr::opts_chunk$set(connection = db, max.print = 30)
reach_tables_list <- dbGetQuery(db, 
                                "show tables")

# Get the list of table names from the database
reach_tables_list <- dbGetQuery(db, "SHOW TABLES") 

table_names <- reach_tables_list[[1]]  

reach_tables <- list()

reach_hh_level1  <- dbGetQuery(db, "SELECT * FROM `level-1`") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_hh_level1 downloaded")
print(Sys.time())

reach_hh_identification_rec <- dbGetQuery(db, "SELECT * FROM identification_rec") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_hh_identification_rec downloaded")
print(Sys.time())

reach_hh_liste_menage <- dbGetQuery(db, "SELECT * FROM liste_menage") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_hh_liste_menage downloaded")
print(Sys.time())

reach_hh_naiss_enfants_1_59_mois <- dbGetQuery(db, "SELECT * FROM naiss_enfants_1_59_mois") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_hh_naiss_enfants_1_59_mois downloaded")
print(Sys.time())

reach_hh_deces_enfants_1_59_mois <- dbGetQuery(db, "SELECT * FROM deces_enfants_1_59_mois") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_hh_deces_enfants_1_59_mois downloaded")
print(Sys.time())

reach_hh_traitement_surveillance_enf <- dbGetQuery(db, "SELECT * FROM traitement_surveillance_enf") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_hh_traitement_surveillance_enf downloaded")
print(Sys.time())

write.csv(reach_hh_level1, paste0(mypath, "reach_hh_level1.csv"),row.names=F)
write.csv(reach_hh_identification_rec, paste0(mypath, "reach_hh_identification_rec.csv"),row.names=F)
write.csv(reach_hh_liste_menage, paste0(mypath, "reach_hh_liste_menage.csv"),row.names=F)
write.csv(reach_hh_naiss_enfants_1_59_mois, paste0(mypath, "reach_hh_naiss_enfants_1_59_mois.csv"),row.names=F)
write.csv(reach_hh_deces_enfants_1_59_mois, paste0(mypath, "reach_hh_deces_enfants_1_59_mois.csv"),row.names=F)
write.csv(reach_hh_traitement_surveillance_enf, paste0(mypath, "reach_hh_traitement_surveillance_enf.csv"),row.names=F)

# menages -----------------------------------------------------------------

# Establish connection
db <- dbConnect(
  RMySQL::MySQL(),
  dbname = "reach_menages",
  host = "db.reach.instat.ml",  # Usually "localhost" or an IP
  port = 3307,  # Change if needed
  user = username,
  password = pword
)

knitr::opts_chunk$set(connection = db, max.print = 30)
reach_tables_list <- dbGetQuery(db, 
                                "show tables")

# Get the list of table names from the database
reach_tables_list <- dbGetQuery(db, "SHOW TABLES") 

table_names <- reach_tables_list[[1]]  

reach_tables <- list()

reach_menages_level1  <- dbGetQuery(db, "SELECT * FROM `level-1`") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_menages_level1 downloaded")
print(Sys.time())

reach_menages_hh_rec <- dbGetQuery(db, "SELECT * FROM hh_rec") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_menages_rec downloaded")
print(Sys.time())

reach_menages_hh_gps <- dbGetQuery(db, "SELECT * FROM hh_gps") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_menages_gps downloaded")
print(Sys.time())

write.csv(reach_menages_level1, paste0(mypath, "reach_menages_level1.csv"), row.names=F)
write.csv(reach_menages_hh_rec, paste0(mypath, "reach_menages_hh_rec.csv"), row.names=F)
write.csv(reach_menages_hh_gps, paste0(mypath, "reach_menages_hh_gps.csv"), row.names=F)


# mortalite-menages -------------------------------------------------------

# Establish connection
db <- dbConnect(
  RMySQL::MySQL(),
  dbname = "mortalite_menage",
  host = "db.reach.instat.ml",  # Usually "localhost" or an IP
  port = 3307,  # Change if needed
  user = username,
  password = pword
)


knitr::opts_chunk$set(connection = db, max.print = 30)
reach_tables_list <- dbGetQuery(db, 
                                "show tables")

# Get the list of table names from the database
reach_tables_list <- dbGetQuery(db, "SHOW TABLES") 

table_names <- reach_tables_list[[1]]  

reach_tables <- list()

reach_mortalite_menage_level1  <- dbGetQuery(db, "SELECT * FROM `level-1`") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_menage_level1 downloaded")
print(Sys.time())

reach_mortalite_menage_hh_gps  <- dbGetQuery(db, "SELECT * FROM hh_gps") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_menage_hh_gps downloaded")
print(Sys.time())

reach_mortalite_menage_hh_rec  <- dbGetQuery(db, "SELECT * FROM hh_rec") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_menage_hh_rec downloaded")
print(Sys.time())

reach_mortalite_menage_qhsec01x  <- dbGetQuery(db, "SELECT * FROM qhsec01x") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_menage_qhsec01x downloaded")
print(Sys.time())

reach_mortalite_menage_section_ce  <- dbGetQuery(db, "SELECT * FROM section_ce") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_menage_section_ce downloaded")
print(Sys.time())

reach_mortalite_menage_section_de  <- dbGetQuery(db, "SELECT * FROM section_de") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_menage_section_de downloaded")
print(Sys.time())


write.csv(reach_mortalite_menage_level1, paste0(mypath, "reach_mortalite_menage_level1.csv"), row.names=F)
write.csv(reach_mortalite_menage_hh_gps, paste0(mypath, "reach_mortalite_menage_hh_gps.csv"), row.names=F)
write.csv(reach_mortalite_menage_hh_rec, paste0(mypath, "reach_mortalite_menage_hh_rec.csv"), row.names=F)
write.csv(reach_mortalite_menage_qhsec01x, paste0(mypath, "reach_mortalite_menage_qhsec01x.csv"), row.names=F)
write.csv(reach_mortalite_menage_section_ce, paste0(mypath, "reach_mortalite_menage_section_ce.csv"), row.names=F)
write.csv(reach_mortalite_menage_section_de, paste0(mypath, "reach_mortalite_menage_section_de.csv"), row.names=F)


# mortalite-femmes --------------------------------------------------------

# Establish connection
db <- dbConnect(
  RMySQL::MySQL(),
  dbname = "mortalite_femmes",
  host = "db.reach.instat.ml",  # Usually "localhost" or an IP
  port = 3307,  # Change if needed
  user = username,
  password = pword
)


knitr::opts_chunk$set(connection = db, max.print = 30)
reach_tables_list <- dbGetQuery(db, 
                                "show tables")

# Get the list of table names from the database
reach_tables_list <- dbGetQuery(db, "SHOW TABLES") 

table_names <- reach_tables_list[[1]]  

reach_tables <- list()

reach_mortalite_femme_level1  <- dbGetQuery(db, "SELECT * FROM `level-1`") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_femme_level1 downloaded")
print(Sys.time())

reach_mortalite_femme_qsecover <- dbGetQuery(db,  "SELECT * FROM qsecover") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_femme_qsecover downloaded")
print(Sys.time())

reach_mortalite_femme_qwsec01 <- dbGetQuery(db, "SELECT * FROM qwsec01") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_femme_qwsec01 downloaded")
print(Sys.time())

reach_mortalite_femme_qwsec2a <- dbGetQuery(db, "SELECT * FROM qwsec2a") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_femme_qwsec2a downloaded")
print(Sys.time())

reach_mortalite_femme_qwsec2b <- dbGetQuery(db, "SELECT * FROM qwsec2b") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_femme_qwsec2b downloaded")
print(Sys.time())

reach_mortalite_femme_qwsec2c <- dbGetQuery(db, "SELECT * FROM qwsec2c") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_femme_qwsec2c downloaded")
print(Sys.time())

reach_mortalite_femme_qwsec6a <- dbGetQuery(db, "SELECT * FROM qwsec6a") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_femme_qwsec6a downloaded")
print(Sys.time())

reach_mortalite_femme_qwsec11 <- dbGetQuery(db, "SELECT * FROM qwsec11") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_femme_qwsec11 downloaded")
print(Sys.time())

reach_mortalite_femme_qwsec2y <- dbGetQuery(db, "SELECT * FROM qwsec2y") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_femme_qwsec2y downloaded")
print(Sys.time())

write.csv(reach_mortalite_femme_level1, paste0(mypath, "reach_mortalite_femme_level1.csv"), row.names=F)
write.csv(reach_mortalite_femme_qsecover, paste0(mypath, "reach_mortalite_femme_qsecover.csv"), row.names=F)
write.csv(reach_mortalite_femme_qwsec01, paste0(mypath, "reach_mortalite_femme_qwsec01.csv"), row.names=F)
write.csv(reach_mortalite_femme_qwsec2a, paste0(mypath, "reach_mortalite_femme_qwsec2a.csv"), row.names=F)
write.csv(reach_mortalite_femme_qwsec2b, paste0(mypath, "reach_mortalite_femme_qwsec2b.csv"), row.names=F)
write.csv(reach_mortalite_femme_qwsec2c, paste0(mypath, "reach_mortalite_femme_qwsec2c.csv"), row.names=F)
write.csv(reach_mortalite_femme_qwsec6a, paste0(mypath, "reach_mortalite_femme_qwsec6a.csv"), row.names=F)
write.csv(reach_mortalite_femme_qwsec11, paste0(mypath, "reach_mortalite_femme_qwsec11.csv"), row.names=F)
write.csv(reach_mortalite_femme_qwsec2y, paste0(mypath, "reach_mortalite_femme_qwsec2y.csv"), row.names=F)

# mortalite-denomb --------------------------------------------------------

#REACH HOUSEHOLD FORMS
# Establish connection
db <- dbConnect(
  RMySQL::MySQL(),
  dbname = "mortalite_denomb",
  host = "db.reach.instat.ml",  # Usually "localhost" or an IP
  port = 3307,  # Change if needed
  user = username,
  password = pword
)

knitr::opts_chunk$set(connection = db, max.print = 30)
reach_tables_list <- dbGetQuery(db, 
                                "show tables")

# Get the list of table names from the database
reach_tables_list <- dbGetQuery(db, "SHOW TABLES") 

table_names <- reach_tables_list[[1]]  

reach_tables <- list()

reach_mortalite_denomb_level1 <- dbGetQuery(db, "SELECT * FROM `level-1`") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_denomb_level1 downloaded")
print(Sys.time())


reach_mortalite_denomb_qhhsecover <- dbGetQuery(db, "SELECT * FROM qhhsecover") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_denomb_qhhsecover downloaded")
print(Sys.time())

reach_mortalite_denomb_qhsec01xx <- dbGetQuery(db, "SELECT * FROM qhsec01xx") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_denomb_qhsec01xx downloaded")
print(Sys.time())

reach_mortalite_denomb_section_che <- dbGetQuery(db, "SELECT * FROM section_che") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_denomb_section_che downloaded")
print(Sys.time())

reach_mortalite_denomb_section_ct <- dbGetQuery(db, "SELECT * FROM section_ct") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_denomb_section_ct downloaded")
print(Sys.time())

write.csv(reach_mortalite_denomb_level1, paste0(mypath, "reach_mortalite_denomb_level1.csv"), row.names=F)
write.csv(reach_mortalite_denomb_qhhsecover, paste0(mypath, "reach_mortalite_denomb_qhhsecover.csv"), row.names=F)
write.csv(reach_mortalite_denomb_qhsec01xx, paste0(mypath, "reach_mortalite_denomb_qhsec01xx.csv"), row.names=F)
write.csv(reach_mortalite_denomb_section_che, paste0(mypath, "reach_mortalite_denomb_section_che.csv"), row.names=F)
write.csv(reach_mortalite_denomb_section_ct, paste0(mypath, "reach_mortalite_denomb_section_ct.csv"), row.names=F)


# mortalite-concession --------------------------------------------------

#REACH HOUSEHOLD FORMS
# Establish connection
db <- dbConnect(
  RMySQL::MySQL(),
  dbname = "mortalite_concession",
  host = "db.reach.instat.ml",  # Usually "localhost" or an IP
  port = 3307,  # Change if needed
  user = username,
  password = pword
)


knitr::opts_chunk$set(connection = db, max.print = 30)
reach_tables_list <- dbGetQuery(db, 
                                "show tables")

# Get the list of table names from the database
reach_tables_list <- dbGetQuery(db, "SHOW TABLES") 

table_names <- reach_tables_list[[1]]  

reach_tables <- list()

reach_mortalite_conc_level1 <- dbGetQuery(db, "SELECT * FROM `level-1`") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_conc_level1 downloaded")
print(Sys.time())

reach_mortalite_conc_str_gps <- dbGetQuery(db, "SELECT * FROM str_gps") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_conc_str_gps downloaded")
print(Sys.time())

reach_mortalite_conc_str_rec <- dbGetQuery(db, "SELECT * FROM str_rec") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_conc_str_rec downloaded")
print(Sys.time())

reach_mortalite_conc_cases <- dbGetQuery(db, "SELECT * FROM cases") %>% rename_with(tolower) %>%
  rename_with(~ gsub("-", "_", .x))
print("reach_mortalite_conc_cases downloaded")
print(Sys.time())

write.csv(reach_mortalite_conc_level1, paste0(mypath, "reach_mortalite_conc_level1.csv"), row.names=F)
write.csv(reach_mortalite_conc_str_gps, paste0(mypath, "reach_mortalite_conc_str_gps.csv"), row.names=F)
write.csv(reach_mortalite_conc_str_rec, paste0(mypath, "reach_mortalite_conc_str_rec.csv"), row.names=F)
write.csv(reach_mortalite_conc_cases, paste0(mypath, "reach_mortalite_conc_cases.csv"), row.names=F)

