
'~~~~~~~~~~~~~~~~~~~~~~~
  Project Name: REACH
  Program Name: REACH Data Import.R
  Programmer: Jason Bailey
  Date: 16FEB2025
  Purpose: Pull in REACH Data from INSTAT CS Pro database
           
    
  Edited by:
  Edited date:
  Current Version: 0.1
  
  MUST HAVE USER NAME AND PASSWORD THIS WILL NOT RUN WITHOUT REPLACING
  
  Other necessary files: none
  
  Output files: none
  ~~~~~~~~~~~~~~~~~~~~~~~'

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

#REACH VIEWS (NOT RAW DATA)
# Establish connection 
#MUST HAVE USER NAME AND PASSWORD THIS WILL NOT RUN WITHOUT REPLACING
db <- dbConnect(
  RMySQL::MySQL(),
  dbname = "reach",
  host = "db.reach.instat.ml",  # Usually "localhost" or an IP
  port = 3307,  # Change if needed
  user = "##########",
  password = "##########"
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



reach_accueil_vue  <- dbGetQuery(db,
                                 "SELECT * FROM accueil_vue") %>% rename_with(tolower)
print("reach_accueil_vue downloaded")
print(Sys.time()) 

reach_qwsec01_vue  <- dbGetQuery(db,
                                      "SELECT * FROM qwsec01_vue") %>% rename_with(tolower)
print("reach_qwsec01_vue downloaded")
print(Sys.time())

reach_qwsec2a_vue  <- dbGetQuery(db,
                                 "SELECT * FROM qwsec2a_vue") %>% rename_with(tolower)
print("reach_qwsec2a_vue downloaded")
print(Sys.time())

reach_qwsec6a_vue  <- dbGetQuery(db,
                                 "SELECT * FROM qwsec6a_vue") %>% rename_with(tolower)
print("reach_qwsec6a_vue downloaded")
print(Sys.time())

reach_qwsec2b_vue  <- dbGetQuery(db,
                                 "SELECT * FROM qwsec2b_vue") %>% rename_with(tolower)
print("reach_qwsec2b_vue downloaded")
print(Sys.time())

reach_naiss_enfant_vue  <- dbGetQuery(db,
                                      "SELECT * FROM naiss_enfant_vue") %>% rename_with(tolower)
print("reach_naiss_enfant_vue downloaded")
print(Sys.time())

reach_traitement_surv_vue  <- dbGetQuery(db,
                                         "SELECT * FROM traitement_surv_vue") %>% rename_with(tolower)
print("reach_traitement_surv_vue downloaded")
print(Sys.time())


#REACH CONCESSIONS FORMS
# Establish connection
db <- dbConnect(
  RMySQL::MySQL(),
  dbname = "reach_concessions",
  host = "db.reach.instat.ml",  # Usually "localhost" or an IP
  port = 3307,  # Change if needed
  user = "##########",
  password = "##########"
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

reach_concessions_str_rec  <- dbGetQuery(db,
                                         "SELECT * FROM str_rec") %>% rename_with(tolower)
print("reach_conc_str_rec downloaded")
print(Sys.time())

reach_concessions_str_gps  <- dbGetQuery(db,
                                         "SELECT * FROM str_gps") %>% rename_with(tolower)
print("reach_conc_str_gps downloaded")
print(Sys.time())



#REACH HOUSEHOLD FORMS
# Establish connection
db <- dbConnect(
  RMySQL::MySQL(),
  dbname = "reach_hh",
  host = "db.reach.instat.ml",  # Usually "localhost" or an IP
  port = 3307,  # Change if needed
  user = "##########",
  password = "##########"
)


knitr::opts_chunk$set(connection = db, max.print = 30)
reach_tables_list <- dbGetQuery(db, 
                                "show tables")

# Get the list of table names from the database
reach_tables_list <- dbGetQuery(db, "SHOW TABLES") 

table_names <- reach_tables_list[[1]]  

reach_tables <- list()


reach_hh_identification_rec <- dbGetQuery(db,
                                    "SELECT * FROM identification_rec") %>% rename_with(tolower)
print("reach_hh_identification_rec downloaded")
print(Sys.time())

reach_hh_liste_menage <- dbGetQuery(db,
                               "SELECT * FROM liste_menage") %>% rename_with(tolower)
print("reach_hh_liste_menage downloaded")
print(Sys.time())

reach_hh_naiss_enfants_1_59_mois <- dbGetQuery(db,
                                    "SELECT * FROM naiss_enfants_1_59_mois") %>% rename_with(tolower)
print("reach_hh_naiss_enfants_1_59_mois downloaded")
print(Sys.time())

reach_hh_deces_enfants_1_59_mois <- dbGetQuery(db,
                                               "SELECT * FROM deces_enfants_1_59_mois") %>% rename_with(tolower)
print("reach_hh_deces_enfants_1_59_mois downloaded")
print(Sys.time())

reach_hh_traitement_surveillance_enf <- dbGetQuery(db,
                                               "SELECT * FROM traitement_surveillance_enf") %>% rename_with(tolower)
print("reach_hh_traitement_surveillance_enf downloaded")
print(Sys.time())


# Establish connection
db <- dbConnect(
  RMySQL::MySQL(),
  dbname = "reach_menages",
  host = "db.reach.instat.ml",  # Usually "localhost" or an IP
  port = 3307,  # Change if needed
  user = "##########",
  password = "##########"
)


knitr::opts_chunk$set(connection = db, max.print = 30)
reach_tables_list <- dbGetQuery(db, 
                                "show tables")

# Get the list of table names from the database
reach_tables_list <- dbGetQuery(db, "SHOW TABLES") 

table_names <- reach_tables_list[[1]]  

reach_tables <- list()

reach_menages_hh_rec <- dbGetQuery(db,
                               "SELECT * FROM hh_rec") %>% rename_with(tolower)
print("reach_hh_rec downloaded")
print(Sys.time())

reach_menages_hh_gps <- dbGetQuery(db,
                               "SELECT * FROM hh_gps") %>% rename_with(tolower)
print("reach_menages_hh_gps downloaded")
print(Sys.time())


# Establish connection
db <- dbConnect(
  RMySQL::MySQL(),
  dbname = "mortalite_femmes",
  host = "db.reach.instat.ml",  # Usually "localhost" or an IP
  port = 3307,  # Change if needed
  user = "##########",
  password = "##########"
)


knitr::opts_chunk$set(connection = db, max.print = 30)
reach_tables_list <- dbGetQuery(db, 
                                "show tables")

# Get the list of table names from the database
reach_tables_list <- dbGetQuery(db, "SHOW TABLES") 

table_names <- reach_tables_list[[1]]  

reach_tables <- list()


reach_femme_qsecover <- dbGetQuery(db,
                                  "SELECT * FROM qsecover") %>% rename_with(tolower)
print("reach_femme_qsecover downloaded")
print(Sys.time())

reach_femme_qwsec01 <- dbGetQuery(db,
                                   "SELECT * FROM qwsec01") %>% rename_with(tolower)
print("reach_femme_qwsec01 downloaded")
print(Sys.time())

reach_femme_qwsec2a <- dbGetQuery(db,
                                  "SELECT * FROM qwsec2a") %>% rename_with(tolower)
print("reach_femme_qwsec2a downloaded")
print(Sys.time())

reach_femme_qwsec2b <- dbGetQuery(db,
                                  "SELECT * FROM qwsec2b") %>% rename_with(tolower)
print("reach_femme_qwsec2b downloaded")
print(Sys.time())

reach_femme_qwsec2c <- dbGetQuery(db,
                                  "SELECT * FROM qwsec2c") %>% rename_with(tolower)
print("reach_femme_qwsec2c downloaded")
print(Sys.time())

reach_femme_qwsec6a <- dbGetQuery(db,
                                  "SELECT * FROM qwsec6a") %>% rename_with(tolower)
print("reach_femme_qwsec6a downloaded")
print(Sys.time())

reach_femme_qwsec11 <- dbGetQuery(db,
                                  "SELECT * FROM qwsec11") %>% rename_with(tolower)
print("reach_femme_qwsec11 downloaded")
print(Sys.time())

reach_femme_qwsec2y <- dbGetQuery(db,
                                   "SELECT * FROM qwsec2y") %>% rename_with(tolower)
print("reach_qwsec2y downloaded")
print(Sys.time())