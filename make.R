
################################################
# REACH - Mali
# Baseline survey
################################################

#source("./src/REACH-data-import.R", local = new.env())

# Basic exploration of downloaded files -----------------------------------

source("./src/basic-exploration/vue.R", local = new.env())
source("./src/basic-exploration/concessions.R", local = new.env())
source("./src/basic-exploration/hh.R", local = new.env())
source("./src/basic-exploration/menages.R", local = new.env())
source("./src/basic-exploration/mortalite-menage.R", local = new.env())
source("./src/basic-exploration/mortalite-femme.R", local = new.env())
source("./src/basic-exploration/mortalite-denomb.R", local = new.env())
source("./src/basic-exploration/mortalite-concession.R", local = new.env())

# DHS ---------------------------------------------------------------------

source("./src/dhs/check-sample.R", local = new.env())
source("./src/dhs/merge-ctry-regions.R", local = new.env())
source("./src/dhs/prep-br.R", local = new.env())
source("./src/dhs/prep-br-mali.R", local = new.env())
source("./src/dhs/prep-gr.R", local = new.env())
source("./src/dhs/qc-br.R", local = new.env())
source("./src/dhs/qc-gr.R", local = new.env())
source("./src/dhs/combine-qc.R", local = new.env())
source("./src/dhs/table-dhs.R", local = new.env())
source("./src/dhs/figure-table-prep-dhs-mali.R", local = new.env())

# Cover and section 1 -----------------------------------------------------

source("./src/sec01/explore-qsecover.R", local = new.env())
source("./src/sec01/clean-qsecover.R", local = new.env())
source("./src/sec01/explore-qwsec01.R", local = new.env())
source("./src/sec01/clean-qwsec01.R", local = new.env())
#source("./src/sec01/impute-respondent-dob.R", local = new.env())
source("./src/sec01/impute-respondent-age.R", local = new.env())
source("./src/sec01/combine-cover-sec01.R", local = new.env())
source("./src/sec01/figures-cover-sec01.R", local = new.env())

# Mortality dénombrement --------------------------------------------------

source("./src/denomb-mort/figures-denomb-mort.R", local = new.env())

# Need to create table for number of women in denombrement
# number of women 15-49 in denombrement
# number of women 15-49 in denombrement in mortality survey
# number per cluster/region
# I don't think that I can do this currently. 
# The individual IDs that come with reach_mortalite_femme_qsecover.csv are...
# qsecover_id, level_1_id, w_nom and more household variables..
# Those in reach_mortalite_denomb_qhhsecover are...
# qhhsecover_id level_1_id qhhi1 and more household variables...
# I don't think these level 1 IDs match though

# SBH ---------------------------------------------------------------------

source("./src/sbh/explore-sbh.R", local = new.env())
source("./src/sbh/clean-sbh.R", local = new.env())
source("./src/sbh/combine-sbh-qsecover.R", local = new.env())
source("./src/sbh/figures-sbh.R", local = new.env())

# FPH ---------------------------------------------------------------------

source("./src/fph/explore-fph.R", local = new.env())
source("./src/fph/clean-fph.R", local = new.env())
source("./src/fph/audit-dob.R", local = new.env())
source("./src/fph/combine-fph-qsecover.R", local = new.env())
source("./src/fph/clean-aod.R", local = new.env())
source("./src/fph/impute-dob.R", local = new.env())
source("./src/fph/add-tips-period.R", local = new.env())
source("./src/fph/check-sbh-agreement.R", local = new.env())
source("./src/fph/figures-fph.R", local = new.env())
source("./src/fph/figures-dhs.R", local = new.env())

# Mortality calculations --------------------------------------------------

#source("./src/mort/calc-gapu5m.R", local = new.env())
source("./src/mort/calc-gapu5m-functionized.R", local = new.env())
source("./src/mort/calc-gapu5m-jackknife.R", local = new.env())
source("./src/mort/calc-demogsurv.R", local = new.env())
source("./src/mort/calc-dhsrates.R", local = new.env())
source("./src/mort/compare-methods.R", local = new.env())
source("./src/mort/figures-mort.R", local = new.env())

