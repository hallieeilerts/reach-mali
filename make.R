
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
source("./src/dhs/prep-gr.R", local = new.env())
source("./src/dhs/qc-br.R", local = new.env())
source("./src/dhs/qc-gr.R", local = new.env())
source("./src/dhs/combine-qc.R", local = new.env())
source("./src/dhs/table-dhs.R", local = new.env())

# Cover and section 1 -----------------------------------------------------

source("./src/sec01/explore-qsecover.R", local = new.env())
source("./src/sec01/clean-qsecover.R", local = new.env())
source("./src/sec01/explore-qwsec01.R", local = new.env())
source("./src/sec01/clean-qwsec01.R", local = new.env())
#source("./src/sec01/impute-respondent-dob.R", local = new.env())
source("./src/sec01/impute-respondent-age.R", local = new.env())
source("./src/sec01/combine-cover-sec01.R", local = new.env())
source("./src/sec01/figures-cover-sec01.R", local = new.env())

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
source("./src/fph/figures-fph.R", local = new.env())
source("./src/fph/figures-dhs.R", local = new.env())

# Mortality calculations --------------------------------------------------

#source("./src/mort/calc-gapu5m.R", local = new.env())
source("./src/mort/calc-gapu5m-functionized.R", local = new.env())
source("./src/mort/calc-demogsurv.R", local = new.env())
source("./src/mort/compare-demogsurv.R", local = new.env())
source("./src/mort/figures-mort.R", local = new.env())

