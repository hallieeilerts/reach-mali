
################################################
# REACH - Mali
# Baseline survey
################################################

#source("./src/REACH-data-import.R", local = new.env())

# Basic exploration of downloaded files -----------------------------------

source("./src/basic-exploration/vue.R", local = new.env())
source("./src/basic-exploration/concessions.R", local = new.env())
source("./src/basic-exploration/menages.R", local = new.env())
source("./src/basic-exploration/hh.R", local = new.env())
source("./src/basic-exploration/mortalite-menage.R", local = new.env())
source("./src/basic-exploration/mortalite-femme.R", local = new.env())
source("./src/basic-exploration/mortalite-denomb.R", local = new.env())
source("./src/basic-exploration/mortalite-concession.R", local = new.env())

# DHS ---------------------------------------------------------------------

#source("./src/dhs/download-ir-mali.R", local = new.env())
source("./src/dhs/check-sample.R", local = new.env())
source("./src/dhs/merge-ctry-regions.R", local = new.env())
source("./src/dhs/prep-br.R", local = new.env())
source("./src/dhs/prep-br-mali.R", local = new.env())
source("./src/dhs/prep-ir-mali.R", local = new.env())
source("./src/dhs/prep-gr.R", local = new.env())
source("./src/dhs/qc-br.R", local = new.env())
source("./src/dhs/qc-gr.R", local = new.env())
source("./src/dhs/combine-qc.R", local = new.env())
source("./src/dhs/table-dhs.R", local = new.env())
source("./src/dhs/figure-table-prep-dhs-mali.R", local = new.env())
source("./src/dhs/calc-demogsurv-dhs-mali.R", local = new.env())
source("./src/dhs/calc-gapu5m-dhs-mali.R", local = new.env())
source("./src/dhs/calc-tfr-mali.R", local = new.env())

# Women's questionnaire: cover and section 1 ------------------------------

source("./src/sec01/clean-level1.R", local = new.env())
source("./src/sec01/clean-qsecover.R", local = new.env())
source("./src/sec01/clean-qwsec01.R", local = new.env())
source("./src/sec01/impute-respondent-age.R", local = new.env())
source("./src/sec01/combine-cover-sec01.R", local = new.env())
source("./src/sec01/figures-cover-sec01.R", local = new.env())

# Women's questionnaire: SBH -------------------------------------

source("./src/sbh/explore-sbh.R", local = new.env())
source("./src/sbh/combine-sbh-qsecover.R", local = new.env())
source("./src/sbh/clean-sbh.R", local = new.env())
source("./src/sbh/figures-sbh.R", local = new.env())

# New instat files --------------------------------------------------------

# new instat files that were delivered shortly before finishing evaluation for baseline report
# doublechecking that they produce similar mortality estimates

source("./src/combined/explore-partb-womens.R", local = new.env())
source("./src/combined/clean-fph-combined.R", local = new.env())
source("./src/combined/clean-intdate-combined.R", local = new.env())
source("./src/combined/merge-fph-intdate-combined.R", local = new.env())
source("./src/combined/recover-doby-combined.R", local = new.env())
source("./src/combined/clean-aod-combined.R", local = new.env())
source("./src/combined/impute-dob-combined.R", local = new.env())
source("./src/combined/calc-gapu5m-combined.R", local = new.env())

# Women's questionnaire: FPH -------------------------------------

source("./src/fph/explore-fph.R", local = new.env())
source("./src/fph/clean-fph.R", local = new.env())
source("./src/fph/combine-fph-qsecover.R", local = new.env())
source("./src/fph/audit-dob.R", local = new.env())
source("./src/fph/recover-doby.R", local = new.env())
source("./src/fph/clean-aod.R", local = new.env())
source("./src/fph/impute-dob.R", local = new.env())
source("./src/fph/add-tips-period.R", local = new.env())
source("./src/fph/check-sbh-agreement.R", local = new.env())
source("./src/fph/figures-fph.R", local = new.env())
source("./src/fph/figures-dhs.R", local = new.env())

# Mortality calculations --------------------------------------------------

source("./src/mort/calc-gapu5m-functionized.R", local = new.env())
source("./src/mort/calc-gapu5m-jackknife.R", local = new.env())
source("./src/mort/calc-demogsurv.R", local = new.env())
source("./src/mort/calc-dhsrates.R", local = new.env())
source("./src/mort/compare-methods.R", local = new.env())
source("./src/mort/figures-mort.R", local = new.env())
source("./src/mort/figures-mort-mldhs.R", local = new.env())
source("./src/mort/calc-fert.R", local = new.env())

# Dénombrement --------------------------------------------------

source("./src/denomb/combine-qhhsecover-qhsec01.R", local = new.env())
source("./src/denomb/figures-denomb-mort.R", local = new.env())

# Mapping (mortalite-menage) -------------------------------------

source("./src/mapping/combine-qhsec01-hhrec.R", local = new.env())
source("./src/mapping/table-data-collection.R", local = new.env())
source("./src/mapping/insecure-areas.R", local = new.env())
source("./src/mapping/compare-dece-6m.R", local = new.env())
