*-----------------------------------------------------------------------------*
* Stats 506, Fall 2020 
* Case Study 2
* 
* Estimate regional totals for select types of 
* energy usage using the RECS 2015 data. 
*
* Author: James Henderson
* Updated: September 28, 2020
*-----------------------------------------------------------------------------*
// 79: ---------------------------------------------------------------------- *

// set up: ------------------------------------------------------------------ *
version 16.1
log using 2-energy_type.log, text replace
*cd ~/github/Stats506_F20/case_studies/2-stata/

// data prep: --------------------------------------------------------------- *
local base_url "https://www.eia.gov/consumption/residential/data/2015/csv"
local recs_file "recs2015_public_v4.csv"
local recs_dta = "recs2015.dta"

capture confirm file `recs_dta'
if _rc { 
  display "Downloading file"
  import delimited `base_url'/`recs_file'
  save `recs_dta', replace
} 
else {
  display "Loading local file"
  use `recs_dta', clear
}

// snapshot for later
preserve // batch mode

// We will use these variable names repeatedly: ----------------------------- *
local vars "kwh cufeetng gallonlp gallonfo"

// keep primary energy use variables
keep doeid nweight regionc `vars'
save recs2015_fuels.dta, replace

// generate contributions to estimate for national total: ------------------- *
//local vars "kwh cufeetng gallonlp gallonfo"
foreach var of varlist `vars' {
  generate t`var' = `var' * nweight
}

// compute point estimates for regional totals: ----------------------------- *
//keep doeid regionc t*
collapse (sum) t*, by(regionc)

// add a fake variable to merge on later and save
generate fid=0
save recs2015_fuels_petotal.dta, replace

// keep replicate weights and reshape to long: ------------------------------ *
//use recs2015, clear
restore  // batch mode
keep doeid brrwt1-brrwt96

reshape long brrwt, i(doeid) j(repl)
//save recs2015_brrwt_long.dta, replace // if needed again later

// estimate standard errors: ------------------------------------------------ *
// merge fuel data and replicate weights
merge m:1 doeid using recs2015_fuels.dta

// compute replicate estimates 
//local vars "kwh cufeetng gallonlp gallonfo"
foreach var of varlist `vars' {
  generate t`var'_r = `var' * brrwt
}

collapse (sum) t*_r, by(regionc repl)

// merge against point estimates using regionc
merge m:1 regionc using recs2015_fuels_petotal.dta

// compute residuals
// local vars "kwh cufeetng gallonlp gallonfo"
foreach var in `vars' {
  generate rsq_`var'= (t`var' - t`var'_r)^2
}

// collapse to point estimates and standard errors
//drop *_r _merge
collapse (first) t* (mean) rsq_*, by(regionc)

// Fay coefficient
//local vars "kwh cufeetng gallonlp gallonfo"
foreach var in `vars' {
  replace rsq_`var' = 2 * sqrt(rsq_`var')
}

// reshape and output as csv
reshape long t rsq_, i(regionc) j(fuel) string

rename t total
rename rsq_ std_error

export delimited recs2015_regional_usage.csv, replace

log close
exit, clear
// 79: ---------------------------------------------------------------------- *
