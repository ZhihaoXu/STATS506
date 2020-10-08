*-----------------------------------------------------------------------------*
* Stats 506, Fall 2020 
* Case Study 2
* 
* Estimate national totals for select types of 
* energy usage using the RECS 2015 data. 
*
* Author: James Henderson
* Updated: September 28, 2020
*-----------------------------------------------------------------------------*
// 79: ---------------------------------------------------------------------- *

// set up: ------------------------------------------------------------------ *
version 16.1
log using 2-engergy_type.log, text replace

// data prep: --------------------------------------------------------------- *
local base_url "https://www.eia.gov/consumption/residential/data/2015/csv/"
local recs_file "recs2015_public_v4.csv"
import delimited `base_url'/`recs_file'

// snapshot for later
save recs2015.dta, replace // interactive
// preserve // batch mode

// We will use these variable names repeatedly: ----------------------------- *
local vars "kwh cufeetng gallonlp gallonfo"

// keep primary energy use variables
keep doeid nweight `vars'
save recs2015_fuels.dta, replace

// generate contributions to estimate for national total: ------------------- *
//local vars "kwh cufeetng gallonlp gallonfo"
foreach var of varlist `vars' {
  generate t`var' = `var' * nweight
}

// compute point estimates for national totals: ----------------------------- *
keep doeid t*
collapse (sum) t*

// add a fake variable to merge on later and save
generate fid=0
save recs2015_fuels_petotal.dta, replace

// keep replicate weights and reshape to long: ------------------------------ *
use recs2015, clear
// restore  // batch mode
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

collapse (sum) t*_r, by(repl)

// merge against point estimates using fid

generate fid=0
merge m:1 fid using recs2015_fuels_petotal.dta

// compute residuals
// local vars "kwh cufeetng gallonlp gallonfo"
foreach var in `vars' {
  generate rsq_`var'= (t`var' - t`var'_r)^2
}

// collapse to point estimates and standard errors
drop *_r _merge
collapse (first) t* (mean) rsq_*

// Fay coefficient
local vars "kwh cufeetng gallonlp gallonfo"
foreach var in `vars' {
  replace rsq_`var' = 2 * sqrt(rsq_`var')
}

// reshape and output as csv
generate fid=0
reshape long t rsq_, i(fid) j(fuel) string

rename t total
rename rsq_ std_error
drop fid

export delimited recs2015_usage.csv, replace

log close
// 79: ---------------------------------------------------------------------- *
