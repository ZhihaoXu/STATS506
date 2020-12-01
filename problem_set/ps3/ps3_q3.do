*-----------------------------------------------------------------------------*
* Stats 506, Fall 2020 
* Problem Set 3, Question 3
* 
* Descirbe your script here
* 
*
* Author: Zhihao Xu
* Updated: October 23, 2020
*-----------------------------------------------------------------------------*
// 79: ---------------------------------------------------------------------- *

// set up: ------------------------------------------------------------------ *
cd /Users/zhihao/Desktop/STATS506/Homework/hw3 // comment out before submission 
*version
log using ps3_q3.log, text replace

// data prep: --------------------------------------------------------------- *
// import and split data
import delimited nhanes_ohxden.csv, clear
save ohxden.dta, replace
import delimited nhanes_demo.csv, clear
keep seqn ridageyr ridstatr
merge 1:1 seqn using ohxden.dta
drop if ridstatr != 2

rename ridageyr age
drop if ohddests != 1

cd output



gen age_group = floor(age/6) + 1
local vars1 = "ohx01tc ohx02tc ohx03tc ohx04tc ohx05tc ohx06tc ohx07tc ohx08tc"
local vars2 = "ohx09tc ohx10tc ohx11tc ohx12tc ohx13tc ohx14tc ohx15tc ohx16tc"
local vars3 = "ohx17tc ohx18tc ohx19tc ohx20tc ohx21tc ohx22tc ohx23tc ohx24tc"
local vars4 = "ohx25tc ohx26tc ohx27tc ohx28tc ohx29tc ohx30tc ohx31tc ohx32tc"

putexcel set test.xlsx, replace
foreach var in `vars1' `vars2' `vars3' `vars4'{
	proportion `var', over(age_group)
	return list
	matrix results = r(table)
	putexcel set ohx.xlsx, sheet(`var') modify						  
	putexcel A1 = matrix(results[1..2,.])
	tabulate `var', matrow(names)
	matrix list names
	putexcel A3=matrix(names') 

}

proportion ohx03tc, over(age_group) header
return list

log close
// 79: ---------------------------------------------------------------------- *
