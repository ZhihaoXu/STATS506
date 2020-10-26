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
//cd /Users/zhihao/Desktop/STATS506/Homework/hw3 // comment out before submission 
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
mkdir ./ohx // Comment it if the folder already created
cd ohx

// generate dummy variable for age group
gen age_1 = inrange(age,0,5)
gen age_2 = inrange(age,6,11)
gen age_3 = inrange(age,12,17)
gen age_4 = inrange(age,18,23)
gen age_5 = inrange(age,24,29)
gen age_6 = inrange(age,30,35)
gen age_7 = inrange(age,36,41)
gen age_8 = inrange(age,42,47)
gen age_9 = inrange(age,48,53)
gen age_10 = inrange(age,54,59)
gen age_11 = inrange(age,60,65)
gen age_12 = inrange(age,66,71)
gen age_13 = inrange(age,72,77)
gen age_14 = inrange(age,78,84)


local vars1 = "ohx01tc ohx02tc ohx03tc ohx04tc ohx05tc ohx06tc ohx07tc ohx08tc"
local vars2 = "ohx09tc ohx10tc ohx11tc ohx12tc ohx13tc ohx14tc ohx15tc ohx16tc"
local vars3 = "ohx17tc ohx18tc ohx19tc ohx20tc ohx21tc ohx22tc ohx23tc ohx24tc"
local vars4 = "ohx25tc ohx26tc ohx27tc ohx28tc ohx29tc ohx30tc ohx31tc ohx32tc"

// create table and output
foreach var in `vars1' `vars2' `vars3' `vars4'{
	estpost tabstat age_1 age_2 age_3 age_4 age_5 age_6 age_7 ///
					age_8 age_9 age_10 age_11 age_12 age_13 age_14, ///
			by(`var') statistics(mean semean n) columns(statistics) listwise	
	esttab . using `var'.csv, main(mean) aux(semean) ///
		     nostar unstack noparentheses nonotes replace
}

log close
// 79: ---------------------------------------------------------------------- *
