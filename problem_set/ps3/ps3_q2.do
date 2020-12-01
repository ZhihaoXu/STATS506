*-----------------------------------------------------------------------------*
* Stats 506, Fall 2020 
* Problem Set 3, Question 2
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
log using ps3_q2.log, text replace

// data prep: --------------------------------------------------------------- *
// import and split data
import delimited nhanes_demo.csv, clear
save demo.dta, replace
import delimited nhanes_ohxden.csv, clear
keep seqn ohddests
merge 1:1 seqn using demo.dta
generate ohx = 1
replace ohx = 0 if ohddests != 1
rename ridageyr age
// ohx label
label define ohxnames ///
  1 "Complete" ///
  0 "Missing"
label values ohx ohxnames
// race labels
label define racenames ///
  1 "Mexican American" ///
  2 "Other Hispanic" ///
  3 "Non-Hispanic White" ///
  4 "Non-Hispanic Black" ///
  6 "Non-Hispanic Asian" ///
  7 "Other Race - Including Multi-Racial" 
label values ridreth3 racenames

drop if ridstatr != 2
generate under20 = "<20"
replace under20 = "20+" if age >= 20
generate edu = "Some college/College graduate"
replace edu = "No college/Unknown" if dmdeduc2 != 3 & dmdeduc2 != 4
encode edu, gen(n_edu)
encode under20, gen(u20)

// model fitting: ----------------------------------------------------------- *
cd output
putexcel set ps3_q2.xlsx, sheet(logitreg) modify
putexcel A1="model" B1= "AIC"

logit ohx ///
	  c.age i.riagendr i.n_edu
estat ic
return list
matrix a = r(S)
putexcel A2="c.age i.riagendr i.n_edu" B2= a[1,5]

logit ohx ///
	  c.age##c.age i.riagendr i.n_edu
estat ic
return list
matrix a = r(S)
putexcel A3="cc.age##c.age i.riagendr i.n_edu" B3= a[1,5]

logit ohx ///
	  c.age##c.age##i.riagendr i.n_edu
estat ic
return list
matrix a = r(S)
putexcel A4="cc.age##c.age##i.riagendr i.n_edu" B4= a[1,5]

logit ohx ///
	  c.age##c.age i.riagendr i.n_edu i.u20 
estat ic
return list
matrix a = r(S)
putexcel A5="c.age##c.age i.riagendr i.n_edu i.u20 " B5= a[1,5]

logit ohx ///
	  c.age##c.age##i.u20 i.riagendr i.n_edu
estat ic
return list
matrix a = r(S)
putexcel A6="c.age##c.age##i.u20 i.riagendr i.n_edu" B6= a[1,5]


logit ohx ///
	  c.age##c.age##i.u20 i.riagendr i.n_edu
putexcel set ps3_q2.xlsx, sheet(regout) modify
putexcel A1 = etable
log close
// 79: ---------------------------------------------------------------------- *
