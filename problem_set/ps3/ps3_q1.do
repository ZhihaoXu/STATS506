*-----------------------------------------------------------------------------*
* Stats 506, Fall 2020 
* Problem Set 3, Question 1
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
log using ps3_q1.log, text replace

// data prep: --------------------------------------------------------------- *
// import and split data
import delimited nhanes_demo.csv, clear
save demo.dta, replace
import delimited nhanes_ohxden.csv, clear
keep seqn ohddests
merge 1:1 seqn using demo.dta
rename ridageyr age
generate ohx = 1
replace ohx = 0 if ohddests != 1
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
bys under20 ohx: egen age_iqr = iqr(age)
bys under20 ohx: tab(age_iqr)


mkdir ./output // Comment it if the folder is already created
cd output

// create table: ------------------------------------------------------------ *
// under 20
estpost  tab ohx riagendr if under20 == "<20", chi2
esttab using gender20m.csv, replace wide plain  ///
						  cells(b colpct) stats(chi2 p) unstack

estpost  tab ohx ridreth3 if under20 == "<20", chi2
esttab using race20m.csv, replace wide plain  ///
						  cells(b colpct) stats(chi2 p) unstack

ttest age if under20 == "<20", by(ohx)	
return list					  
putexcel set age.xlsx, sheet(20m) modify						  
putexcel A1="Missing" A2="Complete"					  
putexcel B1=`r(mu_1)' B2=`r(mu_2)'
putexcel C1=0 C2=10
putexcel D1=`r(t)' D2=`r(p)'			 

// over 20
estpost  tab ohx riagendr if under20 == "20+", chi2
esttab using gender20p.csv, replace wide plain  ///
						  cells(b colpct) stats(chi2 p) unstack

estpost  tab ohx ridreth3 if under20 == "20+", chi2
esttab using race20p.csv, replace wide plain  ///
						  cells(b colpct) stats(chi2 p) unstack

estpost  tab ohx edu if under20 == "20+", chi2
esttab using edu20p.csv, replace wide plain  ///
						  cells(b colpct) stats(chi2 p) unstack
						  
ttest age if under20 == "20+", by(ohx)	
return list					  
putexcel set age.xlsx, sheet(20p) modify						  
putexcel A1="Missing" A2="Complete"					  
putexcel B1=`r(mu_1)' B2=`r(mu_2)'
putexcel C1=26 C2=30
putexcel D1=`r(t)' D2=`r(p)'

log close
// 79: ---------------------------------------------------------------------- *


