/* ------------------------------------------------------------------------- *
 * Problem Set 4, Question 1
 * Stats 506, Fall 2020
 *
 * Describe your script here.
 * This file is the SAS code for Problem Set 4, Question 1.
 * Author: Zhihao Xu, xuzhihao@umich.edu
 * ------------------------------------------------------------------------- */

/* libnames:---------------------------------------------------------------- */
libname hw4 '/folders/myfolders/hw4/';
/* 79: --------------------------------------------------------------------- */
/* recs09 */
proc import
 	datafile = "/folders/myfolders/hw4/data/recs2009_public.csv"
 	out = recs09
 	replace;
 	delimiter = ',';
 	getnames = yes;
run;

/* recs2009 replicate weights data*/
proc import
 	datafile = "/folders/myfolders/hw4/data/recs2009_public_repweights.csv"
 	out = brrwt09
 	replace;
 	delimiter = ',';
 	getnames = yes;
run;

/* create long weights for recs09: ----------------------------------------- */
proc transpose data=brrwt09 out=brrwt_long09  prefix=brr_weight_ name=repl;
 	by DOEID;
 	var brr_weight_1-brr_weight_244; 
run; 

/* recs15 */
proc import
 	datafile = "/folders/myfolders/hw4/data/recs2015_public_v4.csv"
 	out = recs15
 	replace;
 	delimiter = ',';
 	getnames = yes;
run;

/* brr_weights long for recs15: -------------------------------------------- */
proc transpose data=recs15 out=brrwt_long15  prefix=BRRWT name=repl;
 	by DOEID;
 	var BRRWT1-BRRWT96; 
run; 


/* data cleaning : --------------------------------------------------------- */

/* Select variables */
data recs09;
 	set recs09;
 	keep DOEID REGIONC TVTYPE1 TVCOLOR NWEIGHT;
run;

data recs15;
 	set recs15;
 	keep DOEID REGIONC TVTYPE1 TVCOLOR NWEIGHT BRRWT1-BRRWT96;
run;

/* Delete missing value */
data recs09;
 	set recs09;
 	if REGIONC = . then delete;
run;

data recs15;
 	set recs15;
 	if REGIONC = . then delete;
run;


/* (a)i: Average number of TV in 2009 by Census Region : ------------------- */

/* sort recs09 by regionc */
proc sort data=recs09 out=recs09;
	by REGIONC;

/* point estimates */
proc means data=recs09 noprint;
  	var TVCOLOR;
  	by REGIONC;
  	weight NWEIGHT; 
  	output out=recs_tv09 mean=avg_tv09; 
run;

data recs09_tv;
 	set recs09;
 	keep doeid regionc tvcolor;
run;

/* sort recs09_tv by doeid */
proc sort data=recs09_tv out=recs09_tv;
	by DOEID;

/* left join */
data recs09_tv_brr;
	Merge recs09_tv(in=T1) brrwt_long09(in=T2);
	If T1;
	by DOEID;
run;

/* sort recs09_tv_brr by regionc and doeid */
proc sort data=recs09_tv_brr out=recs09_tv_brr;
 	by regionc repl; 

/* compute replicate mean */
proc means data=recs09_tv_brr noprint;
 	var tvcolor;
 	by regionc repl;
 	weight brr_weight_1;
 	output out=recs_tv09_repl mean=avg_tv09_repl;
run;

data recs09_tv;
	Merge recs_tv09(in=T1) recs_tv09_repl(in=T2);
	If T1;
	by regionc;
run;


/* compute squared errors  */
data recs09_tv;
 	set recs09_tv;
 	error2 = (avg_tv09_repl - avg_tv09)**2; 
run;

/* compute unscaled MSE */
proc summary data=recs09_tv;
 	by regionc; 
 	var error2;
 	output out=tv09_se
   	mean(error2) = mse; 
run;

/* compute se by fay's method */
data tv09_se;
 	set tv09_se;
 	var09 = mse / (1-0.5)**2;
run;

data ps4_q1_09_tv;
 	merge recs_tv09 tv09_se;
 	by regionc;
run;

%let m = quantile('NORMAL', .975);
data ps4_q1_09_tv;
 	set ps4_q1_09_tv;
 	se = sqrt(var09);
 	lwr = avg_tv09 - &m.*se;
 	upr = avg_tv09 + &m.*se;
run;
proc print data=ps4_q1_09_tv(obs=5); run;

data ps4_q1_tv09;
 	set ps4_q1_09_tv;
 	keep regionc avg_tv09 lwr upr;
run;

proc export data=ps4_q1_tv09
	outfile='/folders/myfolders/hw4/output/ps4_q1_a09.csv' 
	dbms=csv
	replace;
run;

/* (a)ii: Average number of TV in 2015 by Census Region : ------------------ */

/* sort recs15 by regionc */
proc sort data=recs15 out=recs15;
	by REGIONC;


/* point estimates */
proc means data=recs15 noprint;
  	var TVCOLOR;
  	by REGIONC;
  	weight NWEIGHT; 
  	output out=recs_tv15 mean=avg_tv15; 
run;

data recs15_tv;
 	set recs15;
 	keep doeid regionc tvcolor;
run;

/* sort recs15_tv by doeid */
proc sort data=recs15_tv out=recs15_tv;
	by DOEID;

/* left join */
data recs15_tv_brr;
	Merge recs15_tv(in=T1) brrwt_long15(in=T2);
	If T1;
	by DOEID;
run;

/* sort recs15_tv_brr by regionc and doeid */
proc sort data=recs15_tv_brr out=recs15_tv_brr;
 	by regionc repl; 

/* compute replicate mean */
proc means data=recs15_tv_brr noprint;
 	var tvcolor;
 	by regionc repl;
 	weight BRRWT1;
 	output out=recs_tv15_repl mean=avg_tv15_repl;
run;

data recs15_tv;
	Merge recs_tv15(in=T1) recs_tv15_repl(in=T2);
	If T1;
	by regionc;
run;

/* compute squared errors  */
data recs15_tv;
 	set recs15_tv;
 	error2 = (avg_tv15_repl - avg_tv15)**2; 
run;

/* compute unscaled MSE */
proc summary data=recs15_tv;
	by regionc; 
	var error2;
 	output out=tv15_se
   	mean(error2) = mse; 
run;

/* compute se by fay's method */
data tv15_se;
 	set tv15_se;
 	var15 = mse / (1-0.5)**2;
run;

data ps4_q1_15_tv;
 	merge recs_tv15 tv15_se;
 	by regionc;
run;

data ps4_q1_15_tv1;
 	merge recs15_tv tv15_se;
	by regionc;
run;

data ps4_q1_15_tv;
 	set ps4_q1_15_tv;
 	se = sqrt(var15);
 	lwr = avg_tv15 - &m.*se;
 	upr = avg_tv15 + &m.*se;
run;
proc print data=ps4_q1_15_tv(obs=5); run;

data ps4_q1_tv15;
 	set ps4_q1_15_tv;
 	keep regionc avg_tv15 lwr upr;
run;

proc export data=ps4_q1_tv15
	outfile='/folders/myfolders/hw4/output/ps4_q1_a15.csv' 
	dbms=csv
	replace;
run;

data recs_tv_dif;
	Merge ps4_q1_15_tv(in=T1) ps4_q1_09_tv1(in=T2);
	If T1;
	by regionc;
run;

data recs_tv_dif;
	set recs_tv_dif;
	se = sqrt(var15 + var09);
	dif = avg_tv15 - avg_tv09;
	lwr = dif - &m.*se;
	upr = dif + &m.*se;
run;

data recs_tv_dif;
	set recs_tv_dif;
	keep reginoc dif lwr upr;
run;

proc export data=recs_tv_dif
	outfile='/folders/myfolders/hw4/output/ps4_q1_aii.csv' 
	dbms=csv
	replace;
run;


/* (b)i: Proportion of TV Type in 2015 by Census Region -------------------- */
/* sort recs09 and brrwt09 by doeid */
proc sort data=recs09 out=recs09;
	by DOEID;
proc sort data=brrwt09 out=brrwt09;
	by DOEID;	
data recs09_w_brr;
	Merge recs09(in=T1) brrwt09(in=T2);
	If T1;
	by DOEID;
run;

proc surveyfreq data = recs09_w_brr varmethod=BRR (fay=.5);
	weight NWEIGHT;
	tables REGIONC * TVTYPE1 / row;
	repweights brr_weight_1 -- brr_weight_244;
	ods output CrossTabs=recs09_prop;
run;

data recs09_prop;
	set recs09_prop;
	keep REGIONC TVTYPE1 RowPercent RowStdErr;
run;
data recs09_prop;
 	set recs09_prop;
 	if TVTYPE1 = . then delete;
 	if REGIONC = . then delete;
run;

data recs09_prop_;
 	set recs09_prop;
 	se = RowStdErr;
 	lwr = RowPercent - &m.*se;
 	upr = RowPercent + &m.*se;
run;
data recs09_prop_;
	set recs09_prop_;
	keep REGIONC TVTYPE1 RowPercent lwr upr;
run;
proc print data=recs09_prop_;run;

proc export data=recs09_prop_
	outfile='/folders/myfolders/hw4/output/ps4_q1_b09.csv' 
	dbms=csv
	replace;
run;

/* (b)i: Proportion of TV Type in 2015 by Census Region -------------------- */

proc surveyfreq data = recs15 varmethod=BRR (fay=.5);
	weight NWEIGHT;
	tables REGIONC * TVTYPE1 / row;
	repweights BRRWT1 -- BRRWT96;
	ods output CrossTabs=recs15_prop;
run;

data recs15_prop;
	set recs15_prop;
	keep REGIONC TVTYPE1 RowPercent RowStdErr;
run;
data recs15_prop;
 	set recs15_prop;
 	if TVTYPE1 = . then delete;
 	if REGIONC = . then delete;
run;

data recs15_prop_;
 	set recs15_prop;
 	se = RowStdErr;
 	lwr = RowPercent - &m.*se;
 	upr = RowPercent + &m.*se;
run;
data recs15_prop_;
	set recs15_prop_;
	keep REGIONC TVTYPE1 RowPercent lwr upr;
run;

proc export data=recs15_prop_
	outfile='/folders/myfolders/hw4/output/ps4_q1_b15.csv' 
	dbms=csv
	replace;
run;

data recs_prop_dif;
	Merge 
		recs09_prop(in=T1 rename=(RowPercent=rowper09 RowStdErr=rowse09)) 
		recs15_prop(in=T2 rename=(RowPercent=rowper15 RowStdErr=rowse15)) ;
	If T1;
	by REGIONC TVTYPE1;
run;

data recs_prop_dif;
	set recs_prop_dif;
	se = sqrt(rowse09**2 + rowse15**2);
	dif = rowper15 - rowper09;
	lwr = dif - &m.*se;
	upr = dif + &m.*se;
run;

data recs_prop_dif;
	set recs_prop_dif;
	keep reginoc tvtype1 dif lwr upr;
run;

proc export data=recs_prop_dif
	outfile='/folders/myfolders/hw4/output/ps4_q1_bii.csv' 
	dbms=csv
	replace;
run;

/* 79: --------------------------------------------------------------------- */
