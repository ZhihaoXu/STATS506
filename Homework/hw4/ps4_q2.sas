/* ------------------------------------------------------------------------- *
 * Problem Set 4, Question 2
 * Stats 506, Fall 2020
 *
 * Describe your script here.
 * This file is the SAS code for Problem Set 4, Question 2.
 * Author: Zhihao Xu, xuzhihao@umich.edu
 * ------------------------------------------------------------------------- */

/* libnames:---------------------------------------------------------------- */
libname hw4 '/folders/myfolders/hw4/';
/* 79: --------------------------------------------------------------------- */
/* demo */
proc import
 	datafile = "/folders/myfolders/hw4/data/nhanes_demo.csv"
 	out = demo
 	replace;
 	delimiter = ',';
 	getnames = yes;
run;

/* ohxden */
proc import
 	datafile = "/folders/myfolders/hw4/data/nhanes_ohxden.csv"
 	out = ohxden
 	replace;
 	delimiter = ',';
 	getnames = yes;
run;

data nhanes;
	merge demo ohxden;
	by SEQN;
	ohx01pre = (OHX01TC = 2);
run;

proc logistic data= nhanes; 
 model ohx01pre = riagendr ridageyr ridageyr*ridageyr;
 ods output 
 	ParameterEstimates=pe
 	FitStatistics=fs;
run;

proc surveylogistic data=nhanes; 
 model ohx01pre = riagendr ridageyr ridageyr*ridageyr;
 weight WTMEC2YR;
 ods output 
 	ParameterEstimates=pe_w
 	FitStatistics=fs_w;
run;

proc export data=pe
	outfile='/folders/myfolders/hw4/output/ps4_q2_1.csv' 
	dbms=csv
	replace;
run;

proc export data=fs
	outfile='/folders/myfolders/hw4/output/ps4_q2_2.csv' 
	dbms=csv
	replace;
run;

proc export data=pe_w
	outfile='/folders/myfolders/hw4/output/ps4_q2_3.csv' 
	dbms=csv
	replace;
run;
proc export data=fs_w
	outfile='/folders/myfolders/hw4/output/ps4_q2_4.csv' 
	dbms=csv
	replace;
run;