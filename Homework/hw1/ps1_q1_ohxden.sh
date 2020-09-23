#!usr/bin/env bash

# Stats 506, Fall 2020 Homework 1 Question 1
#
# Author(s): Zhihao Xu
# Updated: September 22, 2020
# 79: -------------------------------------------------------------------------

# download data for the problem set if needed: --------------------------------
files=("OHXDEN_J.XPT" "OHXDEN_I.XPT" "OHXDEN_H.XPT" "OHXDEN_G.XPT") 
urls=(
"https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/OHXDEN_J.XPT"
"https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/OHXDEN_I.XPT"
"https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/OHXDEN_H.XPT"
"https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/OHXDEN_G.XPT"
)
csv_files=("OHXDEN_J.csv" "OHXDEN_I.csv" "OHXDEN_H.csv" "OHXDEN_G.csv")
for i in 0 1 2 3
do
	 ## download only if the file doesn't exist
	if [ ! -f "${files[i]}" ]; then
		wget ${urls[i]}
	fi
done

for i in 0 1 2 3
do
	if [ ! -f "${csv_files[i]}" ]; then
		Rscript ./xpt2csv.R "${files[i]}"
	fi
done

if [ ! -f "nhanes_ohxden.csv" ]; then
	head -n +1 OHXDEN_J.csv |
	cut -d "," -f2,4,6-65>> nhanes_ohxden.csv
	for i in 3 2 1 0
	do
		tail -n +2 "${csv_files[i]}" |
		cut -d "," -f2,4,6-65>> nhanes_ohxden.csv
	done
fi
echo "Part (a) Done" 

# 79: -------------------------------------------------------------------------
