#!usr/bin/env bash

# Stats 506, Fall 2020 Homework 1 Question 1
#
# Author(s): Zhihao Xu
# Updated: September 22, 2020
# 79: -------------------------------------------------------------------------

# download data for the problem set if needed: --------------------------------
files=("DEMO_J.XPT" "DEMO_I.XPT" "DEMO_H.XPT" "DEMO_G.XPT") 
urls=(
"https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT"
"https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT"
"https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.XPT"
"https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.XPT"
)
csv_files=("DEMO_J.csv" "DEMO_I.csv" "DEMO_H.csv" "DEMO_G.csv")
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

if [ ! -f "nhanes_demo.csv" ]; then
	head -n +1 DEMO_J.csv |
	cut -d "," -f2,6,9,18-19,4,44-45,43,42>> nhanes_demo.csv
	for i in 3 2 1 0
	do
		tail -n +2 "${csv_files[i]}" |
		cut -d "," -f2,6,9,18-19,4,44-45,43,42>> nhanes_demo.csv
	done
fi
echo "Part (b) Done"
# 79: -------------------------------------------------------------------------
