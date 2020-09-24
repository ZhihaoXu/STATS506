#!usr/bin/env bash

# Stats 506, Fall 2020 Homework 1 Question 1
#
# Author: Zhihao Xu
# Updated: September 22, 2020
# 79: -------------------------------------------------------------------------

files=("DEMO_J.XPT" "DEMO_I.XPT" "DEMO_H.XPT" "DEMO_G.XPT") 
urls=(
"https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT"
"https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT"
"https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.XPT"
"https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.XPT"
)
csv_files=("DEMO_J.csv" "DEMO_I.csv" "DEMO_H.csv" "DEMO_G.csv")

# download data for the problem set
for i in 0 1 2 3
do
	if [ ! -f "${files[i]}" ]; then
		wget ${urls[i]}
	fi
done

# transfer the XPT file to csv by R
for i in 0 1 2 3
do
	if [ ! -f "${csv_files[i]}" ]; then
		Rscript ./xpt2csv.R "${files[i]}"
	fi
done

file="nhanes_demo.csv"
# cut selected column
if [ ! -f "nhanes_demo.csv" ]; then
	head -n +1 DEMO_J.csv |
	cut -d "," -f2,6,9,18-19,4,44-45,43,42>> $file
	for i in 3 2 1 0
	do
		tail -n +2 "${csv_files[i]}" |
		cut -d "," -f2,6,9,18-19,4,44-45,43,42>> $file
	done
fi

# Check duplicate lines
uniq_lines=$(< $file sort | uniq | wc -l)
n_lines=$(< $file wc -l)
if [ $uniq_lines == $n_lines ]; then
    echo "No duplicates in $file. Part (b) Finished."
else
    echo "Total lines in $file:" $n_lines, Unique lines: $uniq_lines.
fi

# 79: -------------------------------------------------------------------------
