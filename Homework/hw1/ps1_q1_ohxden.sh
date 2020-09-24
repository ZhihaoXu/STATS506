#!usr/bin/env bash

# Stats 506, Fall 2020 Homework 1 Question 1
#
# Author: Zhihao Xu
# Updated: September 22, 2020
# 79: -------------------------------------------------------------------------

files=("OHXDEN_J.XPT" "OHXDEN_I.XPT" "OHXDEN_H.XPT" "OHXDEN_G.XPT") 
urls=(
"https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/OHXDEN_J.XPT"
"https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/OHXDEN_I.XPT"
"https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/OHXDEN_H.XPT"
"https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/OHXDEN_G.XPT"
)
csv_files=("OHXDEN_J.csv" "OHXDEN_I.csv" "OHXDEN_H.csv" "OHXDEN_G.csv")

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

file="nhanes_ohxden.csv"
# cut selected column
if [ ! -f "$file" ]; then
	head -n +1 OHXDEN_J.csv |
	cut -d "," -f2,4,6-65>> $file
	for i in 3 2 1 0
	do
		tail -n +2 "${csv_files[i]}" |
		cut -d "," -f2,4,6-65>> $file
	done
fi

# Check duplicate lines
uniq_lines=$(< $file sort | uniq | wc -l)
n_lines=$(< $file wc -l)
if [ $uniq_lines == $n_lines ]; then
    echo "No duplicates in $file. Part (a) Finished."
else
    echo "Total lines in $file:" $n_lines, Unique lines: $uniq_lines.
fi

# 79: -------------------------------------------------------------------------
