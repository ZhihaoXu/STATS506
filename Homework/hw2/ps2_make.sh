#!usr/bin/env bash

# Stats 506, Fall 2020
#
# Shell Script for Problem Set 2
#
# Author(s): James Henderson
# Updated: September 13, 2020
# 79: -------------------------------------------------------------------------

# download data for the problem set if needed: --------------------------------
url_base="https://www.eia.gov/consumption/residential/data"
files=(
"recs2015_public_v4.csv" 
"recs2009_public.csv" 
"recs2009_public_repweights.csv"
"recs2009_public_codebook.xlsx"
"codebook_publicv4.xlsx"
) 
urls=(
"$url_base/2015/csv/${files[0]}"
"$url_base/2009/csv/${files[1]}"
"$url_base/2009/csv/${files[2]}"
"$url_base/2009/xls/${files[3]}"
"$url_base/2015/xls/${files[4]}"
)

if [ ! -d "./data" ]; then
  mkdir ./data
fi
if [ ! -d "./pic" ]; then
  mkdir ./pic
fi

for i in 0 1 2 3 4
do
	if [ ! -f "./data/${files[i]}" ]; then
		wget -P "./data/" ${urls[i]}
	fi
done
# 79: -------------------------------------------------------------------------
