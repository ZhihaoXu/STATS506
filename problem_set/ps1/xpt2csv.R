## Transfer XPT file to CSV
##
## This Rscript can read the file name from the cammand line. Then 
## read the XPT file, transfer it to csv and save it with the same 
## name with extension ".csv".
## Usage: Rscript ./xpt2csv.R filename
## 
## Author: Zhihao Xu, xuzhihao@umich.edu
## Updated: September 24, 2020
#! Update the date every time you work on a script. 
# 79: -------------------------------------------------------------------------

#! Load libraries at the top of your script.
# libraries: ------------------------------------------------------------------
library('foreign') 

# main:------------------------------------------------------------------------
name = commandArgs(TRUE)[1]
data=read.xport(name) 
csv_name = paste(substr(name,1,nchar(name)-3),"csv",sep="")
write.csv(data, file=csv_name)
                
