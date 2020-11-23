# Examples from the notes on using the data.table package
# Stats 506, Fall 2020
#
# Covered in this example:
#   fread()
#   data.table()
#   "i"
#   "j"
#   "by"
#   special symobls: .N, .SD, 
#   "keyby" 
#   chaining aka pipes
#
# Author: James Henderson
# Updated: November 9, 2020
# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(tidyverse); library(data.table)

# NYC 14 flights data: --------------------------------------------------------
url = 
  'https://github.com/arunsrinivasan/flights/wiki/NYCflights14/flights14.csv'
nyc14 = fread(url)
class(nyc14)

# creating: -------------------------------------------------------------------
n = 1e3
data.table(a = 1:n, b = rnorm(n), c = sample(letters, n, replace = TRUE))

# [] are generic (primitive) functions: ---------------------------------------
getS3method('[', 'data.table')

# subsetting data in "i": -----------------------------------------------------
# dt[i, j, by]

## by position
nyc14[1:2, ]
lga_dtw[c(1, .N)] # .N is a special symbol, compare dplyr::n()

## using a logical (compare "dplyr::filter()")
lga_dtw = nyc14[origin == 'LGA' & dest == 'DTW', ]

## indexing a matrix
ld_mat = as.matrix(lga_dtw)
ld_mat[c(1, nrow(ld_mat))]

## indexing a data.fame
ld_df = as.data.frame(lga_dtw)
dim(ld_df[1:2])  # What would happen if we called: ld_df[c(1, nrow(ld_df))] ?

# ordering data in  "i": ------------------------------------------------------
lga_dtw[order(-month, -day, dep_time)] # compare, dplyr::arrange()

# selecting columns in "j": ---------------------------------------------------

## when "j" is a list, the result is a data.table
nyc14[origin == 'LGA' & dest == 'DTW', 
      list(dep_time, arr_time, carrier, flight) ]

## `.()` is a synonym for `list()` within `data.table`  
nyc14[origin == 'LGA' & dest == 'DTW', .(dep_time, arr_time, carrier, flight)]

## using a character vector of column names
nyc14[origin == 'LGA' & dest == 'DTW', 
      c("dep_time", "arr_time", "carrier", "flight")]
my_cols = c("dep_time", "arr_time", "carrier", "flight")
nyc14[origin == 'LGA' & dest == 'DTW', ..my_cols] ## .. to refer to global env
nyc14[origin == 'LGA' & dest == 'DTW', my_cols, with = FALSE]
nyc14[origin == 'LGA' & dest == 'DTW', .SD, .SDcols = my_cols]

## deselect columns using negation (`-` or `!`) using `with = FALSE`  
length(nyc14) 
length(nyc14[, -c("tailnum"), with = FALSE])
length(nyc14[, !c("cancelled", "year", "day", "hour", "min"), with = FALSE])

# using the "by" argument: ----------------------------------------------------

## find the percent of flights with delays of 30 minutes or more by carrier
nyc14[ , delay30 := 1L * {dep_delay > 30 | arr_delay > 30}]
nyc14[, .(del30_pct = 100 * mean(delay30)), by = carrier]

## find % of flights with delays of 30 minutes or more by carrier and origin.

### use a list to specify multiple grouping variables / (default)  
nyc14[, .(del30_pct = 100 * mean(delay30)),  by = .(carrier, origin)]

#### character vector to specify multiple grouping variables / (programming)
nyc14[, .(del30_pct = 100 * mean(delay30)),  c("carrier", "origin")]

# using the keyby argument: ---------------------------------------------------
delay_pct1 = nyc14[, .(del30_pct = 100 * mean(delay30)), by = carrier]
key(delay_pct1)

delay_pct2 = nyc14[, .(del30_pct = 100 * mean(delay30)), keyby = carrier]
key(delay_pct2)

cbind(delay_pct1, delay_pct2)

# chaining: -------------------------------------------------------------------

## Find max departure delay by flight among all flights from LGA to DTW
## Then, select flights within the shortest 10% of max_delay
nyc14[origin == 'LGA' & dest == 'DTW', 
      .(max_delay = max(dep_delay)), 
      by = .(carrier, flight)
  ][, .(carrier, flight, max_delay, max_delay_q10 = quantile(max_delay, .1)) 
  ][max_delay < max_delay_q10, -"max_delay_q10", with = FALSE
  ]

## using pipes
nyc14[origin == 'LGA' & dest == 'DTW', 
      .(max_delay = max(dep_delay)), 
      by = .(carrier, flight)] %>%
  .[, .(carrier, flight, max_delay, max_delay_q10 = quantile(max_delay, .1))
   ] %>%
  .[max_delay < max_delay_q10, -"max_delay_q10", with = FALSE]



