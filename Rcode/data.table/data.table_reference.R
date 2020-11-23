# Reference Semantics in data.table
# Stats 506, Fall 2020
#
# Covered in this example:
#   
# Author: James Henderson
# Updated: November 9, 2020
# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(data.table)

# NYC 14 flights data: --------------------------------------------------------
url = 
  'https://github.com/arunsrinivasan/flights/wiki/NYCflights14/flights14.csv'
nyc14 = fread(url)
class(nyc14)

# Truncate all arrival delays at 0: -------------------------------------------
range(nyc14$arr_delay)
nyc14[, range(arr_delay)]

# Address for nyc14 location in memory
tracemem(nyc14$arr_delay)

nyc14[arr_delay < 0, arr_delay := 0]
range(nyc14$arr_delay)
nyc14[, range(arr_delay)]

# Show memory location
tracemem(nyc14$arr_delay)

# Question (pause and think first): -------------------------------------------
tracemem(nyc14[, 'arr_delay'])
tracemem(nyc14[, .(arr_delay)])
nyc14[, tracemem(arr_delay)]

# Create a deep copy by converting to data.frame: -----------------------------
nyc_df = as.data.frame(nyc14)
tracemem(nyc_df$arr_delay)
tracemem(nyc_df)
nyc_df$arr_delay[which(nyc_df$arr_delay < 0)] = 0
tracemem(nyc_df$arr_delay)

# adding columns by reference: ------------------------------------------------

## compute maximum delay by carrier and flight
nyc14[, max_dep_delay := max(dep_delay), by = .(carrier, flight)][]

## compute maximum and minimum delay by carrier and flight
nyc14[, 
      `:=`(max_dep_delay = max(dep_delay),
           min_dep_delay = min(dep_delay)        
      ),
      by = .(carrier, flight)
][]

## delete by changing reference to NULL
nyc14[, c("month") := NULL]
nyc14[, day := NULL]

