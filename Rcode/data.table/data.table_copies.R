# Copying a data.table by reference vs by value. 
# Stats 506, Fall 2020
#
# Author: James Henderson
# Updated: November 9, 2020
# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(data.table)

# Copies: ---------------------------------------------------------------------
DT1 = data.table(A = 5:1, B = letters[5:1])
DT2 = DT1         # Copy by reference
DT3 = copy(DT1)   # Copy by value
DT4 = DT1[, .SD]  # Value or reference? See below.

DT1[, C := 2 * A]    # Create a new column 
DT1
DT2 
DT3
DT4                # What do we learn about the DT1[,.SD] syntax? 

DT2[, D := 2 * C]
DT2
DT1

# Which addresses are the same? :----------------------------------------------
tracemem(DT1)
tracemem(DT2)
tracemem(DT3)
tracemem(DT4)
rbind(
  DT1[, lapply(.SD, tracemem), .SDcols = c('A', 'B')],
  DT2[, lapply(.SD, tracemem), .SDcols = c('A', 'B')],
  DT3[, lapply(.SD, tracemem)],
  DT4[, lapply(.SD, tracemem)]
)
