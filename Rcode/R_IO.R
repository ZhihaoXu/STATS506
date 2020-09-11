recs = read.table(
  'data/recs2015_public_v4.csv',
  sep = ",",
  stringsAsFactors = FALSE,
  header = TRUE
)

# inspect
dim(recs)
class(recs)
head(names(recs))

#output
write.table(
  lme4::InstEval, 
  file = '../../data/InstEval.txt', 
  sep = '\t', 
  row.names = FALSE
)

## (Tidyverse) readR
recs_tib = readr::read_delim(
  'data/recs2015_public_v4.csv',
  delim=','
)
dim(recs_tib)
class(recs_tib)


##
recs_dt = data.table::fread("data/recs2015_public_v4.csv")
#recs_dt = 
#   data.table::fread('gunzip -c ../../data/recs2015_public_v4.csv.gz')

