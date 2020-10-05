#' ---
#' title: "Case Study 1 (tidyverse): Report"
#' author: "James Henderson, PhD"
#' date: "`r format.Date(Sys.Date(), '%b %d, %Y')`"
#' output: 
#'   html_document:
#'     code_folding: hide
#' ---

#' ## About
#' This is a report using data from the 2009 Residential Energy Consumption
#' Survey ([RECS]())) run by the Energy Information Agency. 
#' 
#' We use this data to answer the question:
#' 
#' > Which Census Division has the highest proportion of single-family                                                                  
#'   attached homes?
#'   

#+ setup, include=FALSE
knitr::opts_chunk$set(echo = FALSE)

#+ r-header, include=TRUE
## Stats 506, F20
## Case Study 1 - tidyverse
## 
## Which Census Division has the highest proportion of single-family                                                                  
##   attached homes?  We'll use the 2009 RECS to answer this question. 
##
##  0. See 0-case_study1.R for data sources. 
##  1. clean data
##  2. compute point estimates of proportions by census division
##  3. construct CI's for the point estimates
##  4. make tables/graphics for presentation
##
## Author: James Henderson, jbhender@umich.edu
## Updated: September 21, 2020
# 79: -------------------------------------------------------------------------

#+ libraries
# libraries: ------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
})

#+ data, output=FALSE, message=FALSE
# directories: ----------------------------------------------------------------
path = './data'

# data: -----------------------------------------------------------------------

## 2009 RECS data
### doeid, division, typehuq, nweight
recs_file = sprintf('%s/recs2009_public.csv', path)
recs_min = sprintf('%s/recs_min.csv', path)
if ( !file.exists(recs_min) ) {
  recs = read_delim( recs_file, delim = ',' ) %>%
    select( id = DOEID, w = NWEIGHT, division = DIVISION, type = TYPEHUQ)
  write_delim(recs, path = , delim = ',')
} else {
  recs = read_delim(recs_min, delim = ',')
}

## 2009 RECS replicate weights
wt_file = sprintf('%s/recs2009_public_repweights.csv', path)
rep_weights = read_delim(wt_file, delim = ',') %>%
  rename(id = DOEID) %>%
  select(-NWEIGHT)

## codebook
cb_file = sprintf('%s/recs2009_public_codebook.xlsx', path)
codebook = readxl::read_xlsx(cb_file, skip = 1) %>% 
  select(1:4) %>%
  filter(!is.na(`Variable Name`))

#+ data cleaning
# data cleaning: --------------------------------------------------------------

## variables of interest
variables = c(division = 'DIVISION', type = 'TYPEHUQ')
codes = codebook %>%
  filter(`Variable Name` %in% variables) %>%
  transmute(
    variable = `Variable Name`,
    levels = 
      stringr::str_split(`Response Codes and Labels`, pattern = '\\r\\n'),
    labels =  stringr::str_split(`...4`, pattern = '\\r\\n')
  )
  
## apply labels
decode_recs = function(x, varname, codes = codes) {
  # apply factor labels to variables using the codebook "codes"
  # Inputs: 
  #   x - input vector to be changed to factor
  #   varname - the name of the 'variable' in `codes`
  #   codes - a codebook of factor levels and labels
  # Output: x converted to a factor with levels given in codes

  with(filter(codes, variable == varname),
   factor(x, levels = as.numeric(levels[[1]]), labels = labels[[1]])
  )
}

recs = recs %>% 
  mutate(division = decode_recs(division, 'DIVISION', codes),
         type = decode_recs(type, 'TYPEHUQ', codes),
         id = as.double(id)
         )

# combine mountain subdivisions: ----------------------------------------------
levels = with(recs, levels(division))
mt_divs = c("Mountain North Sub-Division (CO, ID, MT, UT, WY)",
            "Mountain South Sub-Division (AZ, NM, NV)")
new_mt_div = "Mountain Census Division (AZ, CO, ID, MT, NM, NV, UT, WY)"
levels[grep('^Moun', levels)] = new_mt_div
levels = unique(levels)
short_levels = stringr::str_split(levels, ' Census') %>%
  vapply(., function(x) x[[1]], FUN.VALUE = "z") %>%
  stringr::str_trim()

recs = recs %>%
  mutate( 
    division = as.character(division),
    division = ifelse(division %in% mt_divs, new_mt_div, division),
    division = factor(division, levels = levels)
  )

#+ point_estimates
# point estimates of housing type proportions by Census division: -------------
type_by_division = recs %>%
  group_by(division, type) %>%
  summarize( nhomes = sum(w), .groups = 'drop_last' ) %>%
  mutate( p_type = nhomes / sum(nhomes) )

#+ cis
# for CI's, make rep_weights long format: -------------------------------------
long_weights = rep_weights %>%
  pivot_longer(
    cols = starts_with('brr'),
    names_to = 'rep',
    names_prefix = 'brr_weight_',
    values_to = 'rw'
  ) %>%
  mutate( rep = as.integer(rep) )

# compute confidence intervals, using replicate weights: ----------------------

## replicate proportions
type_by_div_repl = recs %>%
  select(-w) %>%
  left_join(long_weights, by = 'id') %>%
  group_by(division, rep, type) %>%
  summarize( nhomes = sum(rw), .groups = 'drop_last' ) %>%
  mutate( p_type_repl = nhomes / sum(nhomes) ) %>%
  ungroup()

## variance of replicate proportions around the point estimate
fay = 0.5
type_by_div_var = type_by_div_repl %>%
  left_join(select(type_by_division, -nhomes), by = c('division', 'type')) %>%
  group_by(division, type) %>%
  summarize(v = mean( {p_type_repl - p_type}^2 ) / { {1 - fay}^2 }, 
            .groups = 'drop')
  
type_by_division = type_by_division %>%
  left_join(type_by_div_var, by = c('division', 'type'))

## construct CI's
m = qnorm(.975)
type_by_division = type_by_division %>%
  mutate(
   se = sqrt(v),
   lwr = pmax(p_type - m * se, 0),
   upr = pmin(p_type + m * se, 1)
  )

#filter(type_by_division, type == 'Single-Family Attached') %>%
#  arrange(desc(p_type))

#' ## Results
#' Here is what we found.
#' 
#' ### Figures {.tabset .tabset-pills .tabset-fade}

#' #### Single-Family Attached (Figure)
#+ plot_attached
# construct a plot answering the key question: --------------------------------
div_ord = {
  type_by_division %>%
  ungroup() %>%
  mutate( 
    division_cln = factor(as.numeric(division), labels = short_levels)
  ) %>%
  filter(type == 'Single-Family Attached') %>%
  arrange(p_type)
  }$division_cln %>%
  as.character()

p_attached = type_by_division %>%
  filter(type == 'Single-Family Attached') %>%
  mutate( across(all_of(c('p_type', 'lwr', 'upr')), 
                 .fns = function(x) 100 * x) 
  ) %>%
  ungroup() %>%
  mutate( 
    division_cln = factor(as.numeric(division), labels = short_levels)
  ) %>%
  mutate( 
    division_cln = factor( as.character(division_cln), levels = div_ord)
  ) %>%
  ggplot( aes(x = p_type, y = division_cln) ) +
   geom_point() +
   geom_errorbarh( aes(xmin = lwr, xmax = upr) ) + 
   theme_bw() +
   xlab('Single-Family Attached Homes (%)') +
   ylab('Census Division') +
   xlim(c(0, 12))

#+ figure1, fig.cap=cap1
cap1 = paste(
  "**Figure 1.** *Percentage of single-family attached homes among all home",
  "types by Census Division.*"
)
p_attached 

#' 
#' #### All Home Types (Figure)

#+ plot_all
# construct a plot with all available housing types: --------------------------
plot_all = type_by_division %>%
  mutate( across(all_of(c('p_type', 'lwr', 'upr')), 
                 .fns = function(x) 100 * x) 
  ) %>%
  ungroup() %>%
  mutate( 
    division_cln = factor(as.numeric(division), labels = short_levels)
  ) %>%
  mutate( 
    division_cln = factor( as.character(division_cln), levels = div_ord),
    `Housing Type` = type
  ) %>%
  ggplot( aes(x = p_type, y = division_cln, 
              color = `Housing Type`, shape = `Housing Type`) 
  ) +
   geom_point(
     position = position_dodge2(width = 0.5)
   ) +
   geom_errorbarh( 
    aes(xmin = lwr, xmax = upr),
    position = position_dodge(width = 0.5),
    height = 0.75,
    alpha = 0.75
   ) + 
   theme_bw() +
   xlab('% of Homes') +
   ylab('') + 
   scale_color_manual( 
    values = c('darkblue', 'red3', 'darkred', 'green4', 'darkgreen')
   ) +
   scale_shape_manual( values = c(18, 16, 1, 17, 2)) 

#+ figure2, fig.cap = cap2
cap2 = paste( 
  '**Figure 2.** *Percent of all house types by Census Division.*' )
plot_all

#'
#' #### All Home Types (table)

#+ table1
type_by_division %>%
  mutate( across(all_of(c('p_type', 'lwr', 'upr')), 
                 .fns = function(x) 100 * x) 
  )  %>%
  ungroup() %>%
  mutate( 
    division_cln = factor(as.numeric(division), labels = short_levels),
    division_cln = factor( as.character(division_cln), levels = div_ord)
  ) %>%
  transmute(
    `Census Division` = division_cln,
    `Housing Type` = type,
    `% of homes (95% CI)` = 
      sprintf('%04.1f%% (%04.1f, %04.1f)', p_type, lwr, upr)
  ) %>%
  DT::datatable()

# 79: -------------------------------------------------------------------------
c(80,37, 129, 108, 83, 70, 130, 118,81, 31)
