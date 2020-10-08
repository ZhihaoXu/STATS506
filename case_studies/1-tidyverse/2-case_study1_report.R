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










#+ libraries
# libraries: ------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
})

#+ directory, output=FALSE, message=FALSE
# directories: ----------------------------------------------------------------
path = './data'

#+ data, output=FALSE, message=FALSE, warning=FALSE

# data: -----------------------------------------------------------------------

## 2009 RECS data
recs_09_file = sprintf('%s/recs2009_public.csv', path)
recs_09 = read_delim( recs_09_file, delim = ',' ) %>%
  select( id = DOEID, division = DIVISION, w = NWEIGHT,
          ur = UR, tv_num = TVCOLOR, tv_type = TVTYPE1)
## 2015 RECS data
recs_15_file = sprintf('%s/recs2015_public_v4.csv', path)
recs_15 = read_delim( recs_15_file, delim = ',' ) %>%
  select( id = DOEID, division = DIVISION, w = NWEIGHT,
          ur = UATYP10, tv_num = TVCOLOR, tv_type = TVTYPE1)
## 2009 codebook
cb_09_file = sprintf('%s/recs2009_public_codebook.xlsx', path)
codebook_09 = readxl::read_xlsx(cb_09_file, skip = 1) %>% 
  select(1:4) %>%
  filter(!is.na(`Variable Name`))
## 2015 codebook
cb_15_file = sprintf('%s/codebook_publicv4.xlsx', path)
codebook_15 = readxl::read_xlsx(cb_15_file, skip = 3) %>% 
  select(1:6) %>%
  filter(!is.na(`SAS Variable Name`))
## 2009 RECS replicate weights
wt_file = sprintf('%s/recs2009_public_repweights.csv', path)
rep_weights09 = read_delim(wt_file, delim = ',') %>%
  rename(id = DOEID) %>%
  select(-NWEIGHT)
## 2015 RECS replicate weights
recs15 = read_delim(file=recs_15_file, delim = ',') 
brrnames = names(recs15)[grepl("^BRRWT", names(recs15))]
rep_weights15 = recs15 %>%
  select(id = DOEID,
         all_of(brrnames))


#+ data cleaning
# data cleaning: --------------------------------------------------------------
## variables of interest for recs2009
variables_09 = c(division = 'DIVISION', ur = 'UR', tv_type = 'TVTYPE1')
codes_09 = codebook_09 %>%
  filter(`Variable Name` %in% variables_09) %>%
  transmute(
    variable = `Variable Name`,
    levels = 
      stringr::str_split(`Response Codes and Labels`, pattern = '\\r\\n'),
    labels =  stringr::str_split(`...4`, pattern = '\\r\\n')
  )
## variables of interest for recs2015
variables_15 = c(division = 'DIVISION', ur = 'UATYP10', tv_type = 'TVTYPE1')
codes_15 = codebook_15 %>%
  filter(`SAS Variable Name` %in% variables_15) %>%
  transmute(
    variable = `SAS Variable Name`,
    levels = 
      stringr::str_split(`...5`, pattern = '\\r\\n'),
    labels =  stringr::str_split(`Final Response Set`, pattern = '\\r\\n')
  )
codes_09[1,] = codes_15[1,]
codes_09[3,] = codes_15[3,]

# This function is directly copied from the tidyverse case study
decode_recs = function(x, varname, codes = codes) {
  # apply factor labels to variables using the codebook "codes"
  # Inputs: 
  #   x - input vector to be changed to factor
  #   varname - the name of the 'variable' in `codes`
  #   codes - a codebook of factor levels and labels
  # Output: x converted to a factor with levels given in codes
  with(filter(codes, variable == varname),
       factor(x, levels = factor(levels[[1]]), labels = labels[[1]])
  )
}

recs_09 = recs_09 %>% 
  mutate(division = decode_recs(division, 'DIVISION', codes_09),
         ur = decode_recs(ur, 'UR', codes_09),
         tv_type = decode_recs(tv_type, 'TVTYPE1', codes_09),
         id = as.double(id)
  )
recs_15 = recs_15 %>% 
  mutate(division = decode_recs(division, 'DIVISION', codes_15),
         ur = decode_recs(ur, 'UATYP10', codes_15),
         tv_type = decode_recs(tv_type, 'TVTYPE1', codes_15),
         id = as.double(id)
  )
# combine urban subdivisions for recs15: --------------------------------------
levels = with(recs_15, levels(ur))
urs = c("Urban Area",
        "Urban Cluster")
new_ur = "Urban"
levels[grep('^Urban', levels)] = new_ur
levels = unique(levels)
recs_15 = recs_15 %>%
  mutate( 
    ur = as.character(ur),
    ur = ifelse(ur %in% urs, new_ur, ur),
    ur = factor(ur, levels = levels)
  )



#+ point_estimates
## recs2009
## mean of the number of televisions by census division
mean_by_div_09 = recs_09 %>%
  group_by(division) %>%
  summarise(mean_tv = sum(tv_num * w)/sum(w), .groups = 'drop_last')
## proportion of the type of the most used television by census division
prop_by_div_09 = recs_09 %>% 
  group_by(division,tv_type) %>%
  summarise( n = n() , 
             new_w = sum(w),
             .groups = 'drop_last') %>%
  mutate( prop_type = new_w / sum(new_w) ) %>%
  select(-n,-new_w)

## mean of the number of televisions by urban/rural status
mean_by_ur_09 = recs_09 %>%
  group_by(ur) %>%
  summarise(mean_tv = sum(tv_num * w)/sum(w), .groups = 'drop_last')
## proportion of the type of the most used television by urban/rural status
prop_by_ur_09 = recs_09 %>% 
  group_by(ur,tv_type) %>%
  summarise( n = n() , 
             new_w = sum(w),
             .groups = 'drop_last') %>%
  mutate( prop_type = new_w / sum(new_w) ) %>%
  select(-n,-new_w)

## recs2015
## mean of the number of televisions by census division
mean_by_div_15 = recs_15 %>%
  group_by(division) %>%
  summarise(mean_tv = sum(tv_num * w)/sum(w), .groups = 'drop_last')
## proportion of the type of the most used television by census division
prop_by_div_15 = recs_15 %>% 
  group_by(division,tv_type) %>%
  summarise( n = n() , 
             new_w = sum(w),
             .groups = 'drop_last') %>%
  mutate( prop_type = new_w / sum(new_w) ) %>%
  select(-n,-new_w)

## mean of the number of televisions by urban/rural status
mean_by_ur_15 = recs_15 %>%
  group_by(ur) %>%
  summarise(mean_tv = sum(tv_num * w)/sum(w), .groups = 'drop_last')
## proportion of the type of the most used television by urban/rural status
prop_by_ur_15 = recs_15 %>% 
  group_by(ur,tv_type) %>%
  summarise( n = n() , 
             new_w = sum(w),
             .groups = 'drop_last') %>%
  mutate( prop_type = new_w / sum(new_w) ) %>%
  select(-n,-new_w)

#+ cis
## make rep_weights09 long format for CI
long_weights_09 = rep_weights09 %>%
  pivot_longer(
    cols = starts_with('brr'),
    names_to = 'rep',
    names_prefix = 'brr_weight_',
    values_to = 'rw'
  ) %>%
  mutate( rep = as.integer(rep) )
## make rep_weights15 long format for CI
long_weights_15 = rep_weights15 %>%
  pivot_longer(
    cols = starts_with('BRR'),
    names_to = 'rep',
    names_prefix = 'BRRWT',
    values_to = 'rw'
  ) %>%
  mutate( rep = as.integer(rep) )

plot_tv_type = function(long_weights,recs, prop_est, by_obj){
  # generate the CI table for proportion of the most used television   
  # and plot it using ggplot2
  # Inputs: 
  #   long_weights - replicate weight of target year (2009 / 2015)
  #   recs - recs data of target year (2009 / 2015)
  #   prop_est - the estimation table of proportion
  #   by_obj - specify the plot is group by 'division' or 'ur status'
  # Output: add lwr, upr and se for CI in proportion table 
  year = deparse(substitute(long_weights))
  year = paste0("20",substr(year,nchar(year)-1,nchar(year)))
  prop_repl = recs %>%
    left_join(long_weights, by = 'id') %>%
    group_by((!!as.name(by_obj)), rep, tv_type) %>%
    summarise( n = n() , 
               new_w = sum(rw),
               .groups = 'drop_last')  %>%
    mutate( prop_repl = new_w / sum(new_w) ) %>%
    select(-n,-new_w) %>%
    ungroup()
  
  fay = 0.5
  prop_var = prop_repl %>%
    left_join(prop_est, by = c(by_obj, 'tv_type')) %>%
    group_by((!!as.name(by_obj)), tv_type) %>%
    summarize(v = mean( {prop_repl - prop_type}^2 ) / { {1 - fay}^2 }, 
              .groups = 'drop')
  
  prop_est = prop_est %>%
    left_join(prop_var, by = c(by_obj, 'tv_type'))
  
  ## construct CI's
  m = qnorm(.975)
  prop_est = prop_est %>%
    mutate(
      se = sqrt(v),
      lwr = pmax(prop_type - m * se, 0),
      upr = pmin(prop_type + m * se, 1)
    ) %>%
    select(-v, se)
  
  p = prop_est  %>%
    group_by((!!as.name(by_obj)),tv_type) %>% 
    ggplot(aes(x = prop_type, y = (!!as.name(by_obj)))) +
    geom_point(aes(color = tv_type), position = position_dodge(0.3)) +
    geom_errorbarh( aes(xmin = lwr, xmax = upr, color=tv_type), 
                    position = position_dodge(0.3)) + 
    xlab('Proportion of Type for Most Used Television') +
    theme_bw()
  pic_name = paste('ps2_q1_prop_', by_obj, "_", year, ".png", sep="")
  ggsave(pic_name, width = 8,height = 4)
  print(p)
  return(prop_est)
}

plot_tv_num = function(long_weights,recs, mean_est, by_obj){
  # generate the CI table for the mean number of televisions   
  # and plot it using ggplot2
  # Inputs: 
  #   long_weights - replicate weight of target year (2009 / 2015)
  #   recs - recs data of target year (2009 / 2015)
  #   mean_est - the estimation table of mean value of TV number
  #   by_obj - specify the plot is group by 'division' or 'ur status'
  # Output: add lwr, upr and se for CI in mean estimate table 
  year = deparse(substitute(long_weights))
  year = paste0("20",substr(year,nchar(year)-1,nchar(year)))
  mean_repl = recs %>%
    left_join(long_weights, by = 'id') %>%
    group_by((!!as.name(by_obj)), rep) %>%
    transmute( mean_repl = sum(rw * tv_num)/ sum(rw) ) %>%
    ungroup()
  
  fay = 0.5
  mean_var = mean_repl %>%
    left_join(mean_est, by = by_obj) %>%
    group_by((!!as.name(by_obj))) %>%
    summarize(v = mean( {mean_repl - mean_tv}^2 ) / { {1 - fay}^2 }, 
              .groups = 'drop')
  
  mean_est = mean_est %>%
    left_join(mean_var, by = by_obj)
  
  ## construct CI's
  m = qnorm(.975)
  mean_est = mean_est %>%
    mutate(
      se = sqrt(v),
      lwr = pmax(mean_tv - m * se, 0),
      upr = mean_tv + m * se
    ) %>%
    select(-v, se)
  
  p = mean_est  %>%
    group_by((!!as.name(by_obj))) %>% 
    ggplot(aes(x = mean_tv, y = (!!as.name(by_obj)))) +
    geom_point(position = position_dodge(0.3)) +
    geom_errorbarh( aes(xmin = lwr, xmax = upr), 
                    position = position_dodge(0.3)) + 
    xlab('Mean number of Televisions') +
    theme_bw()
  pic_name = paste('ps2_q1_mean_', by_obj, "_", year, ".png", sep="")
  ggsave(pic_name, width = 8,height = 4)
  print(p)
  return(mean_est)
}

#+ r change
plot_diff_tv_num = function(data_09, data_15, by_obj){
  # generate the CI table for the change of mean number of television
  # from 2009 to 2015 and plot it using ggplot2
  # Inputs: 
  #   data_09 - the estimate table of 2009
  #   data_15 - the estimate table of 2015
  #   by_obj - specify the plot is group by 'division' or 'ur status'
  # Output: a new table with the estimated change of mean number of televisions
  # from 2009 to 2015 and together with lwr, upr for CI.
  m = qnorm(.975)
  diff_by_div = data_09 %>%
    left_join(data_15, by=by_obj) %>%
    group_by((!!as.name(by_obj))) %>%
    transmute(
      dif = mean_tv.y - mean_tv.x,
      se = sqrt(se.x^2 + se.y^2),
      lwr = dif - m * se,
      upr = dif + m * se
    ) %>%
    select(-se)
  
  p = diff_by_div  %>%
    group_by((!!as.name(by_obj))) %>% 
    ggplot(aes(x = dif, y = (!!as.name(by_obj)))) +
    geom_point( position = position_dodge(0.3)) +
    geom_vline(xintercept = 0, col = "blue", linetype="dashed")  + 
    geom_errorbarh( aes(xmin = lwr, xmax = upr), 
                    position = position_dodge(0.3)) + 
    xlab('Proportion of Type for Most Used Television') +
    theme_bw()
  pic_name = paste('ps2_q1_mean_diff_', by_obj,".png", sep="")
  ggsave(pic_name, width = 8,height = 4)
  
  print(p)
  return(diff_by_div)
}

plot_diff_tv_type = function(data_09, data_15, by_obj){
  # generate the CI table for the difference of proportion of the most used 
  # television from 2009 to 2015 and plot it using ggplot2
  # Inputs: 
  #   data_09 - the estimate table of 2009
  #   data_15 - the estimate table of 2015
  #   by_obj - specify the plot is group by 'division' or 'ur status'
  # Output: a new table with the estimated change of proportion from 
  #   2009 to 2015 and together with lwr, upr for CI.
  m = qnorm(.975)
  diff_by_div = data_09 %>%
    left_join(data_15, by=c(by_obj,"tv_type")) %>%
    transmute(
      tv_type = tv_type,
      dif = prop_type.y - prop_type.x,
      se = sqrt(se.x^2 + se.y^2),
      lwr = dif - m * se,
      upr = dif + m * se
    ) %>%
    select(-se)
  
  p = diff_by_div  %>%
    group_by((!!as.name(by_obj)),tv_type) %>% 
    ggplot(aes(x = dif, y = (!!as.name(by_obj)))) +
    geom_point(aes(color = tv_type), position = position_dodge(0.3)) +
    geom_vline(xintercept = 0, col = "black", linetype="dashed") + 
    geom_errorbarh( aes(xmin = lwr, xmax = upr, color=tv_type), 
                    position = position_dodge(0.3)) + 
    xlab('Proportion of Type for Most Used Television') +
    theme_bw()
  pic_name = paste('ps2_q1_prop_diff_', by_obj,".png", sep="")
  ggsave(pic_name, width = 8,height = 4)
  print(p)
  return(diff_by_div)
}

