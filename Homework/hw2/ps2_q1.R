## Stats 506, Fall 2020 Problem Set 2 Question 1
##
## Author: Zhihao Xu, xuzhihao@umich.edu
## Updated: October 9, 2020 
## The code in data cleaning process refers to the code used in
## tidyverse case study.
# 79: -------------------------------------------------------------------------
# libraries: ------------------------------------------------------------------
library(tidyverse)
library(kableExtra)

# directories: ----------------------------------------------------------------
path = './data'

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


# Problem (a)
## recs2009
## mean of the number of televisions by census division and ur status
mean_09 = recs_09 %>%
  group_by(division, ur) %>%
  summarise(mean_tv = sum(tv_num * w)/sum(w), .groups = 'drop_last')


## proportion of the type of the most used television 
## by census division and ur status
prop_09 = recs_09 %>% 
  group_by(division, ur, tv_type) %>%
  summarise( n = n() , 
             new_w = sum(w),
             .groups = 'drop_last') %>%
  mutate( prop_type = new_w / sum(new_w) ) %>%
  select(-n,-new_w)

# Problem (b)
## recs2015
## mean of the number of televisions by census division and ur status
mean_15 = recs_15 %>%
  group_by(division, ur) %>%
  summarise(mean_tv = sum(tv_num * w)/sum(w), .groups = 'drop_last')

## proportion of the type of the most used television 
## by census division and ur status
prop_15 = recs_15 %>% 
  group_by(division, ur, tv_type) %>%
  summarise( n = n() , 
             new_w = sum(w),
             .groups = 'drop_last') %>%
  mutate( prop_type = new_w / sum(new_w) ) %>%
  select(-n,-new_w)




# Problem (c)
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


plot_tv_type = function(long_weights,recs, prop_est){
  # generate the CI table for proportion of the most used television   
  # and plot it using ggplot2
  # Inputs: 
  #   long_weights - replicate weight of target year (2009 / 2015)
  #   recs - recs data of target year (2009 / 2015)
  #   prop_est - the estimation table of proportion
  # Output: add lwr, upr and se for CI in proportion table 
  year = deparse(substitute(long_weights))
  year = paste0("20",substr(year,nchar(year)-1,nchar(year)))
  prop_repl = recs %>%
    left_join(long_weights, by = 'id') %>%
    group_by(division, ur, rep, tv_type) %>%
    summarise( n = n() , 
               new_w = sum(rw),
               .groups = 'drop_last')  %>%
    mutate( prop_repl = new_w / sum(new_w) ) %>%
    select(-n,-new_w) %>%
    ungroup()
  
  fay = 0.5
  prop_var = prop_repl %>%
    left_join(prop_est, by = c('division', 'ur', 'tv_type')) %>%
    group_by(division, ur, tv_type) %>%
    summarize(v = mean( {prop_repl - prop_type}^2 ) / { {1 - fay}^2 }, 
              .groups = 'drop')
  
  prop_est = prop_est %>%
    left_join(prop_var, by = c('division', 'ur', 'tv_type'))
  
  ## construct CI's
  m = qnorm(.975)
  prop_est = prop_est %>%
    mutate(
      se = sqrt(v),
      lwr = pmax(prop_type - m * se, 0),
      upr = pmin(prop_type + m * se, 1),
    ) %>%
    select(-v, se)
  
  p = prop_est  %>%
    group_by(division, ur,tv_type) %>% 
    ggplot(aes(x = prop_type, y = division, shape = ur)) +
    geom_point(aes(color = tv_type), position = position_dodge(0.3)) +
    geom_errorbarh( aes(xmin = lwr, xmax = upr, color=tv_type), 
                    position = position_dodge(0.3)) + 
    facet_grid(tv_type~ur) + 
    xlab('Proportion of Type for Most Used Television') +
    theme_bw()
  pic_name = paste('./pic/ps2_q1_prop_', year, ".png", sep="")
  ggsave(pic_name, width = 8,height = 4)
  print(p)
  return(prop_est)
}

plot_diff_tv_type = function(data_09, data_15){
  # generate the CI table for the difference of proportion of the most used 
  # television from 2009 to 2015 and plot it using ggplot2
  # Inputs: 
  #   data_09 - the estimate table of 2009
  #   data_15 - the estimate table of 2015
  # Output: a new table with the estimated change of proportion from 
  #   2009 to 2015 and together with lwr, upr for CI.
  m = qnorm(.975)
  diff_tv_type = data_09 %>%
    left_join(data_15, by=c('division', 'ur', "tv_type")) %>%
    transmute(
      tv_type = tv_type,
      dif = prop_type.y - prop_type.x,
      se = sqrt(se.x^2 + se.y^2),
      lwr = dif - m * se,
      upr = dif + m * se
    ) %>%
    drop_na() %>%
    select(-se)
  
  p = diff_tv_type  %>%
    group_by(division, ur, tv_type) %>% 
    ggplot(aes(x = dif, y = division, shape = ur)) +
    geom_point(aes(color = tv_type), position = position_dodge(0.3)) +
    geom_vline(xintercept = 0, col = "black", linetype="dashed") + 
    geom_errorbarh( aes(xmin = lwr, xmax = upr, color=tv_type), 
                    position = position_dodge(0.3)) + 
    facet_grid(tv_type~ur) + 
    xlab('Proportion of Type for Most Used Television') +
    theme_bw()
  pic_name = paste('./pic/ps2_q1_prop_diff',".png", sep="")
  ggsave(pic_name, width = 8,height = 4)
  print(p)
  return(diff_tv_type)
}


plot_tv_num = function(long_weights,recs, mean_est){
  # generate the CI table for the mean number of televisions   
  # and plot it using ggplot2
  # Inputs: 
  #   long_weights - replicate weight of target year (2009 / 2015)
  #   recs - recs data of target year (2009 / 2015)
  #   mean_est - the estimation table of mean value of TV number
  # Output: add lwr, upr and se for CI in mean estimate table 
  year = deparse(substitute(long_weights))
  year = paste0("20",substr(year,nchar(year)-1,nchar(year)))
  mean_repl = recs %>%
    left_join(long_weights, by = 'id') %>%
    group_by(division, ur, rep) %>%
    transmute( mean_repl = sum(rw * tv_num)/ sum(rw) ) %>%
    ungroup()
  
  fay = 0.5
  mean_var = mean_repl %>%
    left_join(mean_est, by = c('division','ur')) %>%
    group_by(division, ur) %>%
    summarize(v = mean( {mean_repl - mean_tv}^2 ) / { {1 - fay}^2 }, 
              .groups = 'drop')
  
  mean_est = mean_est %>%
    left_join(mean_var, by = c('division','ur'))
  
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
    group_by(division, ur) %>% 
    ggplot(aes(x = mean_tv, y = division, color = ur, shape = ur)) +
    geom_point(position = position_dodge(0.3)) +
    geom_errorbarh( aes(xmin = lwr, xmax = upr), 
                    position = position_dodge(0.3)) + 
    xlab('Mean of the Number of Televisions') +
    theme_bw()
  pic_name = paste('./pic/ps2_q1_mean_', year, ".png", sep="")
  ggsave(pic_name, width = 8,height = 4)
  print(p)
  return(mean_est)
}


plot_diff_tv_num = function(data_09, data_15){
  # generate the CI table for the change of mean number of television
  # from 2009 to 2015 and plot it using ggplot2
  # Inputs: 
  #   data_09 - the estimate table of 2009
  #   data_15 - the estimate table of 2015
  # Output: a new table with the estimated change of mean number of televisions
  # from 2009 to 2015 and together with lwr, upr for CI.
  m = qnorm(.975)
  diff_num = data_09 %>%
    left_join(data_15, by=c('division', 'ur')) %>%
    group_by(division, ur) %>%
    transmute(
      dif = mean_tv.y - mean_tv.x,
      se = sqrt(se.x^2 + se.y^2),
      lwr = dif - m * se,
      upr = dif + m * se
    ) %>%
    select(-se)
  
  p = diff_num  %>%
    group_by(division, ur) %>% 
    ggplot(aes(x = dif, y = division, shape = ur, color = ur)) +
    geom_point( position = position_dodge(0.3)) +
    geom_vline(xintercept = 0, col = "blue", linetype="dashed")  + 
    geom_errorbarh( aes(xmin = lwr, xmax = upr), 
                    position = position_dodge(0.3)) + 
    xlab('Mean of the Number of Televisions') +
    theme_bw()
  pic_name = paste('./pic/ps2_q1_mean_diff',".png", sep="")
  ggsave(pic_name, width = 8,height = 4)
  
  print(p)
  return(diff_num)
}

# don't run it when being sourced, if needs to run it interactively, 
# change it to TRUE
if (FALSE) {
  prop_ci_09 = plot_tv_type(long_weights_09, recs_09, prop_09)
  prop_ci_15 = plot_tv_type(long_weights_15, recs_15, prop_15)
  plot_diff_tv_type(prop_ci_09, prop_ci_15)
  
  num_ci_09 = plot_tv_num(long_weights_09, recs_09, mean_09)
  num_ci_15 = plot_tv_num(long_weights_15, recs_15, mean_15)
  plot_diff_tv_num(num_ci_09, num_ci_15)
}




#! Keep this as a reference for code length at 
#! the top and bottom of your scripts. 
# 79: -------------------------------------------------------------------------
