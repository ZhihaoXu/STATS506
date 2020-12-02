## Stats 506, Fall 2020 Problem Set 5 Question 1
##
## Author: Zhihao Xu, xuzhihao@umich.edu
## Updated: December 1, 2020 
# 79: -------------------------------------------------------------------------
# libraries: ------------------------------------------------------------------
library(tidyverse)
library(data.table)

# directories: ----------------------------------------------------------------
path = './data/'

# data: -----------------------------------------------------------------------
## 2009 RECS data
recs_09 = fread(paste0(path,'recs2009_public.csv')) %>%
  .[,.(id = DOEID, division = DIVISION, w = NWEIGHT, 
       ur = UR, air = AIRCOND)]
brr_names_09 = paste0('brr_weight_',1:244)
brr_09 = fread(paste0(path,'recs2009_public_repweights.csv')) %>%
  .[,-c("NWEIGHT"), with = FALSE] %>%
  .[,.(id=DOEID), brr_names_09]
## 2015 RECS data
recs_15 = fread(paste0(path,'recs2015_public_v4.csv')) %>%
  .[,.(id = DOEID, division = DIVISION, w = NWEIGHT, 
       ur = UATYP10, air = AIRCOND)]
recs_15[, ur:=ifelse(ur=='R','R','U')]
brr_names_15 = paste0('BRRWT',1:96)
brr_15 = fread(paste0(path,'recs2015_public_v4.csv')) %>%
  .[,.(id=DOEID), brr_names_15]


## proportion of homes with air conditioning equipment in each group for recs 09
mean_air_ur_09 = 
  recs_09[ , .(mean_air = sum(air * w) / sum(w)), keyby = .(division, ur)]

brr_long_09 = melt(brr_09, id.vars = 'id', 
                   measure.vars = brr_names_09,
                   value.name = 'w', variable.name = 'repl')

mean_air_ur_repl_09 = 
  merge(brr_long_09, 
        recs_09[,.(id, air, division, ur)],
        by = 'id') %>%
  .[ , .(mean_air_repl = sum(air * w) / sum(w)), 
     keyby = .(division, repl, ur)]

mean_air_ur_09 = 
  merge( mean_air_ur_09, 
         mean_air_ur_repl_09[, !"repl"], 
         by = c('division', 'ur') 
  ) %>%
  .[ , .( mean_air = mean_air[1],
          se = 2 * sqrt(mean({mean_air_repl - mean_air}^2))
  ), .(division, ur) ]  

m = qnorm(.975)
mean_air_ur_09[ , `:=`(lwr = pmax(0, mean_air - m * se),
                       upr = pmin(1, mean_air + m * se))]

## proportion of homes with air conditioning equipment in each group for recs 15
mean_air_ur_15 = 
  recs_15[ , .(mean_air = sum(air * w) / sum(w)), keyby = .(division, ur)]

brr_long_15 = melt(brr_15, id.vars = 'id', measure.vars = brr_names_15,
                    value.name = 'w', variable.name = 'repl')

mean_air_ur_repl_15 = 
  merge(brr_long_15, 
        recs_15[,.(id, air, division, ur)],
        by = 'id') %>%
  .[ , .(mean_air_repl = sum(air * w) / sum(w)), 
     keyby = .(division, repl, ur)]

mean_air_ur_15 = 
  merge( mean_air_ur_15, 
         mean_air_ur_repl_15[, !"repl"], 
         by = c('division', 'ur') 
  ) %>%
  .[ , .( mean_air = mean_air[1],
          se = 2 * sqrt(mean({mean_air_repl - mean_air}^2))
  ), .(division, ur) ]  

m = qnorm(.975)
mean_air_ur_15[ , `:=`(lwr = pmax(0, mean_air - m * se),
                    upr = pmin(1, mean_air + m * se))]


mean_air_ur_dif = 
  merge(mean_air_ur_15,
        mean_air_ur_09,
        by=c('division','ur')
  ) %>%
  .[, .(dif = mean_air.x - mean_air.y,
        se = sqrt(se.x^2 + se.y^2)
  ), .(division, ur) ]  
mean_air_ur_dif[ , `:=`(lwr = pmax(-1, dif - m * se),
                        upr = pmin(1, dif + m * se),
                        se=NULL)]

## order
divisions = c(
  "New England",
  "Middle Atlantic",
  "East North Central",
  "West North Central",
  "South Atlantic",
  "East South Central",
  "West South Central",
  "Mountain North",
  "Mountain South",
  "Pacific")
urs = c('Rural', 'Urban')

mean_air_ur_dif[ , `:=`(division = factor(division, rep(1:10,each=2), 
                                          labels = divisions[rep(1:10,each=2)]),
                        ur = factor(ur, 
                                    levels = rep(c('R','U'),10), 
                                    labels=urs[rep(1:2,10)]))]
mean_air_ur_09[ , `:=`(division = factor(division, rep(1:10,each=2), 
                                         labels = divisions[rep(1:10,each=2)]),
                       ur = factor(ur, 
                                   levels = rep(c('R','U'),10), 
                                   labels=urs[rep(1:2,10)]))]
mean_air_ur_15[ , `:=`(division = factor(division, rep(1:10,each=2), 
                                         labels = divisions[rep(1:10,each=2)]),
                       ur = factor(ur, 
                                   levels = rep(c('R','U'),10), 
                                   labels=urs[rep(1:2,10)]))]
setkey(mean_air_ur_dif, division)
setkey(mean_air_ur_09, division)
setkey(mean_air_ur_15, division)

tab09 =
  mean_air_ur_09[ , .(division, ur,
                       ci = sprintf('<center>%4.1f <br>(%4.1f, %4.1f)</center>', 
                                    100 * mean_air, 100 * lwr, 100 * upr) )
  ] %>%
  dcast(., division ~ ur, value.var = 'ci' )

setnames(tab09, c('Census Division', 
                  'Rural, % <br> (95% CI)',
                  'Urban, % <br> (95% CI)'))

tab15 =
  mean_air_ur_15[ , .(division, ur,
                       ci = sprintf('<center>%4.1f <br>(%4.1f, %4.1f)</center>', 
                                    100 * mean_air, 100 * lwr, 100 * upr) )
  ] %>%
  dcast(., division ~ ur, value.var = 'ci' )

setnames(tab15, c('Census Division', 
                   'Rural, % <br> (95% CI)',
                   'Urban, % <br> (95% CI)'))

tabdif =
  mean_air_ur_dif[ , .(division, ur,
                        ci = sprintf('<center>%4.1f <br>(%4.1f, %4.1f)</center>', 
                                     100 * dif, 100 * lwr, 100 * upr) )
  ] %>%
  dcast(., division ~ ur, value.var = 'ci' )

setnames(tabdif, c('Census Division', 
                 'Rural, % <br> (95% CI)',
                 'Urban, % <br> (95% CI)'))

# DT::datatable(tab15, escape = FALSE)

p1 = 
  mean_air_ur_09 %>%
  rename( rurality = ur ) %>%
  ggplot( aes( x = division, color = rurality) ) +
  geom_point( aes(y = mean_air), position = position_dodge(width = 0.5) ) +
  geom_errorbar( aes(ymin = lwr, ymax = upr), 
                 position = position_dodge(width = 0.5) ) +
  theme_bw() +
  scale_color_manual( values = c("darkred", "darkblue")) +
  ylab("Proportion of Homes with Air Conditioning Equipment.") +
  xlab("Census Division") +
  coord_flip() 
cap1 = paste(
  '**Figure 1.** *Estimated Proportion of Homes with Air Conditioning ',
  'Equipment by rurality in each Census Division in 2009.*')

p2 = 
  mean_air_ur_15 %>%
  rename( rurality = ur ) %>%
  ggplot( aes( x = division, color = rurality) ) +
  geom_point( aes(y = mean_air), position = position_dodge(width = 0.5) ) +
  geom_errorbar( aes(ymin = lwr, ymax = upr), 
                 position = position_dodge(width = 0.5) ) +
  theme_bw() +
  scale_color_manual( values = c("darkred", "darkblue")) +
  ylab("Proportion of Homes with Air Conditioning Equipment.") +
  xlab("Census Division") +
  coord_flip() 
cap2 = paste(
  '**Figure 2.** *Estimated Proportion of Homes with Air Conditioning ',
  'Equipment by rurality in each Census Division in 2015.*')

p3 = 
  mean_air_ur_dif %>%
  rename( rurality = ur ) %>%
  ggplot( aes( x = division, color = rurality) ) +
  geom_point( aes(y = dif), position = position_dodge(width = 0.5) ) +
  geom_errorbar( aes(ymin = lwr, ymax = upr), 
                 position = position_dodge(width = 0.5) ) +
  theme_bw() +
  scale_color_manual( values = c("darkred", "darkblue")) +
  ylab("Proportion of Homes with Air Conditioning Equipment.") +
  xlab("Census Division") +
  coord_flip() 
cap3 = paste(
  '**Figure 3.** *Estimated Change of Proportion of Homes with Air Conditioning ',
  'Equipment by rurality in each Census Division from 2009 to 2015.*')



#! Keep this as a reference for code length at 
#! the top and bottom of your scripts. 
# 79: -------------------------------------------------------------------------
