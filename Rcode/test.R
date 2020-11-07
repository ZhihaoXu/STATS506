library(tidyverse)
data = read.delim("CommViolPredUnnormalizedData.txt", sep = ",", header=F) %>%
  select(
    comminty = V1, population = V6, householdsize = V7, racepctblack = V8,
    racePctWhite = V9, racePctAsian = V10, 
    racePctHisp = V11, murderRate = V131) %>%
  mutate(
    size = ifelse(householdsize<=median(householdsize),"S", "L")
  ) 

data %>% 
  group_by(size) %>%
  transmute(
    meanMur = mean(murderRate)
  )

ps.formula <- as.formula(I(size=="L") ~ population + racepctblack+ 
                           racePctWhite + racePctAsian + racePctHisp)
ps.fit <- glm(ps.formula,family="binomial", data=data)
data$ps <- predict(ps.fit, type="response")
require(MatchIt)
set.seed(123)
match.obj <- matchit(ps.formula, data = data,
                     distance = 'logit', 
                     method = "nearest", 
                     replace=FALSE,
                     caliper = .2, 
                     ratio = 1)
matched.data <- match.data(match.obj)
matched.data %>% 
  group_by(size) %>%
  transmute(
    meanMur = mean(murderRate)
  )
