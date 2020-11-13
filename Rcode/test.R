library(tidyverse)
data = read.delim("../data/nhanes.csv", sep = ",", header=T)
  
data %>% 
  group_by(size) %>%
  transmute(
    meanMur = mean(murderRate)
  )

ps.formula <- as.formula(diabete~ relative_heart_attack+gender+age+race+edu+annual_income+bmi+
                          smoke_life+phy_vigorous+phy_moderate+blood_press+blood_press2+hyper_med+hbq_med+
                          high_chol+meadial_access+cover_hc +health_diet+year_smoke+year_hyper)
ps.fit <- svyglm(ps.formula,family="binomial", data=data,iter=1000,  weights=data$weight)
data$ps <- predict(ps.fit, type="response")
require(MatchIt)
set.seed(123)
match.obj <- matchit(ps.formula, data = data,
                     distance = 'mahalanobis', 
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
