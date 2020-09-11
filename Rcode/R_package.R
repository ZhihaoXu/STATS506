library(tidyverse)
library(data.table)
library(lme4)
library(MASS)

install.packages("devtools")
devtools::install_github("tidyverse/dplyr")


.libPaths() 

search()
data(package="lme4")

fm1 = lme4::lmer(Reaction ~Days + (Days | Subject),sleepstudy)
anova(fm1)
anova
lme4:::anova.merMod(fm1)

MASS::select()
