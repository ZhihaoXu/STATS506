suppressPackageStartupMessages({
  library(tidyverse); library(data.table)
})
set.seed(42)
iris = iris[sample(1:nrow(iris), 10, replace = FALSE), ] %>% 
  as.data.table()
iris
iris10 = iris

iris10[, .(avg_sl = mean(Sepal.Length)), by = Species]

iris10[, mpl := max(Petal.Length), .(Species)]
iris10[Petal.Length == mpl, .(Species, Petal.Length)]

iris10[Petal.Length == max(Petal.Length), .(Species, Petal.Length)]

iris10[, `:=`(Sepal.Ratio = Sepal.Length / Sepal.Width)]
iris10[, `:=`(Thin = Sepal.Ratio > 2)]
iris10[ Thin == FALSE ] %>%
  .[, .SD, .SDcols = grep('^Sepal', names(iris10), value = TRUE)]


iris10[, 
       .(Species,
         Sepal.Ratio = Sepal.Length / Sepal.Width,
         Petal.Ratio = Petal.Length / Petal.Width
       )][, lapply(.SD, mean), Species]


iris10[, .N, c("Species")]
