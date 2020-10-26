# Timing comparison for sums: -------------------------------------------------
library(tidyverse)
library(microbenchmark)

## Not vectorized
x = -5e2:5e2

s0 = 0
for ( i in 1:length(x) ) {
  s0 = s0 + x[i]
}

## Vectorized
s1 = sum(x)

## Do we get the same sum?
s0 == s1

# Functions to compare: -------------------------------------------------------
f0 = function(n) {
  # Explicit looping with indexing
  x = 0:n
  s0 = 0
  for ( i in 1:length(x) ) {
    s0 = s0 + x[i]
  }
  s0
}

f1 = function(n) {
  # Explicit looping without indexing
  s0 = 0
  for ( i in 1:n ) {
    s0 = s0 + i
  }
  s0
}

f2 = function(n){
  # Call sum
  x = 0:n
  s0 = sum(x)
  s0
}

# Units here should be nanoseconds
cap = paste0(
  '**Demonstration 2.**',
  '*Comparing the run times of loops to a vectorized sum.*'
)
microbenchmark( f0 = f0(1e4), f1 = f1(1e4), f2 = f2(1e4), times = 1e3 ) %>% 
  summary() %>%
  knitr::kable( digits = 1, caption = cap, format = 'html' ) %>%
  kableExtra::kable_styling(full_width = TRUE)

## simulation parameters
n = 1e4
df = 3
quantiles = c(-1.96, 1.96)

## simulate data and draw a historgram
dat = rt(n, df)
hist(dat, prob = TRUE, las = 1, col = 'darkgreen', 
     breaks = function(x) seq(min(x) - 1, max(x) + 1, 1),
     ylim = c(0, ceiling(50 * dt(0, df)) / 50)
)
## distribution we drew from
curve(dt(x, df = df), -10, 10, add = TRUE, col = 'red')
abline(v = quantiles, col = 'red')


n = 1e4      # number of Monte Carlo samples
x = rnorm(n) # Monte Carlo sample
mean({sin(x) - cos(x)}^2) # estimate

integrand = function(x) {
  {sin(x) - cos(x)}^2 * dnorm(x)
}
integrate(integrand, -Inf, Inf)

cap = "**Figure 1.** *Illustrating why E[sin(X)] = 0 for X ~ N(0,1).*"
curve(sin, -2 * pi, 2 * pi, n = 1e3 + 1, lwd = 2, las = 1, xaxt = 'n')
curve(dnorm, -2 * pi, 2 * pi, n = 1e3 + 1, lwd = 2, col = 'red', add = TRUE)
curve(dnorm(x) * sin(x), -2 * pi, 2 * pi, n = 1e3 + 1, 
      lwd = 2, col = 'blue', add = TRUE)
abline(h = 0, v = c(-pi, 0, pi), lty = 'dashed', col = 'grey')

legend('topright', col = c('black', 'red', 'blue'), 
       lwd = 2, bty = 'n', cex = 1.2,
       legend = c('sin(x)', expression(phi*'(x)'), expression(phi*'(x)sin(x)'))
)

axis(1, at = pi * seq(-2, 2, 2), 
     labels = c(expression("-2" * pi), 0, expression("2" * pi)) )
axis(1, at = pi * seq(-2, 2, .5), labels = FALSE)




mcrep = 10000                 # Simulation replications
n = 30                        # Sample size we are studying
xmat = rexp(n * mcrep)        # Simulate standard exponential data
dim(xmat) = c(mcrep, n)       # Each row is a dataset
mn = apply(xmat, 1, mean)     # Sample mean of each row
std = apply(xmat, 1, sd)      # Sample SD of each row
se = std / sqrt(n)            # Standard errors of the mean
conf_level = .95              # The nominal confidence level
m = qt( 1 - {1 - conf_level} / 2, df = n - 1) # Multiplier for confidence level
# m ~= 2.04
lcb = mn - m * se             # lower confidence bounds
ucb = mn + m * se             # upper confidence bounds
target = 1                    # value we are estimating
cvrg_prob = mean( {lcb < target} & {ucb > target} ) # coverage probability
print(cvrg_prob)



m = 1e5
n = 30
xmat = rnorm(m * n)
dim(xmat) = c(m, n)

tm1 = proc.time()
rowvar1 = apply(xmat, 1, var)
tm1 = proc.time() - tm1

## Time a vectorized approach
tm2 = proc.time()
rowvar2 = 
  rowSums( {xmat - rowMeans(xmat)}^2 ) / {dim(xmat)[2] - 1} 
tm2 = proc.time() - tm2

## Report on the difference
cat("Apply: ", tm1[3], "s, Vectorized: ", tm2[3], "s, Ratio: ",
    round( tm1[3] / tm2[3], 1), '.\n', sep = '' )














