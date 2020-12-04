## Stats 506, Fall 2020 Problem Set 5 Question 2
##
## Author: Zhihao Xu, xuzhihao@umich.edu
## Updated: December 1, 2020 
# 79: -------------------------------------------------------------------------
# libraries: ------------------------------------------------------------------
library(data.table)
library(parallel) 

# directories: ----------------------------------------------------------------
path = './data/'

# data: -----------------------------------------------------------------------
ohxden = fread(paste0(path,'nhanes_ohxden.csv')) 
demo = fread(paste0(path,'nhanes_demo.csv')) 

ohxden[demo, on="SEQN"][OHDDESTS==1,]

ohx_long = melt(ohxden[demo, on="SEQN"][OHDDESTS==1,], 
                id.vars = 'SEQN', 
                measure.vars = paste0('OHX',sprintf("%02d", 1:32),"TC"),
                value.name = 'tooth_status', 
                variable.name = 'tooth')

ohx_long = demo[ohx_long, on="SEQN"
              ][,.(id = SEQN, ohx_status = 1, tooth, 
                   tooth_status, age = RIDAGEYR)
              ][order(id, tooth)]
ohx_long[, perm_tooth := (tooth_status==2)]

year = (ohx_long[,id]>= 62161) + (ohx_long[,id]>=73557) + 
       (ohx_long[,id]>=83732) + (ohx_long[,id]>=93703)
year = factor(year, levels = 1:4, 
              labels = c("2011-2012", "2013-2014", "2015-2016", "2017-2018"))
ohx_long[, year := year]

 


year_list = list("2011-2012", "2013-2014", "2015-2016", "2017-2018")
year_tooth = cbind(rep(paste0('OHX',sprintf("%02d", 1:32),"TC"),each=4), 
                   rep(c("2011-2012", "2013-2014", 
                         "2015-2016", "2017-2018"),32))
year_tooth = apply(year_tooth, 1, list)

bam_appro1 = function(y) {
  # used in mclapply function
  # implement the approach 1
  # fit the logistic regression model and predict using cross-validation
  # Inputs: 
  #   y - year chosed as validate data
  # Output: prediction of probability for validation data
  train_data = ohx_long[year!=y,]
  test_data = ohx_long[year==y,]
  m = mgcv::bam(perm_tooth ~ tooth + s(age, bs = 'cs') + s(id, bs = 're'), 
                  family=binomial(), data = train_data, discrete=TRUE)
  pred = predict(m, test_data, type="response")
  return(pred)
}
bam_appro2 = function(y) {
  # used in mclapply function
  # implement the approach 2
  # fit the logistic regression model and predict using cross-validation
  # Inputs: 
  #   y - year chosed as validate data
  # Output: prediction of probability for validation data
  train_data = ohx_long[year!=y,]
  test_data = ohx_long[year==y,]
  m = mgcv::bam(perm_tooth ~ tooth + s(age, bs = 'cs', by = tooth) 
                                   + s(id, bs = 're'), 
                family=binomial(), data = train_data, discrete=TRUE)
  pred = predict(m, test_data, type="response")
  return(pred)
}

bam_appro3 = function(y) {
  # used in mclapply function
  # implement the approach 3
  # fit the logistic regression model and predict using cross-validation
  # Inputs: 
  #   y - y[[1]] is a length 2 vector, first element is the target tooth and 
  #       second element is the year chosed as validate data
  # Output: prediction of probability for validation data
  y = y[[1]]
  train_data = ohx_long[(year!=y[2])&(tooth==y[1]),]
  test_data = ohx_long[(year==y[2])&(tooth==y[1]),]
  m = mgcv::bam(perm_tooth ~ s(age, bs = 'cs'), 
                family=binomial(), data = train_data, discrete=TRUE)
  pred = predict(m, test_data, type="response")
  return(pred)
}

order_true_y = function(y) {
  # used in mclapply function
  # reorder the true value of response to match the prediction
  # Inputs: 
  #   y - y[[1]] is a length 2 vector, first element is the target tooth and 
  #       second element is the year chosed as validate data
  # Output: true value after reording
  y = y[[1]]
  true = ohx_long[(year==y[2])&(tooth==y[1]),perm_tooth]+0
  return(true)
}

results1 = do.call("c", mclapply(year_list, bam_appro1, mc.cores = 4))
results2 = do.call("c", mclapply(year_list, bam_appro2, mc.cores = 4))
results3 = do.call("c", mclapply(year_tooth, bam_appro3, mc.cores = 4))
true_y3 = do.call("c", mclapply(year_tooth, order_true_y, mc.cores = 4))
true_y = ohx_long[,perm_tooth]+0


ce_loss1 = -mean(true_y * log(results1) + (1 - true_y) * log(1 - results1))
ce_loss2 = -mean(true_y * log(results2) + (1 - true_y) * log(1 - results2))
ce_loss3 = -mean(true_y3 * log(results3) + (1 - true_y3) * log(1 - results3))






#! Keep this as a reference for code length at 
#! the top and bottom of your scripts. 
# 79: -------------------------------------------------------------------------
