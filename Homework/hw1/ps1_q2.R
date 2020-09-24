## Stats 506, Fall 2020 Homework 1 Question 2
##
## Author: Zhihao Xu, xuzhihao@umich.edu
## Updated: September 24, 2020 - Last modified date
# 79: -------------------------------------------------------------------------

# libraries: ------------------------------------------------------------------
library(ggplot2)
# data: -----------------------------------------------------------------------
data = read.csv("isolet_results.csv")

# Compute the TP, FP, TN, FN, sensitivity and specificity used in part (a)
# Input:
#   -tau: given threshold
#   -y,yhat: true and predict value
# Output:
#   -a vector of tp, fp, tn, fn, sensitivity and specificity
compute_tf1 = function(tau,y,yhat){
  tp = sum((yhat>=tau)&(y==1))
  fp = sum((yhat>=tau)&(y==0))
  tn = sum((yhat<tau)&(y==0))
  fn = sum((yhat<tau)&(y==1))
  se = tp/(tp+fn)
  sp = tn/(fp+tn)
  return(c(tp, fp, tn, fn, se, sp))
}

# Compute the area under the curve using trapezoidal rule.
# Input:
#   - x,y: values on x-axis and y-axis
# Output:
#   - the area under the curve
compute_area = function(x,y){
  delta = abs(x[2:length(x)] - x[1:(length(x)-1)])
  val = (y[2:length(y)] + y[1:(length(y)-1)])/2
  return(sum(delta*val))
}

# Compute the table of TP, FP, TN, FN, Sensitity and Specifity, 
# Compute the area under the ROC curve,
# Plot the ROC curve if needed.
# Input:
#   -y,yhat: true and predict value 
#   -plot: indicating no plot or plot by base R graphics or plot by ggplot2. 
# Output:
#   plot of ROC curve if base or ggplot2 is chosen for plot
#   A list with 2 elements:
#     - df: the target data frame with 7 columns
#     - area_roc: the area under the ROC curve
perf_roc = function(y, yhat, plot = c("none", "base", "ggplot2")){
  tau = sort(unique(yhat))
  df = matrix(rep(NA,7*length(tau)),ncol=7)
  colnames(df) = c("Tau","TP","FP","TN","FN","Sensitivity","Specifity")
  df[,1] = tau
  df[,2:7] = t(sapply(tau, compute_tf1,y,yhat))
  # add 0,1 to avoid just compute area from min to max,
  # which can make this estimate more accurate
  fpr = c(1,1- df[,7],0)
  tpr = c(df[1,6],df[,6],0)
  area_roc = compute_area(fpr,tpr)
  # Plot of ROC Curve
  plot = match.arg(plot)
  switch (plot,
          none = cat("No Plot"),
          base = plot(fpr, tpr, type="l",
                      xlab="False Positive Rate (FPR)",
                      ylab="True Positive Rate (TPR)"),
          ggplot2 = {
            p = ggplot(data.frame(fpr,tpr),aes(x=fpr,y=tpr))+
                  geom_path(size=0.6, alpha=0.7)+
                  labs(x="False Positive Rate (FPR)", 
                       y="True Positive Rate (TPR)")+
                  theme(plot.title = element_text(hjust = 0.5))
            print(p)
          }
  )
  return(list(df=df, area_roc=area_roc))
}

# Compute the TP, FP, TN, FN, recall and precision used in part (a)
# Input:
#   -tau: given threshold
#   -y,yhat: true and predict value
# Output:
#   -a vector of tp, fp, tn, fn, recall and precision

compute_tf2 = function(tau,y,yhat){
  tp = sum((yhat>=tau)&(y==1))
  fp = sum((yhat>=tau)&(y==0))
  tn = sum((yhat<tau)&(y==0))
  fn = sum((yhat<tau)&(y==1))
  re = tp/(tp+fn)
  pr = tp/(tp+fp)
  return(c(tp, fp, tn, fn, re, pr))
}

# Compute the table of TP, FP, TN, FN, Recall and Precision, 
# Compute the area under the Precision-Recall Curve,
# Plot the Precision-Recall Curve if needed.
# Input:
#   -y,yhat: true and predict value 
#   -plot: indicating no plot or plot by base R graphics or plot by ggplot2. 
# Output:
#   plot of Precision-Recall curve if base or ggplot2 is chosen for plot
#   A list with 2 elements:
#     - df: the target data frame with 7 columns
#     - area_pr: the area under the Precision-Recall Curve
perf_pr = function(y, yhat, plot = c("none", "base", "ggplot2")){
  tau = sort(unique(yhat))
  df = matrix(rep(NA,7*length(tau)),ncol=7)
  colnames(df) = c("Tau","TP","FP","TN","FN","Recall","Precision")
  df[,1] = tau
  df[,2:7] = t(sapply(tau, compute_tf2,y,yhat))
  # add 0,1 to avoid just compute area from min to max,
  # which can make this estimate more accurate
  re = c(1,df[,6],0) 
  pre = c(0,df[,7],df[dim(df)[1],7])
  area_pr = compute_area(re,pre)
  # Plot of Precision-Recall Curve
  plot = match.arg(plot)
  switch (plot,
          none = cat("No Plot"),
          base = plot(re, pre, type="l", 
                      xlab="Recall",
                      ylab="Precision"),
          ggplot2 = {
            p = ggplot(data.frame(re,pre),aes(x=re,y=pre))+
                  geom_path(size=0.6, alpha=0.7)+
                  labs(x="Recall", 
                       y="Precision")+
                  theme(plot.title = element_text(hjust = 0.5))
            print(p)
          }
  )
  return(list(df=df, area_pr=area_pr))
}
# 79: -------------------------------------------------------------------------
