options(warn=-1)
library(tmle)
library(SuperLearner)
library(dplyr)
library(ggplot2)
library("nnet")

# Convenience functions
get_data <- function(dir, file_in){
  # Get the HR data in data-frame format
  #
  # Args:
  #   dir: working directory as character vector
  #   file_in: name of the file as character vector
  #
  # Returns:
  #   dataframe object with no transformations
  
  df <- read.csv(paste0(dir, file_in), stringsAsFactors = FALSE)
  return(df)
}

# Get knn with different sizes
create_SL_knn <- function(k = c(20, 30)) {
  for(mm in seq(length(k))){
    eval(parse(text = paste('SL.knn.', k[mm], '<- function(..., k = ', k[mm],
                            ') SL.knn(..., k = k)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create_SL_knn(c(10, 15, 20, 25))

# Get Neural networks of different sizes
create_SL_nnet <- function(size = c(2, 3)) {
  for(mm in seq(length(size))){
    eval(parse(text = paste('SL.nnet.', size[mm], '<- function(..., size = ', size[mm],
                            ') SL.nnet(..., size = size)', sep = '')), envir = .GlobalEnv)
  }
  invisible(TRUE)
}
create_SL_nnet(c(2, 3, 4, 5, 6))

# Get both the ridge and lasso regressions
SL.glmnet.0 <- function(..., alpha = 0) SL.glmnet(..., alpha = 0) # Ridge
SL.glmnet.1 <- function(..., alpha = 1) SL.glmnet(..., alpha = 1) # Lasso

get_super_learner <- function(df, Y, A=NULL, learning_library){
  # Train SuperLearner and create counterfactual outcomes
  # 
  # Args:
  #   df: observed data as a data-frame
  #   learning_library: character vector of libraries used for ensembling
  # 
  # Returns:
  #   model object with predictions, and other results (e.g., cvRisk)
  
  y = df[, Y]
  df[Y] <- NULL
  
  if (Y != "salary" && !is.null(A)) {
    # Set treatment to 0, 1 for generating the counterfactual outcomes
    X_0 = df
    X_0[, A] = 0
    X_1 = df
    X_1[, A] = 1
    model <- SuperLearner(Y = y
                          , X = df
                          , newX = rbind(df, X_0, X_1)     # Note: this data is not used for training
                          , SL.library = learning_library
                          , cvControl = list(V = 5)
                          , family = 'binomial'
                          , verbose = FALSE)
    return(model)
  } else {
    
    model <- SuperLearner(Y = y
                          , X = df
                          , SL.library = learning_library
                          , cvControl = list(V = 5)
                          , family = 'binomial'
                          , verbose = FALSE)
    return(model)
  }
}

get_counterfact_outcomes <- function(df, model){
  # Retreive Y_a under each potential outcome from the super learner object
  # 
  # Args:
  #   df: observed data as a data-frame
  #   model: observed data as a data-frame
  # 
  # Returns:
  #   model object with predictions, and other results (e.g., cvRisk)
  Q_bar_AW <- model$SL.predict[1:(nrow(df))]
  output.df = data.frame(Q_bar_AW = Q_bar_AW)
  if (length(model$SL.predict) > nrow(df)) {
    Q_bar_0W <- model$SL.predict[(nrow(df)+1):(2*nrow(df))]
    Q_bar_1W <- model$SL.predict[(2*nrow(df)+1):(3*nrow(df))]
    output.df$Q_bar_0W = Q_bar_0W
    output.df$Q_bar_1W = Q_bar_1W
  }
  return(output.df)
}

bucket_covariate <- function(x, k){
  # Approximate a single quantitative variable based off of the quantiles
  # 
  # Args:
  #   x: The quantitative vector to be approximated
  #   k: The number of quantiles to use in the approximation
  # 
  # Returns:
  #   Numeric vector with numbers corresponding to quantiles (e.g., 1 = 1st quantile)
  
  x_split <- cut(x, breaks = quantile(x, probs = seq(0, 1, 1/k)), include.lowest = TRUE)
  if(any(is.na(x_split))) stop('There are NA values in')
  return(as.numeric(factor(x_split, labels = 1:k)))
}

# Set constants
SL_LIBRARY <- c(
  
  # Linear methods
  'SL.glm'
  , 'SL.glmnet.0' # Ridge
  , 'SL.glmnet.1' # Lasso
  
  # Additive models, Trees and other methods
  , 'SL.gam'
  , 'SL.xgboost'
  , 'SL.randomForest'
  , 'SL.rpartPrune'
  , 'SL.polymars'
  
  # Neural Network Methods
  , 'SL.nnet.2'
  , 'SL.nnet.3'
  , 'SL.nnet.4'
  , 'SL.nnet.5'
  , 'SL.nnet.6'
  
  # Other
  , 'SL.mean'
)

DIR <- '/Users/calvinchi/Documents/PH252D/Project/PH252D/data/'
OUTDIR <- '/Users/calvinchi/Documents/PH252D/Project/PH252D/sandbox/calvin/result/'
FILE_IN_NAME <- 'SL_output.csv'

# SCRIPT STARTS HERE
df = get_data(dir = DIR, file_in = FILE_IN_NAME)
results = matrix(, nrow = 5, ncol = 4)
colnames(results) = c("ATE", "p-value", "95_CI.lower", "95_CI.higher")
rownames(results) = c("PSI.HAT.salary", "VIM.salary", "VIM.last_evaluation", "VIM.satisfaction_level", "VIM.average_montly_hours")

# Get necessary input for TMLE from data
Qbar0W <- df$Q_bar_0W
Qbar1W <- df$Q_bar_1W
tmle.input = data.frame(Qbar0W = df$Q_bar_0W, Qbar1W = df$Q_bar_1W, ghatAW = df$g_hat_AW)

# Estimate PSI.HAT.Salary
W = subset(df, select = -c(left, salary))
tmle = tmle(Y = df$left, A = df$salary, W = W, Q=cbind(tmle.input$Qbar0W, tmle.input$Qbar1W), 
            g1W = tmle.input$ghatAW, family = "binomial")
results["PSI.HAT.salary", "ATE"] = tmle$estimates$ATE$psi
results["PSI.HAT.salary", "p-value"] = tmle$estimates$ATE$pvalue
results["PSI.HAT.salary", c("95_CI.lower", "95_CI.higher")] = tmle$estimates$ATE$CI

# Sensitivity Analysis of Conditioning on more W's
df$satisfaction_level = bucket_covariate(df$satisfaction_level, 5)
df$last_evaluation = bucket_covariate(df$last_evaluation, 5)

W.all = list(time_spend_company = c("time_spend_company"), promotion_last_5years = c("promotion_last_5years"),
             department = c("salesaccounting", "saleshr", "salesIT", "salesmanagement", "salesmarketing", 
                            "salesproduct_mng", "salesRandD", "salessales", "salessupport"),
             satisfaction_level = c("satisfaction_level"), last_evaluation = c("last_evaluation"))
sensitivity.result = data.frame(matrix(, nrow = 5, ncol = 3))
colnames(sensitivity.result) = c("ATE", "CI95.lower", "CI95.higher")
data.df = data.frame(left = df$left, salary = df$salary, average_montly_hours = df$average_montly_hours, 
                     number_project = df$number_project, Work_accident = df$Work_accident)

#data.df = subset(df, select = -c(Q_bar_AW, Q_bar_0W, Q_bar_1W, g_hat_AW, satisfaction_level, last_evaluation))

for (i in seq(4, length(W.all))) {
  print(i)
  data.df = cbind(data.df, df[, W.all[[names(W.all)[i]]]])
  colnames(data.df)[ncol(data.df)] = names(W.all)[i]
  data.df = as.data.frame(sapply(data.df, as.numeric))
  model = get_super_learner(data.df, "left", "salary", learning_library = SL_LIBRARY)
  QBars = get_counterfact_outcomes(data.df, model)
  
  model = get_super_learner(subset(data.df, select = -c(left)), "salary", learning_library = SL_LIBRARY)
  ghatAW = get_counterfact_outcomes(data.df, model)$Q_bar_AW
  
  W = subset(data.df, select = -c(salary, left))
  tmle = tmle(Y = data.df$left, A = data.df$salary, W = W, Q=cbind(QBars$Q_bar_0W, QBars$Q_bar_1W), 
              g1W = ghatAW, family = "binomial")
  sensitivity.result[i, "ATE"] = tmle$estimates$ATE$psi
  sensitivity.result[i, c("CI95.lower", "CI95.higher")] = tmle$estimates$ATE$CI
}
sensitivity.result$W = seq(5, 9, 1)

# Generate Plot for Sensitivity Analysis
png(filename=paste0(OUTDIR, 'sensitivity_analysis.png'))
ggplot(sensitivity.result, aes(x = W, y = ATE)) + geom_errorbar(aes(ymin = CI95.lower, ymax = CI95.higher), width=.1) + 
  geom_line() + geom_point() + labs(x = "Number of Covariates", title = "Sensitivity Analysis for Average Treatment Effect (ATE)") +
  theme(plot.title = element_text(hjust = 0.5))
dev.copy(png, filename=paste0(OUTDIR, 'sensitivity_analysis.png'))
dev.off()

# Estimate ATE for last_evaluation, salary, satisfaction_level, average_monthly_hours
W.interest = c("salary", "average_montly_hours", "satisfaction_level", "last_evaluation")
data.df = subset(df, select = -c(Q_bar_AW, Q_bar_0W, Q_bar_1W, g_hat_AW))
data.df$salary = data.df$salary + 1

for (w in W.interest) {
  data.df = subset(data.df, select = -c(left))
  if (w == "salary") {
    gAW.reg <- multinom(salary ~., data=data.df)
  } else if (w == "average_montly_hours") {
    gAW.reg <- multinom(average_montly_hours ~., data=data.df)
  } else if (w == "satisfaction_level") {
    gAW.reg <- multinom(satisfaction_level ~., data=data.df)
  } else if (w == "last_evaluation") {
    gAW.reg <- multinom(last_evaluation ~., data=data.df)
  }
  
  gAW <- vector(length = nrow(data.df))
  gAW.pred <- predict(gAW.reg, type="probs")
  
  for (i in unique(data.df[, w])) {
    gAW[data.df[, w] == i] = gAW.pred[data.df[, w]==i][i]
  }
  
  gA = vector(length = nrow(data.df))
  for (i in unique(data.df[, w])) {
    gA[data.df[, w] == i] = mean(data.df[, w] == i)
  }
  wt.MSM = gA / gAW
  
  data.df$left = df$left
  if (w == "salary") {
    model <- glm(left ~ salary, data = data.df, weights = wt.MSM)
  } else if (w == "average_montly_hours") {
    model <- glm(left ~ average_montly_hours, data = data.df, weights = wt.MSM)
  } else if (w == "satisfaction_level") {
    model <- glm(left ~ satisfaction_level, data = data.df, weights = wt.MSM)
  } else if (w == "last_evaluation") {
    model <- glm(left ~ last_evaluation, data = data.df, weights = wt.MSM)
  }
  
  results[paste0("VIM.", w), "ATE"] = model$coefficients[2]
  results[paste0("VIM.", w), "p-value"] = coef(summary(model))[2, 4]
  results[paste0("VIM.", w), c("95_CI.lower", "95_CI.higher")] = confint(model)[2, ]
}

# Write all results
write.table(results, file = "results/VIM.txt", quote = FALSE, sep = " ")
write.table(sensitivity.result, paste0(OUTDIR, "Sensitivity.txt"), quote = FALSE, sep = " ")

