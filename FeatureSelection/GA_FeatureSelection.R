library(caret)
library(GA)
library(tidyverse)
library(gbm)
library(tictoc)

df <- read.csv("Domain_Housing_Data_All_coordinates.csv", header = TRUE, sep= ",")
colnames(df)[1] <- "Date"
################################################################################
# Transform the data
################################################################################
df <- df[ , !colnames(df) %in% c("Date", "Address", "latitude", "longitude")]

# Converting categorical variables as factors
cat_cols <- c("Suburb", "State", "Type", "Postcode", "SaleType")
cat_features <- df[ , cat_cols]
cat_features <- cat_features %>% mutate_all(as.factor)  %>% mutate_all(as.numeric)

df_processed <- df
df_processed[ , cat_cols] <- cat_features

df_processed <- na.omit(df_processed)
scaling_cols <- c("Beds", "Baths", "Cars", "Land", "dist_to_cbd", "Median.price", "Price")
df_processed[scaling_cols] <- scale(df_processed[scaling_cols])
#df_processed <- df_processed[, c("Beds", "Baths", "Land", "Price")]

# Features
data_features <- df_processed[, !colnames(df_processed) %in% "Price"]

################################################################################
# Fitness functions for GA
################################################################################
lm_fit <- function(vars, train_ratio) {
  ###############################################################################
  # Function to train a linear model on a subset of data for a given  
  # set of features then calculate the error metrics for use in GA
  #
  # Inputs:
  # -------
  # [+] vars: binary string stating which features to select
  # [+] train_ratio: percentage of data set to use for training (as a decimal)
  #
  # Output:
  # -------
  # Error metric
  
  # Subset the data
  sel_columns <- colnames(data_features)[vars==1]
  print(sel_columns)
  
  # In case no columns are selected
  if(length(sel_columns) == 0) {
    print("No columns selected")
    return(-Inf)
  }
  
  samples <- sample(c(TRUE,FALSE), nrow(df_processed), replace = TRUE,
                    prob = c(train_ratio, 1 - train_ratio))
  data_sel <- df_processed[samples, sel_columns, drop = FALSE]
  data_sel$Price <- df_processed[samples, "Price"]
  
  fitControl <- trainControl(method = "cv", number = 5)
  
  # Include interaction terms
  lm_fit <- train(Price ~ .*. ,data = data_sel,
                  method = 'lm',
                  trControl = fitControl)
  err <- lm_fit$results$RMSE
  # To minimise the error
  return(-err)
}

rf_fit <- function(vars, train_ratio) {
  ##############################################################################
  # Function to train a Random Forest model on a subset of data for a given  
  # set of features then calculate the error metrics for use in GA
  #
  # Inputs:
  # -------
  # [+] vars: binary string stating which features to select
  # [+] train_ratio: percentage of data set to use for training (as a decimal)
  #
  # Output:
  # -------
  # Error metric
  
  # Subset the data
  sel_columns <- colnames(data_features)[vars==1]
  print(sel_columns)
  
  # In case no columns are selected
  if(length(sel_columns) == 0) {
    print("No columns selected")
    return(-Inf)
  }
  
  samples <- sample(c(TRUE,FALSE), nrow(df_processed), replace = TRUE,
                    prob = c(train_ratio, 1 - train_ratio))
  data_sel <- df_processed[samples, sel_columns, drop = FALSE]
  
  fitControl <- trainControl(method = "cv", number = 5)
  # Get the number of random forests
  mtry <- sqrt(ncol(data_sel))
  tunegrid <- expand.grid(.mtry = round(mtry))
  
  rf_fit <- train(x= data_sel,
                  y = df_processed[samples, "Price"],
                  method = 'rf',
                  tuneGrid = tunegrid,
                  trControl = fitControl)
  err <- rf_fit$results$RMSE
  #print(err)
  return(-err)
}

gbm_fit <- function(vars, train_ratio) {
  ##############################################################################
  # Function to train a GBM (Gradient Boosting Machine) model on a subset of data  
  # for a given set of features then calculate the error metrics for use in GA
  #
  # Inputs:
  # -------
  # [+] vars: binary string stating which features to select
  # [+] train_ratio: percentage of data set to use for training (as a decimal)
  #
  # Output:
  # -------
  # Error metric
  
  # Subset the data
  sel_columns <- colnames(data_features)[vars == 1]
  print(sel_columns)
  
  # In case no columns are selected
  if (length(sel_columns) == 0) {
    print("No columns selected")
    return(-Inf)
  }
  
  samples <- sample(c(TRUE, FALSE), nrow(df_processed), replace = TRUE,
                    prob = c(train_ratio, 1 - train_ratio))
  data_sel <- df_processed[samples, sel_columns, drop = FALSE]
  data_sel$Price <- df_processed[samples, "Price"]
  
  fitControl <- trainControl(method = "cv", number = 5)
  
  gbmGrid <- expand.grid(
    n.trees = c(100, 300),
    interaction.depth = c(2, 3, 4),
    shrinkage = c(0.05, 0.1),
    n.minobsinnode = 10
  )
  
  gbm_fit <- train(
    Price ~ .,
    data = data_sel,
    method = "gbm",
    distribution = "gaussian",
    trControl = fitControl,
    tuneGrid = gbmGrid,
    verbose = FALSE
  )
  
  err <- min(gbm_fit$results$RMSE)
  return(-err)
}

################################################################################
# Implementing GA
################################################################################
tic("LM GA training start")
ga_lm <- ga(type = 'binary',
            popSize = 100,
            fitness = function(vars, train_ratio) lm_fit(vars = vars,
                                                       train_ratio = 0.5),
            nBits = ncol(df_processed)-1,
            maxiter = 100,
            keepBest = TRUE,
            run = 30,
            monitor = plot
            )
toc("LM GA training stop")
plot.ga(ga_lm)
linear_mod_int <- data.frame(iteration = c(1:ga_lm@iter), RMSE = c(ga_lm@fitnessValue))
# Best iteration
best_iter <- which.min(linear_mod_int$RMSE)
best_sol_iter <- ga_lm@bestSol[best_iter]
# Print the columns selected in the best solution
colnames(data_features)[best_sol_iter[[1]] == 1]

# best RMSE for linear regression
min(linear_mod_int$RMSE)
write.csv(linear_mod_int,"~/lm_ga_int_generations.csv",row.names = FALSE)

tic("RF GA training start")
ga_rf <- ga(type = 'binary',
            popSize = 100,
            fitness = function(vars, train_ratio) rf_fit(vars = vars,
                                                         train_ratio = 0.3),
            nBits = ncol(df_processed)-1,
            maxiter = 100,
            keepBest = TRUE,
            run = 30,
            monitor = plot
)
toc("RF GA training stop")
plot.ga(ga_rf)
rf_mod_int <- data.frame(iteration = c(1:ga_rf@iter), RMSE = c(ga_rf@fitnessValue))
# Best iteration
best_iter_rf <- which.min(rf_mod_int$RMSE)
best_sol_iter_rf <- ga_rf@bestSol[1]
colnames(data_features)[best_sol_iter_rf[[1]] == 1]
# best RMSE for linear regression
min(rf_mod_int$RMSE)
ga_rf@solution
colnames(data_features)[ga_rf@solution == 1]
write.csv(rf_mod_int,"~/rf_ga_int_generations.csv",row.names = FALSE)

tic("GBM GA training start")
ga_gbm <- ga(type = 'binary',
            popSize = 100,
            fitness = function(vars, train_ratio) gbm_fit(vars = vars,
                                                         train_ratio = 0.3),
            nBits = ncol(df_processed)-1,
            maxiter = 100,
            keepBest = TRUE,
            run = 30,
            monitor = plot
)
toc("GBM GA training stop")
plot.ga(ga_gbm)
gbm_mod_int <- data.frame(iteration = c(1:ga_gbm@iter), RMSE = c(ga_gbm@fitnessValue))
# Best iteration
best_iter_gbm <- which.min(gbm_mod_int$RMSE)
best_sol_iter_gbm <- ga_gbm@bestSol[1]
colnames(data_features)[best_sol_iter_gbm[[1]] == 1]
# best RMSE for linear regression
min(gbm_mod_int$RMSE)
ga_gbm@solution
colnames(data_features)[ga_gbm@solution == 1]
write.csv(rf_mod_int,"~/gbm_ga_int_generations.csv",row.names = FALSE)
