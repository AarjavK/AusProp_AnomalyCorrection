library(caret)
library(GA)
library(tidyverse)
library(gbm)
library(tictoc)

fpath <- "~/Domain_Housing_Data_250511.csv"

df <- read.csv(fpath, header = TRUE, sep= ",")
colnames(df)[1] <- "Date"
################################################################################
# Transform the data
################################################################################
df <- df[ , !colnames(df) %in% c("Date", "Address")]

# Converting categorical variables as factors
cat_cols <- c("Suburb", "State", "Type", "Postcode", "SaleType")
cat_features <- df[ , cat_cols]
cat_features <- cat_features %>% mutate_all(as.factor)  %>% mutate_all(as.numeric)

df_processed <- df
df_processed[ , cat_cols] <- cat_features

df_processed <- na.omit(df_processed)

# Features
data_features <- df_processed[, !colnames(df_processed) %in% "Price"]

################################################################################
# Fitness functions for GA
################################################################################
gbm_fit <- function(vars, train_ratio) {
  ##############################################################################
  # Function to train a gradient boost model on a subset of data for a given  
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
  
  # In case no columns are selected
  if(length(sel_columns) == 0) {
    return(-Inf)
  }
  
  samples <- sample(c(TRUE,FALSE), nrow(df_processed), replace = TRUE,
                    prob = c(train_ratio, 1 - train_ratio))
  data_sel <- df_processed[samples, sel_columns, drop = FALSE]
  data_sel$Price <- df_processed[samples, "Price"]
  
  fitControl <- trainControl(method = "cv", number = 5)
  
  # Create Gradient Boost Model
  model <- gbm(Price ~ ., data = data_sel,
               distribution = "gaussian",
               n.trees = 50,
               shrinkage = 0.5,
               cv.folds = 10,
               interaction.depth = 10
              )
  
  predictions <- predict(model, df_processed[!samples, sel_columns, drop = FALSE])
  
  return(RMSE(pred = predictions, obs = df_processed[!samples, "Price", drop = FALSE]))
}

lm_fit <- function(vars, train_ratio) {
  ##############################################################################
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
  
  # In case no columns are selected
  if(length(sel_columns) == 0) {
    return(-Inf)
  }
  
  samples <- sample(c(TRUE,FALSE), nrow(df_processed), replace = TRUE,
                    prob = c(train_ratio, 1 - train_ratio))
  data_sel <- df_processed[samples, sel_columns, drop = FALSE]
  data_sel$Price <- df_processed[samples, "Price"]
  
  fitControl <- trainControl(method = "cv", number = 5)
  
  lm_fit <- train(Price ~ . ,data = data_sel,
                  method = 'lm',
                  trControl = fitControl)
  return(lm_fit$results$RMSE)
}

################################################################################
# Implementing GA
################################################################################
tic("LM GA training start")
ga_lm <- ga(type = 'binary',
            popSize = 1000,
            fitness = function(vars, train_ratio) lm_fit(vars = vars,
                                                       train_ratio = 0.7),
            nBits = ncol(df_processed)-1,
            maxiter = 1000,
            keepBest = TRUE,
            run = 30,
            monitor = plot
            )
toc("LM GA training stop")
