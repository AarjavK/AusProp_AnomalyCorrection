library(caret)
library(GA)
library(tidyverse)
library(gbm)
library(tictoc)

fpath <- "path/to/your/data.csv"

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
scaling_cols <- c("Beds", "Baths", "Cars", "Land", "Median.price", "Price")
df_processed[scaling_cols] <- scale(df_processed[scaling_cols])
#df_processed <- df_processed[, c("Beds", "Baths", "Land", "Price")]

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
  print(vars)
  print(colnames(data_features)[vars==1])
  sel_columns <- colnames(data_features)[vars==1]

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
  
  # Create Gradient Boost Model
  model <- gbm(Price ~ ., data = data_sel,
               distribution = "gaussian",
               n.trees = 50,
               shrinkage = 0.5,
               cv.folds = 10,
               interaction.depth = 10
              )
  
  predictions <- predict(model, df_processed[!samples, sel_columns, drop = FALSE])
  err <- RMSE(pred = predictions, obs = df_processed[!samples, "Price", drop = FALSE])
  #print(err)
  return(-err)
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
  print(vars)
  print(colnames(data_features)[vars==1])
  sel_columns <- colnames(data_features)[vars==1]
  
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
  
  lm_fit <- train(Price ~ .*. ,data = data_sel,
                  method = 'lm',
                  trControl = fitControl)
  err <- lm_fit$results$RMSE
  #print(err)
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
  #print(vars)
  #print(colnames(data_features)[vars==1])
  sel_columns <- colnames(data_features)[vars==1]
  
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


################################################################################
# Implementing GA
################################################################################
tic("LM GA training start")
ga_lm <- ga(type = 'binary',
            popSize = 100,
            fitness = function(vars, train_ratio) lm_fit(vars = vars,
                                                       train_ratio = 0.7),
            nBits = ncol(df_processed)-1,
            maxiter = 100,
            keepBest = TRUE,
            run = 30,
            monitor = plot
            )
toc("LM GA training stop")

linear_mod_int <- data.frame(iteration = c(1:ga_lm@iter), RMSE = c(ga_lm@fitnessValue))
write.csv(linear_mod_int,"~/lm_ga_int_generations.csv",row.names = FALSE)

tic("RF GA training start")
ga_rf <- ga(type = 'binary',
            popSize = 100,
            fitness = function(vars, train_ratio) rf_fit(vars = vars,
                                                         train_ratio = 0.7),
            nBits = ncol(df_processed)-1,
            maxiter = 100,
            keepBest = TRUE,
            run = 30,
            monitor = plot
)
toc("RF GA training stop")

rf_mod_int <- data.frame(iteration = c(1:ga_rf@iter), RMSE = c(ga_rf@fitnessValue))
ga_rf@solution
colnames(data_features)[ga_rf@solution == 1]
write.csv(rf_mod_int,"~/rf_ga_int_generations.csv",row.names = FALSE)


tic("GBM GA training start")
ga_gbm <- ga(type = 'binary',
            popSize = 100,
            fitness = function(vars, train_ratio) gbm_fit(vars = vars,
                                                         train_ratio = 0.7),
            nBits = ncol(df_processed)-1,
            maxiter = 100,
            keepBest = TRUE,
            run = 30,
            monitor = plot
)
toc("GBM GA training stop")
