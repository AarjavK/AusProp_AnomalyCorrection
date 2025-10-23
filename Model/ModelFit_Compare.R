################################################################################
# Scrape domain.com.au for housing data
################################################################################
library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret)
library(randomForest)
#library(tictoc)
#library(rlang) # for !! and sym
library(shapviz)
library(kernelshap)
library(future)

set.seed(42)

#data = read.csv('Domain_housing_data_all.csv')
data = read.csv('Domain_housing_data_all_coordinates.csv')
colnames(data)[1] <- "Date"

################################################################################
# Transform the data
################################################################################
processed_data <- data[, !colnames(data) %in% c('Date','Address', 'longitude', 
                                                'latitude')]
processed_data <- na.omit(processed_data)

data <- processed_data

#cat_cols <- c("State", "Type", "SaleType")
#cat_features_dummies <- dummyVars(" ~ State + Type + SaleType", data = processed_data)
#cat_features <- predict(cat_features_dummies, newdata = processed_data)
#cat_features <- as.data.frame(cat_features)
cat_cols <- c("Suburb","State","Type","Postcode","SaleType")
cat_features <- data[, cat_cols]
cat_features <- cat_features %>% mutate_all(as.factor) %>% mutate_all(as.numeric)

processed_data <- cbind(processed_data[ , !colnames(processed_data) %in% cat_cols], 
                        cat_features)

factor_cols <- c("Beds", "Baths", "Cars")
factor_features <- processed_data[ , factor_cols]

# Scaling
scaling = TRUE
if (scaling == TRUE) {
  factor_features <- factor_features %>% mutate_all(as.factor) %>% mutate_all(as.numeric)
  processed_data[ , colnames(factor_features)] <- factor_features
  
  scaling_cols <- c("Land", "Median.price", "Price", "dist_to_cbd", factor_cols)
  processed_data["Median.price"] <- processed_data["Median.price"] %>% mutate_all(as.numeric)
  processed_data["Price"] <- processed_data["Price"] %>% mutate_all(as.numeric)
  processed_data[scaling_cols] <- scale(processed_data[scaling_cols])
} else {
  factor_features <- factor_features %>% mutate_all(as.factor)
  processed_data[ , colnames(factor_features)] <- factor_features
}

model_data <- processed_data[, !colnames(processed_data) %in% c("Date", "Address","Suburb", "longitude", "latitude")]
model_data_cols <- colnames(model_data)

################################################################################
# MODELING
################################################################################
train_model <- function(df, features, y, model_type, p = 0.7) {
  # Function train a model using a given feature set and model type
  #
  # Inputs:
  # -------
  #   [+] df: dataframe containing the data
  #   [+] features: list of features to include in training
  #   [+] y: the value to predict
  #   [+] p: (Optional) the training percentage - 0.7 (70%) by default
  #
  # Outputs:
  # --------
  #   The trained model
  
  # Train-test split
  trainIndex <- createDataPartition(y = df[, y], p = p, list = FALSE)
  
  train_set <- df[trainIndex, ]
  test_set <- df[-trainIndex, ]
  
  fitControl <- trainControl(method = "cv", number = 10)
  feature_set <- train_set[, features]
  
  if(model_type == "lm") {
    trained_model <- train(!!sym(y) ~ .*. ,
                           model_data, 
                           trControl = fitControl,
                           method = 'lm')
  } else if (model_type == "rf") {
    mtry <- sqrt(ncol(df))
    tunegrid <- expand.grid(.mtry=c(1:mtry*2)) # 1 to 2 * sqrt(num_features)
    trained_model <- train(x = feature_set, 
                           y = train_set[[y]],
                           method = 'rf', 
                           tunegrid = tunegrid,
                           trControl = fitControl)
  } else if (model_type == "gbm") {
      gbmGrid <- expand.grid(
        n.trees = c(100, 300),
        interaction.depth = c(2, 3, 4),
        shrinkage = c(0.05, 0.1),
        n.minobsinnode = 10
      )
    
      gbm_fit <- train(
          !!sym(y) ~ .,
          data = data_sel,
          method = "gbm",
          distribution = "gaussian",
          trControl = fitControl,
          tuneGrid = gbmGrid,
          verbose = FALSE
      )
  } else {
    stop("This model is not supported!!")
  }
  
}


trainIndex <- createDataPartition(y = model_data$Price, p = 0.7, list = FALSE)

train_set <- model_data[trainIndex, ]
test_set <- model_data[-trainIndex, ]

# Check the stratification of the split
table(train_set$State)
table(test_set$State)
################################################################################
# Base line models - use repeated cv to further split the train set into train-test 
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 10, savePredictions = "final")
feature_set <- train_set[, !colnames(model_data) %in% "Price"]

################################ LINEAR MODELS #################################
baseline_lm_no_int_sol <- train(Price ~ .+. ,
                                model_data, 
                                trControl = fitControl,
                                method = 'lm')

baseline_lm_int_sol <- train(Price ~ .*. ,
                             model_data, 
                             trControl = fitControl,
                             method = 'lm')

################################ RANDOM FOREST #################################
mtry = sqrt(ncol(model_data))
tunegrid = expand.grid(.mtry=c(1:mtry*2))

baseline_rf_no_int_sol <- train(Price ~ .+.,
                                 data = model_data,
                                 method = 'rf', 
                                 tunegrid = tunegrid,
                                 trControl = fitControl)
#baseline_rf_int_sol <- train(Price ~ .*.,
#                             data = model_data,
#                             method = 'rf', 
#                             tunegrid = tunegrid,
#                             trControl = fitControl)

##################################### GBM ######################################
gbmGrid <- expand.grid(
  n.trees = c(100, 300),
  interaction.depth = c(2, 3, 4),
  shrinkage = c(0.05, 0.1),
  n.minobsinnode = 10 
)

baseline_gbm_sol <- train(Price ~ .+.,
  data = train_set,
  method = "gbm",
  distribution = "gaussian",
  trControl = fitControl,
  tuneGrid = gbmGrid,
  verbose = FALSE
  )

#baseline_gbm_sol_int <- train(Price ~ .*.,
#  data = train_set,
#  method = "gbm",
#  distribution = "gaussian",
#  trControl = fitControl,
#  tuneGrid = gbmGrid,
#  verbose = FALSE
#)


########################## GENETIC ALGORITHM SOLUTION ##########################
lm_ga_sol <- c("Postcode", "Beds", "Baths", "Cars", "Land", 
               "Median.price", "dist_to_cbd", "Price")
rf_ga_sol <- c("State", "Postcode", "Beds", "Baths", "Cars", "Land", 
               "Median.price", "dist_to_cbd", "Price")
gbm_ga_sol <- c("State", "Postcode", "Beds", "Baths", "Cars", "Land", 
               "Type", "dist_to_cbd", "Price")

model_data_lm <- model_data[, lm_ga_sol]
model_data_rf <- model_data[, rf_ga_sol]
model_data_gbm <- model_data[, gbm_ga_sol]

train_set_lm <- model_data_lm[trainIndex, ]
test_set_lm <- model_data_lm[-trainIndex, ]

train_set_rf <- model_data_rf[trainIndex, ]
test_set_rf <- model_data_rf[-trainIndex, ]

train_set_gbm <- model_data_gbm[trainIndex, ]
test_set_gbm <- model_data_gbm[-trainIndex, ]

############################### LINEAR REGRESSION ##############################
lm_no_int_sol <- train(Price ~ .+. ,
                      model_data_lm, 
                      trControl = fitControl,
                      method = 'lm')

lm_int_sol <- train(Price ~ .*. ,
                   model_data_lm, 
                   trControl = fitControl,
                   method = 'lm')

################################ RANDOM FOREST #################################
mtry_rf = sqrt(ncol(model_data_rf))
tunegrid_rf = expand.grid(.mtry=c(1:mtry_rf*2))

rf_no_int_sol <- train(Price ~ .+.,
                                data = train_set_rf,
                                method = 'rf', 
                                tunegrid = tunegrid_rf,
                                trControl = fitControl)
#rf_int_sol <- train(Price ~ .*.,
#                             data = train_set_rf,
#                             method = 'rf', 
#                             tunegrid = tunegrid_rf,
#                             trControl = fitControl)

##################################### GBM ######################################
gbm_sol <- train(Price ~ .+.,
                data = train_set_gbm,
                method = "gbm",
                distribution = "gaussian",
                trControl = fitControl,
                tuneGrid = gbmGrid,
                verbose = FALSE
)

#gbm_sol_int <- train(Price ~ .*.,
#                    data = train_set_gbm,
#                    method = "gbm",
#                    distribution = "gaussian",
#                    trControl = fitControl,
#                    tuneGrid = gbmGrid,
#                    verbose = FALSE
#)

################################################################################
# METRICS
################################################################################
get_metrics <- function(model, model_name) {
  # Function to calculate the metrics of a given model name
  # 
  # Inputs:
  # -------
  #   [+] model: the actual model
  #   [+] model_name: Name of the model (string)
  #
  # Output:
  # -------
  # Dateframe containing 1 row with error metrics
  #
  
  mean_pred <- mean(model$pred$pred)
  var_pred <- var(model$pred$pred)
  mean_obs <- mean(model$pred$obs)
  
  bias_var <- model$pred %>%
    group_by(rowIndex) %>%
    summarise(
      mean_pred = mean(pred),
      var_pred = var(pred),
      obs = mean(obs)
    )
  
  # Calculate bias and variance
  b <- mean((bias_var$mean_pred - bias_var$obs)^2,na.rm = TRUE)
  v <- mean(bias_var$var_pred, na.rm = TRUE)
  
  data.frame(
    model = model_name,
    RMSE = sqrt(mean((model$pred$obs - model$pred$pred)^2)),
    MAE = mean(abs(model$pred$obs - model$pred$pred)),
    bias = b,
    variance = v
  )
}

baseline_tr_metrics <- rbind(
  get_metrics(baseline_lm_no_int_sol, "LM"),
  get_metrics(baseline_lm_int_sol, "LM (int)"),
  get_metrics(baseline_gbm_sol, "GBM"),
  #get_metrics(baseline_gbm_sol_int, "GBM (int)"),
  get_metrics(baseline_rf_no_int_sol, "RF")
  #get_metrics(baseline_rf_int_sol, "RF (int)"),
)

baseline_tr_metrics

# Plot the baseline training metrics
# Convert to long format for easier plotting
metrics_long <- baseline_tr_metrics %>%
  pivot_longer(cols = c(RMSE, MAE, bias, variance), names_to = "Metric", values_to = "Value")

ggplot(metrics_long, aes(x = model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Model Comparison: Performance Metrics",
    x = "",
    y = "Metric value"
  ) +
  theme(axis.text=element_text(size=12),
        axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold"))

ggplot(baseline_tr_metrics, aes(x = model, y = RMSE)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Model Comparison: Performance Metrics",
    x = "",
    y = "RMSE"
  ) +
  theme(axis.text=element_text(size=12),
        axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold"))


# Checking against the validation set
baseline_lm_no_int_predict <- predict(baseline_lm_no_int_sol, new_data = test_set)
baseline_lm_int_predict <- predict(baseline_lm_int_sol, newdata = test_set)
baseline_rf_no_int_predict <- predict(baseline_rf_no_int_sol, new_data = test_set)
baseline_gbm_predict <- predict(baseline_gbm_sol, new_data = test_set)
baseline_gbm_int_predict <- predict(baseline_gbm_sol_int, newdata = test_set)

lm_no_int_predict <- predict(lm_no_int_sol, new_data = test_set_lm)
lm_int_predict <- predict(lm_int_sol, newdata = test_set_lm)
rf_no_int_predict <- predict(rf_no_int_sol, new_data = test_set_rf)
gbm_predict <- predict(gbm_sol, new_data = test_set_gbm)
gbm_int_predict <- predict(gbm_sol_int, newdata = test_set_gbm)


# GA training metrics
ga_tr_metrics <- rbind(
  get_metrics(lm_no_int_sol, "LM"),
  get_metrics(lm_int_sol, "LM (int)"),
  get_metrics(gbm_sol, "GBM"),
  #get_metrics(gbm_sol_int, "GBM (int)"),
  get_metrics(rf_no_int_sol, "RF"),
  #get_metrics(rf_int_sol, "RF (int)"),
)

ga_tr_metrics

# To compare GA and baseline
baseline_tr_metrics <- baseline_tr_metrics %>%
  mutate(FeatureSet = "Baseline")

ga_tr_metrics <- ga_tr_metrics %>%
  mutate(FeatureSet = "GA Selected")

metric_comp <- rbind(baseline_tr_metrics, ga_tr_metrics)
combined_long <- metric_comp %>%
  pivot_longer(cols = c(RMSE, MAE, bias, variance), names_to = "Metric", values_to = "Value")
combined_long$model <- factor(combined_long$model, levels = c("LM", "LM (int)", "GBM", "RF"))

ggplot(combined_long, aes(x = model, y = Value, fill = FeatureSet)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  facet_wrap(~ Metric, scales = "free_y", ncol = 1) +
  labs(
    title = "Model Comparison: Baseline vs GA-Selected Features",
    x = "",
    y = "Metric Value"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.position = "top"
  )

# Re-train the model on the full training set without repeatedcv
fitControl_final <- trainControl(method = "cv", number = 5)

rf_no_int_sol_final <- train(
  Price ~ .+.,
  data = train_set_rf,
  method = 'rf',
  tuneGrid = tunegrid_rf,
  trControl = fitControl_final
)

rf_no_int_predict <- predict(rf_no_int_sol_final, newdata = test_set_rf)

# Plot residuals
residual_df <- data.frame(
  Predicted = rf_no_int_predict,
  Actual = test_set_rf$Price
) %>%
  mutate(Residual = Actual - Predicted)

mean_resid_rf <- mean(residual_df$Residual)
rmse_resid_rf <- sqrt(mean(residuals_rf^2))

ggplot(residual_df, aes(x = Predicted, y = Residual)) +
  geom_point(alpha = 0.5, color = "cornflowerblue", size = 2) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 1) +
  labs(
    title = "Residual Plot – RF (GA-selected features)",
    subtitle = paste("RMSE =", round(rmse_resid_rf, 3)),
    x = "Predicted Price (scaled)",
    y = "Residual (Actual - Predicted)"
  ) +
#  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 14, color = "grey40"),
    axis.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12)
  ) +
  geom_smooth(method = "loess", se = FALSE, color = "red", linewidth = 1)

################################################################################
# Anomaly detection solutions
KNN_data <- read.csv("Domain_housing_data_all_dist_knn_fin.csv")
data_knn <- model_data
data_knn$k <- KNN_data$k.28
data_knn$isAnom <- KNN_data$isAnom_k.28

LOF_data <- read.csv("Domain_housing_data_all_dist_LOF_fin.csv")
data_lof <- model_data
data_lof$lof <- LOF_data$LOF_k.186
threshold <- quantile(data_lof$lof,0.99)
data_lof$isAnom <- ifelse(data_lof$lof > threshold, "Anomaly", "Normal")
  
AE_data <- read.csv("Domain_housing_data_all_AE_best_fin2.csv")
data_AE <- model_data
data_AE$Reconstruction.MSE <- AE_data$Reconstruction.MSE
data_AE$isAnom <- AE_data$isAnomaly

data_knn_no_anom <- data_knn[data_knn$isAnom == "Normal",]
data_lof_no_anom <- data_lof[data_lof$isAnom == "Normal",]
data_AE_no_anom <- data_AE[data_AE$isAnom == "Normal",]

knn_train_ind <- createDataPartition(y = data_knn_no_anom$Price, p = 0.7, list = FALSE)
lof_train_ind <- createDataPartition(y = data_lof_no_anom$Price, p = 0.7, list = FALSE)
AE_train_ind <- createDataPartition(y = data_AE_no_anom$Price, p = 0.7, list = FALSE)

data_knn_no_anom_train <- data_knn_no_anom[knn_train_ind, rf_ga_sol]
data_lof_no_anom_train <- data_lof_no_anom[lof_train_ind,rf_ga_sol]
data_AE_no_anom_train <- data_AE_no_anom[AE_train_ind,rf_ga_sol]

################################ RANDOM FOREST #################################
mtry_rf = sqrt(ncol(data_knn_no_anom_train))
tunegrid_rf = expand.grid(.mtry=c(1:mtry_rf*2))

rf_knn_no_int_sol <- train(Price ~ .+.,
                       data = data_knn_no_anom_train,
                       method = 'rf', 
                       tunegrid = tunegrid_rf,
                       trControl = fitControl)

anom_metrics <- get_metrics(rf_knn_no_int_sol, "KNN RF")

rf_lof_no_int_sol <- train(Price ~ .+.,
                       data = data_lof_train,
                       method = 'rf', 
                       tunegrid = tunegrid_rf,
                       trControl = fitControl)

anom_metrics <- rbind(anom_metrics, get_metrics(rf_lof_no_int_sol, "LOF RF"))

rf_AE_no_int_sol <- train(Price ~ .+.,
                       data = data_AE_train,
                       method = 'rf', 
                       tunegrid = tunegrid_rf,
                       trControl = fitControl)

anom_metrics <- rbind(anom_metrics, get_metrics(rf_AE_no_int_sol, "AE RF"))
################################################################################
# Baseline solutions SHAP
bl_lm_no_int_shap_values <- kernelshap(baseline_lm_no_int_sol, X = model_data %>% select(-Price))
bl_lm_int_shap_values <- kernelshap(baseline_lm_int_sol, X = model_data %>% select(-Price))
bl_rf_no_int_shap_values <- kernelshap(baseline_rf_no_int_sol, X = model_data %>% select(-Price))
bl_gbm_no_int_shap_values <- kernelshap(baseline_gbm_sol, X = model_data %>% select(-Price))

sv_bl_lm_no_int <- shapviz(bl_lm_no_int_shap_values)
sv_bl_lm_int <- shapviz(bl_lm_int_shap_values)
sv_bl_rf <- shapviz(bl_rf_no_int_shap_values)
sv_bl_gbm <- shapviz(bl_gbm_no_int_shap_values)

# GA solutions SHAP
lm_no_int_shap_values <- kernelshap(lm_no_int_sol, X = train_set_lm %>% select(-Price))
lm_int_shap_values <- kernelshap(lm_int_sol, X = train_set_lm %>% select(-Price))
rf_no_int_shap_values <- kernelshap(rf_no_int_sol, X = train_set_rf %>% select(-Price))
gbm_no_int_shap_values <- kernelshap(gbm_sol, X = train_set_gbm %>% select(-Price))

sv_lm_no_int <- shapviz(lm_no_int_shap_values)
sv_lm_int <- shapviz(lm_int_shap_values)
sv_rf <- shapviz(rf_no_int_shap_values)
sv_gbm <- shapviz(gbm_no_int_shap_values)

# Plotting
# Baseline solutions SHAP
sv_importance(sv_bl_lm_no_int, kind = "bee")
sv_importance(sv_bl_lm_int, kind = "bee")
sv_importance(sv_bl_rf, kind = "bee")
sv_importance(sv_bl_gbm, kind = "bee")

sv_importance(sv_bl_lm_no_int, show_numbers = TRUE)
sv_importance(sv_bl_lm_int, show_numbers = TRUE)
sv_importance(sv_bl_rf, show_numbers = TRUE)
sv_importance(sv_bl_gbm, show_numbers = TRUE)

# GA solutions SHAP
sv_importance(sv_lm_no_int, kind = "bee")
sv_importance(sv_lm_int, kind = "bee")
sv_importance(sv_rf, kind = "bee")
sv_importance(sv_gbm, kind = "bee")

sv_importance(sv_lm_no_int, show_numbers = TRUE)
sv_importance(sv_lm_int, show_numbers = TRUE)
sv_importance(sv_rf, show_numbers = TRUE)
sv_importance(sv_gbm, show_numbers = TRUE)

rf_knn_shap_values <- kernelshap(rf_knn_no_int_sol, X = data_knn_no_anom_train %>% select(-Price))
sv_knn <- shapviz(rf_knn_shap_values)

sv_importance(sv_knn, show_numbers = TRUE)
sv_importance(sv_knn, kind = "bee")

################################################################################
# BOOTSTRAPPING

# Keep 20% of the data set for validation set
val_samples <- train_test_split(model_data,0.2)
validation_df <- model_data[val_samples, ]
train_test_df <- model_data[!val_samples, ]

# Train-test split
train_samples <- train_test_split(train_test_df, 0.7)
train_df <- train_test_df[train_samples, ]
test_df <- train_test_df[!train_samples, ]

# Train model
train_lm_model_no_int <- train(Price ~ .+., train_df,
                            method = 'lm')
test_preds <- predict(train_lm_model_no_int, newdata = test_df)
obs_v_preds <- data.frame(test_preds, test_df$Price)
colnames(obs_v_preds)[1] <- "Predictions"
colnames(obs_v_preds)[2] <- "Observations"

# Determine the relationship between predictons and observations
rel_obs_preds <- lm(Observations ~ Predictions, obs_v_preds)
rel_obs_preds_model <- summary(rel_obs_preds)$coefficients

obs_v_preds["rel"] <- rel_obs_preds_model[2]*obs_v_preds$Prediction + 
  rel_obs_preds_model[1]


# Add equation
lm_equation <- function(model_summary) {
  eq <- substitute(italic(Observation) == m %.% italic(Price) + c,
                   list(m = format(unname(model_summary[2]), digits = 2),
                        c = format(unname(model_summary[1]), digits = 2)
                   ))
  as.character(as.expression(eq))
}

ggplot(data = obs_v_preds, aes(x = Predictions)) +
  geom_point(aes(y = Observations, colour = 'Observation'), alpha = 0.6) +
  geom_line(aes(y = rel, colour = 'Regression'), alpha = 1,  size = 1) +
  labs(title = "Predictions vs Observations of Price",
       x = "Prediction",
       y = "Observation") +
  scale_color_brewer(palette = "Set1") +
  geom_text(x =1.5, y = 15, label = lm_equation(rel_obs_preds_model), parse = TRUE)
  
