setwd("Path/to/your/folder")

################################################################################
# Load libraries and data
################################################################################
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-24")
library(microbenchmark)
library(dplyr)
library(tidyr)
library(h2o)
library(tictoc)
library(ggplot2)
library(caret)
library(rlang) # for !! and sym

################################################################################
# Pre-processing
################################################################################
#data = read.csv('Domain_housing_data_all.csv')
data = read.csv('Domain_housing_data_all_coordinates.csv')
colnames(data)[1] <- "Date"

processed_data <- data[, !colnames(data) %in% c('Date','Address', 'Suburb', 'Postcode')]
processed_data["SaleType"] <- lapply(processed_data["SaleType"],
                                     function(x) replace(x, x == "", "AUCTION"))
processed_data <- na.omit(processed_data)

data <- processed_data

cat_cols <- c("State", "Type", "SaleType")
cat_features_dummies <- dummyVars(" ~ State + Type + SaleType", data = processed_data)
cat_features <- predict(cat_features_dummies, newdata = processed_data)
cat_features <- as.data.frame(cat_features)

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
model_data$State <- data$State
################################################################################
# TRAIN AE                                                                     #
################################################################################
# Train-test split
trainIndex <- createDataPartition(y = model_data$Price, p = 0.7, list = FALSE)

train_set <- model_data[trainIndex, ]
test_set <- model_data[-trainIndex, ]

h2o.init()
train_h2o <- as.h2o(train_set)
test_h2o <- as.h2o(test_set)

# Train autoencoder model
hyper_params <- list(
  hidden = list(c(28, 20, 15, 10, 6, 3), 
                c(28, 20, 10, 8, 6), 
                c(28,20,10,8),
                c(28,14,7),
                c(28,20),
                c(28,15),
                c(14),
                c(14,7),
                c(14,10,7),
                c(14,10,7,3)        
  ),
  activation = c("Rectifier", "Tanh", "TanhWithDropout","RectifierWithDropout"),
  l1 = c(0, 1e-4, 1e-6),
  input_dropout_ratio = c(0, 0.1, 0.2)
)
search_criteria <- list(
  strategy = "RandomDiscrete",
  #max_models = 10,  # Maximum number of models to build
  max_runtime_secs = 30*3600 # Maximum runtime in seconds
)

tic("start grid auto")
grid_id <- h2o.grid(
  algorithm = "deeplearning",
  x=colnames(model_data[, !colnames(model_data) %in% "Price"]),
  training_frame = train_h2o,
  autoencoder = TRUE,
  hyper_params = hyper_params,
  search_criteria = search_criteria,
  grid_id = "autoencoder_grid"
)
toc()

############################### EVALUATE MODELS ################################
# Get the grid results
grid_results <- h2o.getGrid(grid_id = "autoencoder_grid")

# Get the mean reconstruction error for each model
model_ids <- grid_results@model_ids
models <- lapply(model_ids, h2o.getModel)

# Function to compute reconstruction error (MSE) on a dataset
get_reconstruction_mse <- function(model, data) {
  # Function to calculate the mean square reconstruction error for a given model
  # and dataset
  #
  # Inputs:
  # -------
  #    [+] model: model trained by h2o
  #    [+] data: the dataset
  #
  # Output:
  # -------
  #    [+] MSE 
  errors <- h2o.anomaly(model, data)
  mean(as.vector(errors))
}

# Build a summary table with train + test MSE
results <- data.frame(
  model_id = character(),
  train_mse = numeric(),
  test_mse = numeric(),
  stringsAsFactors = FALSE
)


for (model in models) {
  train_mse <- get_reconstruction_mse(model, train_h2o)
  test_mse  <- get_reconstruction_mse(model, test_h2o)
  
  # Append row to dataframe
  results <- rbind(results, data.frame(
    model_id = model@model_id,
    train_mse = train_mse,
    test_mse = test_mse,
    stringsAsFactors = FALSE
  ))
}

# PLOT results
results$model_num <- seq_len(nrow(results))

# Reshape into long format for plotting
results_long <- results %>%
  pivot_longer(cols = c(train_mse, test_mse),
               names_to = "dataset",
               values_to = "mse")

ggplot(results_long, aes(x = model_num, y = mse, color = dataset, group = dataset)) +
  geom_line(size = 1) +
  geom_point(size = 1, alpha = 0.6) +
  labs(x = "Model Number",
       y = "Mean Squared Reconstruction Error",
       color = "Dataset") +
  scale_y_continuous(limits=c(min(results_long$mse),max(quantile(results_long$mse, probs = 0.97)))) +
  theme(axis.text=element_text(size=13),
        axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold"))

# Sort by test MSE (ascending)
sorted_results <- results[order(results$test_mse), ]

print(sorted_results)

# Get best model (lowest test MSE)
best_model_id <- results$model_id[1]
best_model <- h2o.getModel(best_model_id)

paste("Best model is:", best_model_id, 
    "with Train MSE =", results$train_mse[1],
    "and Test MSE =", results$test_mse[1], sep=" ")
best_model

# Get the worst model (highest test MSE)
worst_model_id <- results$model_id[nrow(results)]
worst_model <- h2o.getModel(worst_model_id)

paste("Worst model is:", worst_model_id, 
      "with Train MSE =", results$train_mse[1],
      "and Test MSE =", results$test_mse[1], sep=" ")
worst_model


################################################################################

best_anmlt <- h2o.anomaly(best_model, train_h2o, per_feature = FALSE) %>%
  as.data.frame()

# create a label for healthy data
best_anmlt$y = 0

# calculate thresholds from train data
#best_threshold <- quantile(best_anmlt$Reconstruction.MSE, probs = 0.9544)
best_threshold <- quantile(best_anmlt$Reconstruction.MSE, probs = 0.99)

# Calculate anomaly scores for test set
best_test_anmlt <- h2o.anomaly(best_model, test_h2o, per_feature = FALSE) %>% 
  as.data.frame()
best_test_anmlt$y = 1
best_model_results <- data.frame(rbind(best_anmlt,best_test_anmlt), best_threshold)

best_model_results$isAnomaly <- ifelse(best_model_results$Reconstruction.MSE > best_threshold, "Anomaly", "Normal")


best_model_results$isAnomaly <- as.factor(best_model_results$isAnomaly)
temp <- cbind(model_data, best_model_results)
processed_data <- temp[, !colnames(temp) %in% c("y","threshold")]
write.csv(processed_data,"Domain_housing_data_all_AE_best_fin.csv", row.names = FALSE)
# Number of anomalies found
sum(best_model_results$Reconstruction.MSE >= best_threshold)

################################################################################
# Plot                                                                         #
################################################################################
ggplot(data = best_model_results, aes(x=1:nrow(best_model_results), y=Reconstruction.MSE)) +
  geom_point(aes(colour = isAnomaly), alpha = 0.6) +
  scale_color_manual(values=c("gray", "#AA000F")) +
  labs(title = 'Auto encoder with 3 hidden layers',
       x = 'Observation #',
       y = 'Anomaly Score / Reconstruction MSE') +
  scale_y_continuous(limits = c(0,0.25)) +
  theme(axis.text=element_text(size=16),
        axis.title = element_text(size=16, face = "bold"),
        plot.title = element_text(size=16, face = "bold"))

data$Reconstruction.MSE <- best_model_results$Reconstruction.MSE
data$isAnomaly <- best_model_results$isAnomaly
write.csv(data,"Domain_housing_data_all_AE_best_fin2.csv", row.names = FALSE)
plot_anomalies <- function(df,
                           feature,
                           y,
                           outlier_col,
                           jitter = NULL,
                           x_log_scale = FALSE, 
                           y_log_scale = FALSE, 
                           x_angle = FALSE) {
  # Function to create plots highlighting anomalies
  #
  # Inputs:
  # --------
  #   [+] df: data frame containing data
  #   [+] feature: the name of the feature to plot against price (used in x-axis)
  #   [+] outlier_col: the column containing labels "Anomaly" and "Normal"
  #   [+] jitter: (optional) the amount of jitter to apply. If null, no jitter is used
  #   [+] x_log_scale: (optional) boolean value specifying whether or not to use log scale
  #   [+] y_log_scale: (optional) boolean value specifying whether or not to use log scale in y axis
  #   [+] x_angle: (optional) boolean value specifying whether or not x labels should be displayed at an angle
  #
  # Output:
  # --------
  #   plot of anomalies and normal points 
  
  # Split normal and anomalous data for cleaner plotting
  df_norm <- df %>% filter(!!sym(outlier_col) == "Normal")
  df_anom <- df %>% filter(!!sym(outlier_col) == "Anomaly")
  
  p <- ggplot(df_norm, aes(x = !!sym(feature), y=!!sym(y)), color="black") +
    geom_point() +
    theme(axis.text=element_text(size=12),
          axis.title = element_text(size=18, face = "bold"),
          plot.title = element_text(size=22, face = "bold")) +
    theme(legend.position = "none")
  if (x_log_scale) {
    p <- p + scale_x_log10(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
  }
  
  if(y_log_scale) {
    p <- p + scale_y_log10(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
  }
  
  if (x_angle){
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust =  1))
  }
  
  if (!is.null(jitter)) {
    p <- p + geom_jitter(data = df_anom,
                         aes(x = !!sym(feature), y = !!sym(y)),
                         width = jitter,
                         color = "red", size = 2, alpha = 0.8
    )
  } else {
    p <- p + geom_point(data = df_anom, 
                        aes(x = !!sym(feature), y = !!sym(y)),
                        size = 2,colour = "red", fill = "red", alpha = 0.8)
  }
  
  # Show the plot
  p
}

plot_anomalies(data, "Beds", "Price", "isAnomaly", y_log_scale = TRUE, jitter = 0.25)
plot_anomalies(data, "Baths", "Price", "isAnomaly", y_log_scale = TRUE, jitter = 0.25)
plot_anomalies(data, "Cars", "Price", "isAnomaly", y_log_scale = TRUE, jitter = 0.25)
plot_anomalies(data, "dist_to_cbd", "Price", "isAnomaly", y_log_scale = TRUE, x_log_scale = TRUE)
plot_anomalies(data, "Land", "Price", "isAnomaly", y_log_scale = TRUE, x_log_scale = TRUE)
#plot_anomalies(data, "SaleType", "Price", "isAnomaly", y_log_scale = TRUE, jitter = 0.25)
#plot_anomalies(data, "Type", "Price", "isAnomaly", y_log_scale = TRUE, jitter = 0.1, x_angle = TRUE)
plot_anomalies(data, "State", "Price", "isAnomaly", y_log_scale = TRUE, jitter = 0.1)


plot_anomalies(data, "Beds", "Baths", "isAnomaly", jitter = 0.25)
plot_anomalies(data, "Beds", "Cars", "isAnomaly", jitter = 0.25)
plot_anomalies(data, "Beds", "dist_to_cbd", "isAnomaly", y_log_scale = TRUE, jitter = 0.25)
plot_anomalies(data, "Beds", "Land", "isAnomaly", y_log_scale = TRUE, jitter = 0.25)
#########################################################
model_1 <- h2o.deeplearning(x=colnames(model_data[, !colnames(model_data) %in% "Price"]),
                            training_frame = train_h2o,
                            model_id = "Second",
                            nfolds = 5,
                            fold_assignment = "Stratified",
                            autoencoder = TRUE,
                            reproducible = FALSE, # To enable multi-threading
                            ignore_const_cols = FALSE,
                            seed = 42,
                            hidden = c(8, 6, 3), 
                            activation = 'Tanh',
                            standardize = TRUE,
                            categorical_encoding = "OneHotInternal"
)

# now we need to calculate MSE or anomaly score  
anmlt = h2o.anomaly(model_1, train_h2o, per_feature = FALSE) %>% 
  as.data.frame()
# create a label for healthy data
anmlt$y = 0

# calculate thresholds from train data
threshold <- quantile(anmlt$Reconstruction.MSE, probs = 0.9544)

# Calculate anomaly scores for test set
test_anmlt <- h2o.anomaly(model_1, test_h2o, per_feature = FALSE) %>% 
  as.data.frame()
test_anmlt$y = 1
results = data.frame(rbind(anmlt,test_anmlt), threshold)
results <- results %>%
  mutate(isAnomaly=case_when(Reconstruction.MSE <= threshold ~ FALSE, 
                             TRUE ~ TRUE))

results$isAnomaly <- as.factor(results$isAnomaly)
temp <- cbind(model_data, results)
processed_data <- temp[, !colnames(temp) %in% c("y","threshold")]
write.csv(processed_data,"Domain_housing_data_all_AE3.csv", row.names = FALSE)
# Number of anomalies found
which(results$Reconstruction.MSE == max(results$Reconstruction.MSE))

################################################################################
# Plot                                                                         #
################################################################################
ggplot(data = results, aes(x=1:dim(results)[1], y=Reconstruction.MSE)) +
  geom_point(aes(colour = isAnomaly), alpha = 0.6) +
  scale_color_manual(values=c("gray", "#AA000F")) +
  labs(title = 'Auto encoder with 3 hidden layers',
       x = 'Observation #',
       y = 'Anomaly Score / Reconstruction MSE') +
  scale_y_continuous(limits = c(0,0.25)) +
  theme(axis.text=element_text(size=16),
        axis.title = element_text(size=16, face = "bold"),
        plot.title = element_text(size=16, face = "bold"))

df_processed$isAnomaly <- results$isAnomaly
df_processed$Reconstruction.MSE <- results$Reconstruction.MSE
ggplot(data=df_processed, aes(x=Land, y=Beds, color=isAnomaly, shape=isAnomaly,
                              alpha=isAnomaly)) +
  geom_point(aes(colour = isAnomaly)) +
  scale_color_manual(values=c("gray", "#AA000F")) +
  labs(title = 'Anomaly detection using Auto encoder with 3 hidden layers') +
  theme(axis.text=element_text(size=16),
        axis.title = element_text(size=16, face = "bold"),
        plot.title = element_text(size=20, face = "bold"))

# 2 hidden layeys: c(6,3)
# 3 hidden layeys: c(8,6,3)
model_1
head(anmlt)
