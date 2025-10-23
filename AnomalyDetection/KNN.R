setwd("Path/to/your/folder")

################################################################################
# Load libraries and data
################################################################################
library(dplyr)
#library(FNN)
library(caret)
library(dbscan)

#data <- read.csv('Domain_Housing_Data_250511 - Copy.csv')
data = read.csv('Domain_housing_data_all_coordinates.csv')
colnames(data)[1] <- "Date"

################################################################################
# Transform the data
################################################################################
processed_data <- data[, !colnames(data) %in% c('Date','Address', 'longitude', 
                                                'latitude')]
data["SaleType"] <- lapply(data["SaleType"], 
                               function(x) replace(x, x == "", "AUCTION"))
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
# KNN
################################################################################
N <- nrow(model_data)
sqrt_N <- 2*round(sqrt(N))

# Create a grid of hyper parameters for tuning
results <- expand.grid(k =  seq(2,sqrt_N, by=2), 
                       threshold = seq(0.80, 0.99, by = 0.01))

for (i in 1:nrow(results)) {
  # Calculate distance
  knn_dist <- kNNdist(model_data, k = results$k[i])
  
  # Find the number of anomalies found
  num_anoms <- sum(knn_dist > quantile(knn_dist, results$threshold[i]))
  results$num_anoms[i] <- num_anoms  # Using number of anomalies as a validation metric
  results$pt_anoms[i] <- num_anoms / N * 100
}

# Add index to the results data frame for plotting
results$i <- seq(1,nrow(results), by = 1)

ggplot(data = results, aes(x = i, y = pt_anoms, label = pt_anoms)) + 
  geom_point() + 
  theme(axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold")) +
  labs(x = "Model number", y = "Number of Anomalies (%)", 
       title = "Number of anomalies as percentage of total dataset")

KNN_data <- model_data

fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5, 
                           savePredictions = "final")

knn_rmse <- c()
# Calculate the RMSE for each count of K values
for (k in seq(2,sqrt_N, by = 2)) {
  # Calculate distance
  knn_dist <- kNNdist(model_data, k = k)
  threshold <- quantile(knn_dist, 0.99)  
  
  KNN_data[paste("k.",k, sep="")] <- knn_dist
  KNN_data[paste("isAnom_k.",k,sep="")] <- ifelse(knn_dist > threshold,
                                                  "Anomaly",
                                                  "Normal")
  
  # Exclude anomalous points and calculate RMSE for regression
  temp <- KNN_data[KNN_data[paste("isAnom_k.",k,sep="")] == "Normal", model_data_cols]
  lm_model <- train(Price ~ .+. , temp, trControl = fitControl, method = 'lm')
  knn_rmse <- c(knn_rmse, sqrt(mean((lm_model$pred$obs - lm_model$pred$pred)^2)))
}

knn_models <- data.frame(k = seq(2,sqrt_N, by = 2), RMSE = knn_rmse)

ggplot(data = knn_models, aes(x = k, y = RMSE)) +
  geom_point() +
  geom_point(data = knn_models[which.min(knn_models$RMSE),], 
             colour = '#ff77ff',
             size = 4,
             shape = 18) +
  theme(axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold")) +
  labs(x = "Nearest Neighbours", y = "RMSE")

min(knn_models$RMSE)
knn_models[which.min(knn_models$RMSE), "k"]
write.csv(KNN_data,"Domain_housing_data_all_dist_knn_fin.csv", row.names = FALSE)

################################################################################
# Plot the anomalies
################################################################################
KNN_data <- read.csv("Domain_housing_data_all_dist_knn_fin.csv")
data_knn <- data
data_knn$k <- KNN_data$k.28
data_knn$isAnom = KNN_data$isAnom_k.28

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
  #   [+] y: the name of the variable to place in y axis
  #   [+] outlier_col: the column containing labels "Anomaly" and "Normal"
  #   [+] jitter: (optional) the amount of jitter to apply. If null, no jitter is used
  #   [+] x_log_scale: (optional) boolean value specifying whether or not to use log scale in x axis
  #   [+] y_log_scale: (optional) boolean value specifying whether or not to use log scale in y axis
  #   [+] x_angle: (optional) boolean value specifying whether or not x labels should be displayed at an angle
  #
  # Output:
  # --------
  #   plot of anomalies and normal points 
  
  # Split normal and anomalous data for cleaner plotting
  df_normal <- df %>% filter(!!sym(outlier_col) == "Normal")
  df_anom <- df %>% filter(!!sym(outlier_col) == "Anomaly")
  
  p <- ggplot(df_normal, aes(x = !!sym(feature), y=!!sym(y)), color = "black") +
    geom_point() +
    theme(axis.text=element_text(size=12),
          axis.title = element_text(size=18, face = "bold"),
          plot.title = element_text(size=22, face = "bold")) +
    theme(legend.position = "none")
  
  if (x_log_scale) {
    p <- p + scale_x_log10(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
  }
  
  if (y_log_scale) {
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
# k for min average LOF
plot_anomalies(data_knn, "Beds", "Price", "isAnom", y_log_scale = TRUE, jitter = 0.25)
plot_anomalies(data_knn, "Baths", "Price", "isAnom", y_log_scale = TRUE, jitter = 0.25)
plot_anomalies(data_knn, "Cars", "Price", "isAnom", y_log_scale = TRUE, jitter = 0.25)
plot_anomalies(data_knn, "dist_to_cbd", "Price", "isAnom", y_log_scale = TRUE, x_log_scale = TRUE)
plot_anomalies(data_knn, "Land", "Price", "isAnom", y_log_scale = TRUE, x_log_scale = TRUE)
plot_anomalies(data_knn, "SaleType", "Price", "isAnom", y_log_scale = TRUE, jitter = 0.03)
plot_anomalies(data_knn, "Type", "Price", "isAnom", y_log_scale = TRUE, jitter = 0.1, x_angle = TRUE)
plot_anomalies(data_knn, "State", "Price", "isAnom", y_log_scale = TRUE, jitter = 0.1)


plot_anomalies(data_knn, "Beds", "Baths", "isAnom", jitter = 0.25)
plot_anomalies(data_knn, "Beds", "dist_to_cbd", "isAnom", y_log_scale = TRUE, jitter = 0.25)
plot_anomalies(data_knn, "Beds", "Cars", "isAnom", jitter = 0.25)
plot_anomalies(data_knn, "Beds", "Baths", "isAnom", jitter = 0.25)
plot_anomalies(data_knn, "Type", "Beds", "isAnom", jitter = 0.25, x_angle = TRUE)
plot_anomalies(data_knn, "Beds", "Land", "isAnom", y_log_scale = TRUE, jitter = 0.3)
plot_anomalies(data_knn, "dist_to_cbd", "Land", "isAnom", x_log_scale = TRUE, y_log_scale = TRUE, jitter = 0.3)
