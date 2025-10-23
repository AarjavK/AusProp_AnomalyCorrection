setwd("Path/to/your/folder")

################################################################################
# Scrape domain.com.au for housing data
################################################################################
library(tidyverse)
library(ggplot2)
library(ggalt) # For spline
library(dplyr)
library(caret)
library(dbscan)
library(reshape)
library(scales)
library(rlang) # for !! and sym

################################################################################
# Pre-processing
################################################################################
#data = read.csv('Domain_housing_data_all.csv')
data = read.csv('Domain_housing_data_all_coordinates.csv')
colnames(data)[1] <- "Date"

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
# LOF
################################################################################
lof_averages <- c()

sqrt_N <- 2*round(sqrt(nrow(model_data))) 
k <- seq(2,sqrt_N, by=2)

results <- expand.grid(k = k, threshold = seq(0.80, 0.99, by = 0.01))
#model_data = read.csv("Domain_housing_data_all_dist_LOF.csv")
for (i in 1:nrow(results)) {
  lofs <- lof(model_data, minPts = results$k[i])
  
  model_data[paste("LOF_k.", i, sep = "")] <- lofs
  lof_averages <- c(lof_averages, mean(ifelse(model_data[, paste("LOF_k.", i, sep = "")] == Inf, 0, model_data[, paste("LOF_k.", i, sep = "")])))
  
  # Find the number of anomalies found based on threshold
  num_anoms <- sum(lofs > quantile(lofs, results$threshold[i]))
  
  # Using number of anomalies as a validation metric
  results$num_anoms[i] <- num_anoms
  results$pt_anoms[i] <- num_anoms / N * 100
}

# Add index to the results data frame for plotting
results$i <- seq(1,nrow(results), by = 1)

ggplot(data = results, aes(x = i, y = pt_anoms, label = pt_anoms)) + 
  geom_point() + 
  theme(axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold")) +
  labs(x = "Model number", y = "Number of Anomalies (%)", 
       title = "Number of anomalies as percentage of total dataset (LOF)")

lof_ave_df <- data.frame(k, average_LOF = lof_averages)
spline <- as.data.frame(spline(lof_ave_df$k, lof_ave_df$average_LOF))

#write.csv(model_data,"Domain_housing_data_all_LOF.csv", row.names = FALSE)
#write.csv(model_data,"Domain_housing_data_all_dist_LOF.csv", row.names = FALSE)
write.csv(model_data,"Domain_housing_data_all_dist_LOF_fin.csv", row.names = FALSE)

ggplot(data=lof_ave_df, aes(x=k, average_LOF, label = average_LOF)) +
  stat_smooth(aes(x = k, y = average_LOF), 
              formula = y ~ s(x, k = sqrt_N - 1), 
              method = "gam", 
              se = FALSE) +
  geom_point() +
  geom_point(data = lof_ave_df[which.min(lof_ave_df$average_LOF),], 
             colour = '#ff77ff',
             size = 4,
             shape = 18) +
  geom_point(data = lof_ave_df[which.max(lof_ave_df$average_LOF),], 
             colour = 'red',
             size = 4,
             shape = 18) +
  geom_text(aes(label = ifelse(average_LOF == min(average_LOF), 
                               as.character(round(average_LOF, digits=3)), 
                               ''),
                hjust = 0.5, vjust = -0.75)) +
  geom_text(aes(label = ifelse(average_LOF == max(average_LOF), 
                               as.character(round(average_LOF, digits=3)), 
                               ''),
                hjust = 0.5, vjust = -0.75)) +
  theme(axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold")) +
  scale_x_continuous(breaks = seq(0, max(lof_ave_df$k), by = 10)) +
  scale_y_continuous(breaks = seq(0, max(lof_ave_df$average_LOF), by = 0.25)) +
  labs(x = "Num. Neighbours (k)", y = "Mean LOF", title = "Mean LOF over neighbour count")

lof_data <- model_data

fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 5, 
                           savePredictions = "final")

lof_rmse <- c()
# Calculate the RMSE for each count of K values
for (k in seq(2,186, by = 2)) {
  # Calculate distance
  lofs <- lof(model_data, minPts = k)
  threshold <- quantile(lofs, 0.99)  
  
  lof_data[paste("k.",k, sep="")] <- lofs
  lof_data[paste("isAnom_k.",k,sep="")] <- ifelse(lofs > threshold,
                                                  "Anomaly",
                                                  "Normal")
  
  # Exclude anomalous points and calculate RMSE for regression
  temp <- lof_data[lof_data[paste("isAnom_k.",k,sep="")] == "Normal", model_data_cols]
  lm_model <- train(Price ~ .+. , temp, trControl = fitControl, method = 'lm')
  lof_rmse <- c(lof_rmse, sqrt(mean((lm_model$pred$obs - lm_model$pred$pred)^2)))
}

lof_models <- data.frame(k = seq(2,186, by = 2), RMSE = lof_rmse)

ggplot(data = lof_models, aes(x = k, y = RMSE)) +
  geom_point() +
  geom_point(data = lof_models[which.min(lof_models$RMSE),], 
             colour = '#ff77ff',
             size = 4,
             shape = 18) +
  theme(axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold")) +
  labs(x = "Nearest Neighbours (LOF)", y = "RMSE")

min(lof_models$RMSE)
lof_models[which.min(lof_models$RMSE), "k"]

# set threshold
threshold <- quantile(model_data[paste("LOF_k.",which.min(lof_averages)*2, sep="")],0.99)

#scaled_df_min <- model_data[ , c("Beds", "Baths", "Cars", "Land", "Median.price", "dist_to_cbd", "LOF_k.186")] 
#scaled_df_max <- model_data[ , c("Beds", "Baths", "Cars", "Land", "Median.price", "dist_to_cbd", "LOF_k.2")] 

#model_data <- read.csv("Domain_housing_data_all_dist_LOF_fin.csv")
scaled_df_min <- model_data[ , append(c(model_data_cols), paste("LOF_k.",which.min(lof_averages)*2, sep=""))] 
scaled_df_max <- model_data[ , append(c(model_data_cols), paste("LOF_k.",which.max(lof_averages)*2, sep=""))] 
scaled_df_min$State = processed_data$State
scaled_df_max$State = processed_data$State

# Rename column for LOF values
min_occ_k <- which.min(lof_averages)*2
max_occ_k <- which.max(lof_averages)*2
colnames(scaled_df_min)[colnames(scaled_df_min) == "LOF_k.186"] <- "lof_scores"
#colnames(scaled_df_min)[colnames(scaled_df_min) == paste("LOF_k.",min_occ_k, sep="")] <- "lof_scores"
#colnames(scaled_df_max)[colnames(scaled_df_max) == paste("LOF_k.",max_occ_k, sep="")] <- "lof_scores"

scaled_df_min$outliers <- ifelse(scaled_df_min$lof_scores > threshold, "Anomaly", "Normal")
scaled_df_max$outliers <- ifelse(scaled_df_max$lof_scores > threshold, "Anomaly", "Normal")

model_data$min_outliers <- scaled_df_min$outliers
model_data$max_outliers <- scaled_df_max$outliers

data$min_outliers <- scaled_df_min$outliers
data$max_outliers <- scaled_df_max$outliers

# Number of anomalies identified
sum(data$min_outliers == "Anomaly")
sum(data$min_outliers == "Anomaly") / nrow(data) * 100
################################################################################
# Plot
################################################################################
plot_anomalies <- function(df,
                           feature,
                           y,
                           outlier_col,
                           k_val, 
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
  #   [+] k_val: the value of k to use in the plot title
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

# k for min average LOF
plot_anomalies(data, "Beds", "Price", "min_outliers", min_occ_k, y_log_scale = TRUE, jitter = 0.25)
plot_anomalies(data, "Baths", "Price", "min_outliers", min_occ_k, y_log_scale = TRUE, jitter = 0.25)
plot_anomalies(data, "Cars", "Price", "min_outliers", min_occ_k, y_log_scale = TRUE, jitter = 0.25)
plot_anomalies(data, "dist_to_cbd", "Price", "min_outliers", min_occ_k, y_log_scale = TRUE, x_log_scale = TRUE)
plot_anomalies(data, "Land", "Price", "min_outliers", min_occ_k, y_log_scale = TRUE, x_log_scale = TRUE)
plot_anomalies(data, "SaleType", "Price", "min_outliers", min_occ_k, y_log_scale = TRUE, jitter = 0.05)
plot_anomalies(data, "Type", "Price", "min_outliers", min_occ_k, y_log_scale = TRUE, jitter = 0.1, x_angle = TRUE)
plot_anomalies(data, "State", "Price", "min_outliers", min_occ_k, y_log_scale = TRUE, jitter = 0.1)

# k for max average LOFs
plot_anomalies(data, "Beds", "Price", "max_outliers", max_occ_k, y_log_scale = TRUE, jitter = 0.25)
plot_anomalies(data, "Baths", "Price", "max_outliers", max_occ_k, y_log_scale = TRUE, jitter = 0.25)
plot_anomalies(data, "Cars", "Price", "max_outliers", max_occ_k, y_log_scale = TRUE, jitter = 0.25)
plot_anomalies(data, "dist_to_cbd", "Price", "max_outliers", max_occ_k, y_log_scale = TRUE, x_log_scale = TRUE)
plot_anomalies(data, "Land", "Price", "max_outliers", max_occ_k, y_log_scale = TRUE, x_log_scale = TRUE)
plot_anomalies(data, "SaleType", "Price", "max_outliers", max_occ_k, y_log_scale = TRUE, jitter = 0.3)
plot_anomalies(data, "Type", "Price", "max_outliers", max_occ_k, y_log_scale = TRUE, jitter = 0.1, x_angle = TRUE)
plot_anomalies(data, "State", "Price", "max_outliers", min_occ_k, y_log_scale = TRUE, jitter = 0.1)

