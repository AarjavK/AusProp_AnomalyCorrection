setwd("Path/to/your/folder")

################################################################################
# Load libraries and data
################################################################################
library(dplyr)
library(FNN)
library(caret)

data <- read.csv('file.csv')
# For some reason, the csv file has different name for date field
colnames(data)[1] <- "Date" 

################################################################################
# Transform the data
################################################################################
data <- na.omit(data)
data["SaleType"] <- lapply(data["SaleType"], 
                           function(x) replace(x, x == "", "AUCTION"))

# Convert categorical columns to one-hot encodings
cat_cols <- c("Suburb", "State", "Type", "Postcode", "SaleType")

dummies <- dummyVars(" ~ Suburb + State + Type", data = data) 
encoded_df <- predict(dummies, newdata = data)  # Apply encoding
encoded_df <- as.data.frame(encoded_df)

# Add the all the numeric values to the encoded variables
encoded_df <- cbind(data[ , !(colnames(data) %in% c("Suburb",
                                                    "State",
                                                    "Type",
                                                    "Address",
                                                    "Date",
                                                    "SaleType"))], encoded_df)
encoded_df <- encoded_df %>% mutate_all(as.numeric)

# Scale the numeric variables (Price, beds, baths, cars, land)
numeric_cols <- c("Price", "Land")
scaled_df <- encoded_df
#scaled_df[ , colnames(scaled_df) %in% numeric_cols] <- scale(scaled_df[, colnames(scaled_df) %in% 
#                           numeric_cols])

################################################################################
# KNN
################################################################################
# Calculate distances and get the k closest points to each observation
distances <- get.knn(scaled_df, k=5)

# Calculate distance score
dist_scores <- rowMeans(distances$nn.dist)

encoded_df$dist_score <- dist_scores

# Set the anomaly threshold
threshold <- as.numeric(summary(encoded_df$dist_score)[6]/2)
scaled_df$isAnomaly <- ifelse(encoded_df$dist_score > threshold, 
                              "Anomaly", 
                              "Normal")

################################################################################
# Plot the anomalies
################################################################################
price_anom <- ggplot(scaled_df, aes(x = Beds, y=Price, color=isAnomaly, 
                                     shape = isAnomaly)) +
  geom_point() +
  scale_color_manual(values = c("Anomaly" = "red", "Normal" = "black")) +
  labs(title = 'KNN - k=17 with Beds v Price') +
  theme(axis.text=element_text(size=12),
        axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold")) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", 
                                               scientific = FALSE),
                     limits=c(min(scaled_df$Price), max(scaled_df$Price)))

baths_anom <- ggplot(encoded_df, aes(x = Baths, y=Price, color=isAnomaly)) +
  geom_point() +
  scale_color_manual(values = c("Anomaly" = "red", "Normal" = "black")) +
  labs(title = 'KNN - k=20 with Bath') + 
  theme(axis.text=element_text(size=12),
        axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold"))

