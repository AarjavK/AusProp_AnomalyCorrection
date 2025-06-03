setwd("Path/to/your/folder")

################################################################################
# Load libraries and data
################################################################################
library(dplyr)
library(dbscan)
library(caret)

data <- read.csv('file.csv')
colnames(data)[1] <- "Date"

################################################################################
# Transform the data
################################################################################
data <- na.omit(data)
data["SaleType"] <- lapply(data["SaleType"], function(x) replace(x, x == "", "AUCTION"))

# Convert categorical columns to one-hot encodings
cat_cols <- c("Suburb", "State", "Type", "Postcode", "SaleType")

#dummies <- dummyVars(" ~ Suburb + State + Type + Postcode + SaleType", data = data) 
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

################################################################################
# LOF
################################################################################
lof_score <- lof(encoded_df, minPts = 5)
# Use 3rd quartile distance times 1.5 - arrived at this through iterative 
# process
threshold <- as.numeric(summary(lof_score))[3]*3/2

scaled_df$outliers <- ifelse(lof_score > threshold, "Anomaly", "Normal")

################################################################################
# Plot
################################################################################
price_anom <- ggplot(encoded_df, aes(x = Beds, y=Price, color=outliers, 
                                     shape=outliers)) +
  geom_point() +
  scale_color_manual(values = c("Anomaly" = "red", "Normal" = "black")) +
  labs(title = 'LOF - k=5 with Beds v Price') + 
  theme(axis.text=element_text(size=12),
        axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold")) +
  scale_y_continuous(labels=function(x) format(x, big.mark = ",", 
                                               scientific = FALSE),
                     limits=c(min(encoded_df$Price), max(encoded_df$Price)))

