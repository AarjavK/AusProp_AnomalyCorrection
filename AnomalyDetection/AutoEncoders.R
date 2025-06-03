setwd("Path/to/your/folder")

################################################################################
# Load libraries and data
################################################################################
library(dplyr)
library(h2o)
library(ggplot2)
# Required if H2O is not detecting path to JDK
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jdk-24") 

df <- read.csv("file.csv")
################################################################################
# Transform the data
################################################################################
colnames(df)[1] <- "Date"
df <- df[ , !colnames(df) %in% c("Date", "SaleType", "Address")]

# Converting categorical variables as factors
cat_cols <- c("Suburb", "State", "Type", "Postcode")
cat_features <- df[ , cat_cols]
cat_features <- cat_features %>% mutate_all(as.factor)  %>% mutate_all(as.numeric)

df_processed <- df
df_processed[ , cat_cols] <- cat_features

# Add median price for each suburb based on the 1000 properties per suburb sold in the past few months
df_processed <- df %>% 
  group_by(Suburb) %>%
  summarise(suburb_mean = mean(Price))

df_processed <- merge(df, df_processed, by = "Suburb")

# Scale suburb mean price and land
df_processed[ , c("Land", "suburb_mean")] <- scale(df_processed[ , c("Land", "suburb_mean")])

# Drop NA values - we don't have enough features or enough data to allow imputation
df_processed <- na.omit(df_processed)

################################################################################
# TRAIN AE                                                                     #
################################################################################
# Train-test split
sample_size <- floor(0.7*nrow(df_processed))
samples <- sample(c(TRUE, FALSE), nrow(df_processed), 
                  replace = TRUE, prob = c(0.7,0.3))

features <- df_processed[, !colnames(df_processed) %in% "Price"]
train_set <- df_processed[samples, ]
test_set <- df_processed[!samples, ]


h2o.init()
train_h2o <- as.h2o(train_set)
test_h2o <- as.h2o(test_set)

# Train autoencoder model
model_1 <- h2o.deeplearning(x=c(1,2,3,4,5,6,7,8,10),
                            training_frame = train_h2o,
                            model_id = "Second",
                            autoencoder = TRUE,
                            reproducible = FALSE, # To enable multi-threading
                            ignore_const_cols = FALSE,
                            seed = 42,
                            hidden = c(8, 6, 3), 
                            activation = 'Tanh'
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
# Number of anomalies found
which(results$Reconstruction.MSE == max(results$Reconstruction.MSE))

################################################################################
# Plot                                                                         #
################################################################################
ggplot(data = results, aes(x=1:dim(results)[1], y=Reconstruction.MSE)) +
  geom_point(aes(colour = isAnomaly), alpha = 0.6) +
  scale_color_manual(values=c("gray", "#AA000F")) +
  labs(title = 'Anomaly detection using Auto encoder with 2 hidden layers',
       x = 'Observation #',
       y = 'Anomaly Score / Reconstruction MSE') +
  scale_y_continuous(limits = c(0,1.2)) +
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
