setwd("D:/Documents/大学/LatrobeMasterDataScience/THESIS/code")

################################################################################
# Scrape domain.com.au for housing data
################################################################################
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tictoc)

data = read.csv('Domain_housing_data_all.csv')
colnames(data)[1] <- "Date"

################################################################################
# Transform the data
################################################################################
processed_data <- na.omit(data)
processed_data["SaleType"] <- lapply(processed_data["SaleType"],
                                     function(x) replace(x, x == "", "AUCTION"))

cat_cols <- c("Suburb", "State", "Type", "Postcode", "SaleType")
cat_features <- processed_data[ , cat_cols]
cat_features <- cat_features %>% mutate_all(as.factor) %>%
  mutate_all(as.numeric)

scaling_cols <- c("Beds", "Baths", "Cars", "Land", "Median.price", "Price")
processed_data[scaling_cols] <- scale(processed_data[scaling_cols])

model_data <- processed_data[, !colnames(processed_data) %in% c("Date", "Address")]

################################################################################
# MODELING
################################################################################
# Base line linear regress model
tic("Main effects - LM")
lm_no_int_sol <- lm(Price ~ .+. , model_data)
toc("Main effects - LM")

lm_ga_sol <- c("Suburb", "State", "Postcode", "Baths", "Cars",
               "Type", "SaleType", "Median.price", "Price")

tic("GA effects - LM")
lm_no_ga_int_sol <- lm(Price ~ .+. , model_data[, colnames(model_data) %in% lm_ga_sol])
toc("GA effects - LM")
