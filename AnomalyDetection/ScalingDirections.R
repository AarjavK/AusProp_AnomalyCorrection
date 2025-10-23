################################################################################
# Scrape domain.com.au for housing data
################################################################################
library(tidyverse)
library(ggplot2)
library(dplyr)
library(caret)
library(rlang)
library(ggpubr)

################################################################################
# Pre-processing
################################################################################
#data = read.csv('Domain_housing_data_all.csv')
data = read.csv('Domain_housing_data_all_coordinates.csv')
colnames(data)[1] <- "Date"

processed_data <- data[, colnames(data) %in% c('State', 'Postcode', 'Beds',
                                               'Baths', 'Cars', 'Land',
                                               'Median.price', 'dist_to_cbd', 'Price')]
processed_data <- na.omit(processed_data)

data <- processed_data

cat_cols <- c("State")
state_features_dummies <- dummyVars(" ~ State", data = processed_data)
state_features <- predict(state_features_dummies, newdata = processed_data)
state_features <- as.data.frame(state_features)

processed_data <- cbind(processed_data[ , !colnames(processed_data) %in% cat_cols], 
                        state_features)

factor_cols <- c("Beds", "Baths", "Cars", "Postcode")
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
# Linear regression
################################################################################
#data <- read.csv('Domain_housing_data_all_LOF.csv')
#data <- read.csv('Domain_housing_data_all_AE3.csv')
#data <- read.csv('Domain_housing_data_all_dist_LOF.csv')
data <- read.csv('Domain_housing_data_all_dist_LOF_fin.csv')

# Remove dummyvars for state and property type (Type)
data_decoded <- data %>% pivot_longer(cols = starts_with("State"),
                                      names_to = "State",
                                      names_prefix = "State",
                                      values_to = "value") %>% filter(value == 1) %>% select(-value)

# Convert state feature to a factor and store in another column
data_decoded$Factored_State <- as.numeric(factor(data_decoded$State))


base_data <- data_decoded[, colnames(data_decoded) %in% 
                            c("Beds", "Baths", "Cars",
                              "Land", "dist_to_cbd", "Median.price", 
                              "Price", "LOF_k.186")]

threshold <- mean(data_decoded$LOF_k.186) * 2
base_data$isAnomaly <- ifelse(data_decoded$LOF_k.186 > threshold, TRUE, FALSE)
base_data <- na.omit(base_data)

# DF with just anomalies
anomalies_data <- base_data[base_data$isAnomaly == TRUE,
                            !colnames(base_data) %in% c("LOF_k.186")]

# Keep only the "usual" values and exclude the LOF values column
processed_data <- base_data[base_data$isAnomaly == FALSE,
                            !colnames(base_data) %in% c("LOF_k.186")]

df_beds_meds <- processed_data %>%
  group_by(Beds) %>%
  summarise(Baths = median(Baths, na.rm = TRUE),
            Cars = median(Cars, na.rm = TRUE),
            Land = median(Land, na.rm = TRUE),
            dist_to_cbd = median(dist_to_cbd, na.rm = TRUE))
df_baths_meds <- processed_data %>%
  group_by(Baths) %>%
  summarise(Beds = median(Beds, na.rm = TRUE),
            Cars = median(Cars, na.rm = TRUE),
            Land = median(Land, na.rm = TRUE),
            dist_to_cbd = median(dist_to_cbd, na.rm = TRUE))
df_cars_meds <- processed_data %>%
  group_by(Cars) %>%
  summarise(Baths = median(Baths, na.rm = TRUE),
            Beds = median(Beds, na.rm = TRUE),
            Land = median(Land, na.rm = TRUE),
            dist_to_cbd = median(dist_to_cbd, na.rm = TRUE))

# Get quantiles 
processed_data <- processed_data %>%
  mutate(Land_bin = ntile(Land, 10))
processed_data <- processed_data %>%
  mutate(dist_to_cbd_bin = ntile(dist_to_cbd, 10))

df_land_meds <- processed_data %>%
  group_by(Land_bin) %>%
  summarise(Beds = median(Beds, na.rm = TRUE),
            Baths = median(Baths, na.rm = TRUE),
            Cars = median(Cars, na.rm = TRUE),
            dist_to_cbd = median(dist_to_cbd, na.rm = TRUE))
df_dist_meds <- processed_data %>%
  group_by(dist_to_cbd_bin) %>%
  summarise(Beds = median(Beds, na.rm = TRUE),
            Baths = median(Baths, na.rm = TRUE),
            Cars = median(Cars, na.rm = TRUE),
            Land = median(Land, na.rm = TRUE))

# x = Beds
bed_land_rel_orig <- lm(Land ~ Beds, df_beds_meds)
bed_baths_rel_orig <- lm(Baths ~ Beds, df_beds_meds)
bed_cars_rel_orig <- lm(Cars ~ Beds, df_beds_meds)
bed_dist_rel_orig <- lm(dist_to_cbd ~ Beds, df_beds_meds)

# x = Baths
bath_land_rel_orig <- lm(Land ~ Baths, df_baths_meds)
bath_beds_rel_orig <- lm(Beds ~ Baths, df_baths_meds)
bath_cars_rel_orig <- lm(Cars ~ Baths, df_baths_meds)
bath_dist_rel_orig <- lm(dist_to_cbd ~ Baths, df_baths_meds)

# x = Cars
car_land_rel_orig <- lm(Land ~ Cars, df_cars_meds)
car_baths_rel_orig <- lm(Baths ~ Cars, df_cars_meds)
car_beds_rel_orig <- lm(Beds ~ Cars, df_cars_meds)
car_dist_rel_orig <- lm(dist_to_cbd ~ Cars, df_cars_meds)

# x = Land_bin
land_beds_rel_orig <- lm(Beds ~ Land_bin, data = df_land_meds)
land_baths_rel_orig <- lm(Baths ~ Land_bin, data = df_land_meds)
land_cars_rel_orig <- lm(Cars ~ Land_bin, data = df_land_meds)
land_dist_rel_orig <- lm(dist_to_cbd ~ Land_bin, data = df_land_meds)

# x = dist_to_cbd_bin
dist_beds_rel_orig <- lm(Beds ~ dist_to_cbd_bin, data = df_dist_meds)
dist_baths_rel_orig <- lm(Baths ~ dist_to_cbd_bin, data = df_dist_meds)
dist_cars_rel_orig <- lm(Cars ~ dist_to_cbd_bin, data = df_dist_meds)
dist_land_rel_orig <- lm(Land ~ dist_to_cbd_bin, data = df_dist_meds)

################################################################################
# Plotting
################################################################################
get_formula <- function(lm_reg_coeffs,
                        x, 
                        y, 
                        rnd_digits = 4) {
  # Function which returns a string representing the linear regression formula
  #
  # Inputs:
  # -------
  #   [+] lm_reg_coeffs: list
  #       List containing the coeefficients of x^0 and x^1
  #   [+] x: string
  #       The name of the independent variable
  #   [+] y: string
  #       The name of the dependent variable
  #   [+] rnd_digits: int
  #       (Optional) Integer representing the number of digits to round to (default is 4)
  #
  # Outputs:
  # --------
  #   String describing the relationship between the 2 variables
  paste(y, "=", round(lm_reg_coeffs$coefficients[2], digits = rnd_digits),
        "*", x, ifelse(lm_reg_coeffs$coefficients[1] < 0, "-", "+"), 
        abs(round(lm_reg_coeffs$coefficients[1], digits = rnd_digits)), sep = " ")
}

plot_rel <- function(df,
                     df_meds,
                     x_name,
                     y_name,
                     lm_reg_coeffs,
                     anom_df = NULL,
                     eqn_pos = NULL,
                     lim_y = FALSE,
                     title = TRUE,
                     rnd_digits = 4) {
  # Function to plot the relationship between 2 variables' medians
  #
  # Inputs:
  # -------
  #   [+] df: dataframe
  #       Dataframe containing data
  #   [+] df_meds: dataframe
  #       Dataframe containing medians of the data
  #   [+] x_name: string
  #       The name of the independent variable
  #   [+] y_name: string
  #       The name of the dependent variable
  #   [+] lm_reg_coeffs: list
  #       List containing the coefficients of x^0 and x^1
  #   [+] anom_df: dataframe
  #       Dataframe containing the anomalous points
  #   [+] eqn_pos: list
  #       (Optional) List containing x and y coordinates of where to display the equation in the plot
  #   [+] lim_y: Boolean
  #       (Optional) Whether to limit the y-axis or not (default is FALSE)
  #   [+] title: Boolean
  #       (Optional) Whether to display a title for the plot or not (default is TRUE)
  #   [+] rnd_digits: int
  #       (Optional) Integer representing the number of digits to round to (default is 4)
  #
  # Outputs:
  # --------
  #   Plot of the relationship between the 2 variables

  p <- ggplot(df, aes(x = !!sym(x_name), y = !!sym(y_name), group = !!sym(x_name), fill = !!sym(x_name))) +
    geom_boxplot(fill = "cornflowerblue") +
    geom_abline(intercept = lm_reg_coeffs$coefficients[1], 
                slope = lm_reg_coeffs$coefficients[2], linewidth = 1.1, 
                colour = "orange", alpha = 0.75) +
    geom_point(alpha = 0.3) +
    #stat_smooth(method = "lm", formula = y ~ poly(x, degree = 5), size = 1) +
    theme(legend.position = "none",
          axis.text=element_text(size=12),
          axis.title = element_text(size=18, face = "bold"),
          plot.title = element_text(size=22, face = "bold"))
  
  if(title) {
    p <- p + labs(title = paste(x_name, "vs", y_name, sep = " "))
  }
  if(lim_y) { 
    p <- p + coord_cartesian(ylim=c(min(df[[y_name]]),as.numeric(quantile(df[[y_name]], probs = 0.99, na.rm = TRUE))))
  }
  if(length(eqn_pos) == 2) {
    p <- p + annotate("text", x = eqn_pos[1],
             y = eqn_pos[2],
             label = get_formula(lm_reg_coeffs, x_name, y_name),
             size = 4.5)
  } else {
    p <- p + annotate("text", x = max(df[[x_name]], na.rm = TRUE)*0.8,
                  y = as.numeric(quantile(df[[y_name]], probs = 0.99, na.rm = TRUE))*0.9,
                  label = get_formula(lm_reg_coeffs, x_name, y_name),
                  size = 4.5)
  }
  if(length(anom_df)) {
    p <- p + geom_point(data = anom_df, aes(x = !!sym(x_name), y = !!sym(y_name)), 
                        shape = 22, colour = "red", fill = "red", size = 2, alpha = 0.8)
    }
  
  # Ensure medians are place on top of all other elements
  p <- p +geom_point(data = df_meds, aes(x = !!sym(x_name), y = !!sym(y_name)),
             colour = "green", fill = "green", shape=23, size=2.25)
  
  p
}

###################################### Beds ####################################
beds_land <- plot_rel(processed_data, df_beds_meds, "Beds", "Land", bed_land_rel_orig, anomalies_data, lim_y = TRUE, title = FALSE)
beds_baths <- plot_rel(processed_data, df_beds_meds, "Beds", "Baths", bed_baths_rel_orig, anomalies_data, title = FALSE, eqn_pos = c(-1.5, 7))
beds_cars <- plot_rel(processed_data, df_beds_meds, "Beds", "Cars", bed_cars_rel_orig, anomalies_data, title = FALSE, eqn_pos = c(6, 7.5))
beds_dist <- plot_rel(processed_data, df_beds_meds, "Beds", "dist_to_cbd", bed_dist_rel_orig, anomalies_data, title = FALSE, eqn_pos = c(6, 4))

ggarrange(beds_baths, beds_cars,
          beds_land, beds_dist,
          nrow = 2, ncol = 2)

##################################### Baths ####################################
baths_land <- plot_rel(processed_data, df_baths_meds, "Baths", "Land", bath_land_rel_orig, anomalies_data, lim_y = TRUE, title = FALSE)
baths_beds <- plot_rel(processed_data, df_baths_meds, "Baths", "Beds", bath_beds_rel_orig, anomalies_data, title = FALSE, eqn_pos = c(6, -0.5))
baths_cars <- plot_rel(processed_data, df_baths_meds, "Baths", "Cars", bath_cars_rel_orig, anomalies_data, title = FALSE, eqn_pos = c(6, 7))
baths_dist <- plot_rel(processed_data, df_baths_meds, "Baths", "dist_to_cbd", bath_dist_rel_orig, anomalies_data, title = FALSE, eqn_pos = c(6, 4))

ggarrange(baths_beds, baths_cars,
          baths_land, baths_dist,
          nrow = 2, ncol = 2)

##################################### Cars #####################################
cars_land <- plot_rel(processed_data, df_cars_meds, "Cars", "Land", car_land_rel_orig, anomalies_data, lim_y = TRUE, title = FALSE, eqn_pos = c(6.8, 0.82))
cars_beds <- plot_rel(processed_data, df_cars_meds, "Cars", "Beds", car_beds_rel_orig, anomalies_data, title = FALSE, eqn_pos = c(6, 6))
cars_baths <- plot_rel(processed_data, df_cars_meds, "Cars", "Baths", car_baths_rel_orig, anomalies_data, title = FALSE, eqn_pos = c(6.5, 5))
cars_dist <- plot_rel(processed_data, df_cars_meds, "Cars", "dist_to_cbd", car_dist_rel_orig, anomalies_data, lim_y = TRUE, title = FALSE, eqn_pos = c(7, 2))

ggarrange(cars_beds, cars_baths,
          cars_land, cars_dist,
          nrow = 2, ncol = 2)

##################################### Land #####################################
plot_rel(processed_data, df_land_meds, "Land_bin", "Baths", land_baths_rel_orig, lim_y = TRUE, title = FALSE)
plot_rel(processed_data, df_land_meds, "Land_bin", "Beds", land_beds_rel_orig, title = FALSE, eqn_pos = c(4, 0))
plot_rel(processed_data, df_land_meds, "Land_bin", "Cars", land_cars_rel_orig, title = FALSE, eqn_pos = c(6, 7.5))
plot_rel(processed_data, df_land_meds, "Land_bin", "dist_to_cbd", land_dist_rel_orig, title = FALSE, eqn_pos = c(6, 4))

################################## dist_to_cbd #################################
plot_rel(processed_data, df_dist_meds, "dist_to_cbd_bin", "Baths", dist_baths_rel_orig, lim_y = TRUE, title = FALSE)
plot_rel(processed_data, df_dist_meds, "dist_to_cbd_bin", "Beds", dist_beds_rel_orig, title = FALSE, eqn_pos = c(5, 6))
plot_rel(processed_data, df_dist_meds, "dist_to_cbd_bin", "Cars", dist_cars_rel_orig, title = FALSE, eqn_pos = c(6, 7.5))
plot_rel(processed_data, df_dist_meds, "dist_to_cbd_bin", "Land", dist_land_rel_orig, title = FALSE, eqn_pos = c(6, 4))

