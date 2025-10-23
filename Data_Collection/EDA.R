setwd("Path/to/your/folder")

################################################################################
# Scrape domain.com.au for housing data
################################################################################
library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)
library(reshape)
library(scales)
library(ggpubr)
library(ggridges)
# For spatial data
library(viridis)
library(sf)

#data = read.csv('Domain_housing_data_all.csv')
data = read.csv('Domain_housing_data_all_coordinates.csv')
colnames(data)[1] <- "Date"
summary(data)

################################################################################
# Get the NA counts
################################################################################
na_counts <- colSums(is.na(data))
na_df <- data.frame(Column = names(na_counts), NA_Count = na_counts)

ggplot(na_df, aes(x = Column, y = NA_Count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Count of NA Values per column", x = "Column",
       y = "Number of NAs") +
  geom_text(data = subset(na_df, NA_Count > 0),
            aes(label = NA_Count), 
            vjust = -0.5,  
            size = 4) +
  theme(axis.text=element_text(size=13),
        axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust =  1))

################################################################################
# Create box plots
################################################################################
melt_data <- melt(data[c("Beds", "Baths", "Cars", "Land", "dist_to_cbd", "Price")])
ggplot(melt_data, mapping = aes(factor(variable), value)) +
  geom_boxplot(aes(fill=variable)) + facet_wrap(~variable, scale='free') +
  labs(title = "Box plots for each numeric variable") +
  theme(plot.title = element_text(size=22, face = "bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")

################################################################################
# Create corrplot 
################################################################################
data["SaleType"] <- lapply(data["SaleType"], 
                           function(x) replace(x, x == "", "AUCTION"))
data <- na.omit(data)
cat_cols <- c("Suburb", "State", "Type", "Postcode", "SaleType")
cat_features <- data[ , cat_cols]
cat_features <- cat_features %>% mutate_all(as.factor) %>%
  mutate_all(as.numeric)

scaling_cols <- c("Beds", "Baths", "Cars", "Land", "Median.price", "Price", "dist_to_cbd")

df <- data
df[scaling_cols] <- scale(df[scaling_cols])

df["Beds*Baths"] <- df$Beds * df$Baths
df["Beds*Land"] <- df$Beds * df$Land
df["Beds*dist_to_cbd"] <- df$Beds * df$dist_to_cbd
df["Baths*Land"] <- df$Baths * df$Land
df["Baths*dist_to_cbd"] <- df$Baths * df$dist_to_cbd
df["Cars*Baths"] <- df$Cars * df$Baths
df["Cars*Beds"] <- df$Cars * df$Beds
df["Cars*Land"] <- df$Cars * df$Land
df["Cars*dist_to_cbd"] <- df$Cars * df$dist_to_cbd
df["Cars*Beds*Baths"] <- df$Cars * df$Beds * df$Baths
df["Cars*Beds*dist_to_cbd"] <- df$Cars * df$Beds * df$dist_to_cbd
df["Cars*Baths*dist_to_cbd"] <- df$Cars * df$Baths * df$dist_to_cbd
df["Cars*Land*Beds*Baths"] <- df$Cars * df$Land * df$Beds * df$Baths
df["Cars*Land*Beds*dist_to_cbd"] <- df$Cars * df$Land * df$Beds * df$dist_to_cbd
df["Cars*Land*Baths*dist_to_cbd"] <- df$Cars * df$Land * df$Baths * df$dist_to_cbd
df["Cars*Beds*Baths*dist_to_cbd"] <- df$Cars * df$Land * df$Beds * df$dist_to_cbd

df[ , cat_cols] <- cat_features

cor_data_cont <- cor(df[ , !colnames(df) %in% c("Date", 
                                                "Address",
                                                "Suburb",
                                                "State",
                                                "Postcode",
                                                "SaleType",
                                                "Type",
                                                "longitude",
                                                "latitude")])

corrplot(cor_data_cont, method='color',  addCoef.col = 'black',
         col = colorRampPalette(c('#00FFFF','#F0FF5A', '#FF0700'))(100),
         cl.lim = c(-0.1,1), is.corr = FALSE,
         tl.col = 'Black', tl.cex = 1)

################################################################################
# Create histogram for price and land
################################################################################
colnames(data)
# truncating max so that distribution is more visible
ggplot(data, aes(x=Price)) + 
  geom_histogram(binwidth = 100000,  color="#e9ecef",fill="#FF0000", alpha=1) +
  labs(title = "Price distribution") +
  theme(axis.text=element_text(size=12),
        axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold")) +
  scale_x_continuous(label=comma, limits=c(min(data$Price),
                                           as.numeric(quantile(data$Price, probs = 0.997))
                                           )
                     )

ggplot(data, aes(x=Land)) + 
  geom_histogram(binwidth = 50, color="#e9ecef",fill="#FF0000", alpha=1) +
  labs(title = "Land area distribution", x = "Land area") +
  theme(axis.text=element_text(size=12),
        axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold")) +
  scale_x_continuous(label=comma, limits=c(min(data$Land),
                                           as.numeric(quantile(data$Land, probs = 0.95))
                                          )
                     )

ggplot(data, aes(x=dist_to_cbd)) + 
  geom_histogram(binwidth = 20, color="#e9ecef",fill="#FF0000", alpha=1) +
  labs(title = "Distance distribution", x = "Distance to CBD (km)") +
  theme(axis.text=element_text(size=12),
        axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold"))

################################################################################
# Creating scatter plots to show relation
################################################################################
df <- data
bed_price_gg <- ggplot(df, aes(x=Beds, y=Price)) + 
  geom_point(color = 'blue', alpha=0.4) +
  #geom_smooth(method=lm, color = 'red') +
  labs(title = "Num beds vs property price") +
  scale_y_continuous(label=comma, limits=c(min(df$Price), max(df$Price)))

bath_price_gg <- ggplot(df, aes(x=Baths, y=Price)) + 
  geom_point(color = 'blue', alpha=0.4) +
  #geom_smooth(method=lm, color = 'red') +
  labs(title = "Num baths vs property price") +
  scale_y_continuous(label=comma, limits=c(min(df$Price), max(df$Price)))

land_price_gg <- ggplot(df, aes(x=Land, y=Price)) + 
  geom_point(color = 'blue', alpha=0.4) +
  #geom_smooth(method=lm, color = 'red') +
  labs(title = "Land area vs property price") +
  scale_y_continuous(label=comma, limits=c(min(df$Price), max(df$Price))) +
  scale_x_continuous(label=comma, limits=c(min(df$Land), max(df$Land)))

car_price_gg <- ggplot(df, aes(x=Cars, y=Price)) + 
  geom_point(color = 'blue', alpha=0.4) +
  #geom_smooth(method=lm, color = 'red') +
  labs(title = "Car spots vs property price") +
  scale_y_continuous(label=comma, limits=c(min(df$Price), max(df$Price)))

dist_price_gg <- ggplot(df, aes(x=dist_to_cbd, y=Price)) + 
  geom_point(color = 'blue', alpha=0.4) +
  #geom_smooth(method=lm, color = 'red') +
  labs(title = "Distance to CBD vs property price") +
  scale_y_continuous(label=comma, limits=c(min(df$Price), max(df$Price)))

saletype_price_gg <- ggplot(data, aes(x=SaleType, y=Price, color=SaleType)) + 
  geom_point(alpha=0.4) +
  labs(title = "Sale type vs property price") +
  scale_y_continuous(label=comma, limits=c(min(df$Price), max(df$Price))) +
  scale_x_discrete(labels = c('Auction', 'Prior to Auction', 'Private Sale', 'Unkown')) +
  theme(axis.text.x = element_text(angle = 45, hjust =  1), 
        legend.position = "none")

state_price_gg <- ggplot(data, aes(x=State, y=Price, color=State)) + 
  geom_point(alpha=0.4) +
  labs(title = "State vs property price") +
  scale_y_continuous(label=comma, limits=c(min(df$Price), max(df$Price))) +
  theme(axis.text.x = element_text(angle = 45, hjust =  1),
        legend.position = "none")

ggarrange(bed_price_gg, bath_price_gg, 
          car_price_gg, land_price_gg,
          state_price_gg, saletype_price_gg, 
          dist_price_gg,
          ncol = 2, nrow = 4
          )

type_price_gg <- ggplot(data, aes(x=Price, y=Type, color=Type)) + 
  geom_point(alpha=0.6) +
  labs(title = "Property type vs property price") +
  scale_x_continuous(label=comma, limits=c(min(data$Price), max(data$Price))) +
  theme(axis.text=element_text(size=12),
        axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold"),
        legend.position = "none")

# Ridgeline
ggplot(data, aes(x=Price, y=Type, fill=Type)) +
  geom_point(alpha=0.6) +
  geom_density_ridges(alpha = 0.8) + 
  theme_ridges() + 
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  scale_x_continuous(label=comma, limits=c(min(data$Price), 
                                           as.numeric(quantile(data$Price, probs = 0.997)))) +
  labs(title = "Property type vs property price", legend.position = "none")


# Create heatmap overlayed on map of Australia
sales_by_region <- data %>%
  group_by(State, Postcode) %>%
  summarise(Sales = n(), .groups = "drop")

# Load Australian postcode shapefile
aus_postcodes <- st_read("POA_2021_AUST_GDA2020_SHP/POA_2021_AUST_GDA2020.shp")  
aus_postcodes$POA_CODE21 <- as.numeric(aus_postcodes$POA_CODE21)

aus_states <- st_read("STE_2021_AUST_SHP_GDA2020/STE_2021_AUST_GDA2020.shp")

# Join with sales data
aus_map <- aus_postcodes %>%
  left_join(sales_by_region, by = c("POA_CODE21" = "Postcode"))
#aus_map$Sales[is.na(aus_map$Sales)] <- 0

ggplot(aus_map) +
  geom_sf(aes(fill = Sales), color = NA) +
  geom_sf(data = aus_states, fill = NA, color = "black", size = 0.6) +
  scale_fill_viridis(option = "plasma", 
                     trans = "log", 
                     na.value = "lightgrey", 
                     labels = label_number(accuracy = 1),
                     breaks = c(1, 10, 50, 100, max(aus_map$Sales, na.rm = TRUE))) +
  labs(fill = "Number of Sales") +
  theme_void()
