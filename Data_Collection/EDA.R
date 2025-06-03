setwd("Path/to/your/folder")

################################################################################
# import packages and data
################################################################################
library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)
library(reshape)
library(scales)
library(ggpubr)

data = read.csv('Domain_Housing_Data_250511 - Copy.csv')
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
            size = 5) +
  theme(axis.text=element_text(size=16),
        axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold"))

################################################################################
# Create box plots
################################################################################
melt_data <- melt(data[c("Beds", "Baths", "Cars", "Land", "Price")])
ggplot(melt_data, mapping = aes(factor(variable), value)) +
  geom_boxplot(aes(fill=variable)) + facet_wrap(~variable, scale='free') +
  labs(title = "Box plots for each numeric variable") +
  theme(plot.title = element_text(size=22, face = "bold"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())

################################################################################
# Create corrplot 
################################################################################
data <- na.omit(data)
data["SaleType"] <- lapply(data["SaleType"], 
                           function(x) replace(x, x == "", "AUCTION"))
cat_cols <- c("Suburb", "State", "Type", "Postcode", "SaleType")
cat_features <- data[ , cat_cols]
cat_features <- cat_features %>% mutate_all(as.factor) %>%
  mutate_all(as.numeric)

df <- data
df[ , cat_cols] <- cat_features

cor_data_cont <- cor(df[ , !colnames(df) %in% c("Date", 
                                                "Address",
                                                "Suburb",
                                                "State",
                                                "Postcode",
                                                "SaleType",
                                                "Type")])
corrplot(cor_data_cont, method='color',  addCoef.col = 'black',
         col = colorRampPalette(c('#8FFF99', '#FF9999', 'red'))(100),
         cl.lim = c(0,1), is.corr = FALSE,
         tl.col = 'Black', tl.cex = 1.5)

################################################################################
# Create histogram for price and land
################################################################################
colnames(data)
ggplot(data, aes(x=Price)) + 
  geom_histogram(binwidth = 100000,  color="#e9ecef",fill="#FF0000", alpha=1) +
  labs(title = "Price distribution") +
  theme(axis.text=element_text(size=12),
        axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold")) +
  scale_x_continuous(label=comma, limits=c(min(df$Price), max(df$Price)))

ggplot(data, aes(x=Land)) + 
  geom_histogram(binwidth = 1000, color="#e9ecef",fill="#FF0000", alpha=1) +
  labs(title = "Land area distribution") +
  theme(axis.text=element_text(size=12),
        axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold"))


################################################################################
# Creating scatter plots to show relation
################################################################################
bed_price_gg <- ggplot(df, aes(x=Beds, y=Price)) + 
  geom_point(color = 'orange', alpha=0.6) +
  geom_smooth(method=lm, color = 'red') +
  labs(title = "Num beds vs property price") +
  scale_y_continuous(label=comma, limits=c(min(df$Price), max(df$Price)))

bath_price_gg <- ggplot(df, aes(x=Baths, y=Price)) + 
  geom_point(color = 'orange', alpha=0.6) +
  geom_smooth(method=lm, color = 'red') +
  labs(title = "Num baths vs property price") +
  scale_y_continuous(label=comma, limits=c(min(df$Price), max(df$Price)))

land_price_gg <- ggplot(df, aes(x=Land, y=Price)) + 
  geom_point(color = 'orange', alpha=0.6) +
  geom_smooth(method=lm, color = 'red') +
  labs(title = "Land area vs property price") +
  scale_y_continuous(label=comma, limits=c(min(df$Price), max(df$Price)))

car_price_gg <- ggplot(df, aes(x=Cars, y=Price)) + 
  geom_point(color = 'orange', alpha=0.6) +
  geom_smooth(method=lm, color = 'red') +
  labs(title = "Car spots vs property price") +
  scale_y_continuous(label=comma, limits=c(min(df$Price), max(df$Price)))

saletype_price_gg <- ggplot(data, aes(x=SaleType, y=Price, color=SaleType)) + 
  geom_point(alpha=0.6) +
  labs(title = "Sale type vs property price") +
  scale_y_continuous(label=comma, limits=c(min(df$Price), max(df$Price))) +
  scale_x_discrete(labels = c('Auction', 'Prior to Auction', 'Private Sale')) +
  theme(axis.text.x = element_text(angle = 45, hjust =  1), 
        legend.position = "none")

state_price_gg <- ggplot(data, aes(x=State, y=Price, color=State)) + 
  geom_point(alpha=0.6) +
  labs(title = "State vs property price") +
  scale_y_continuous(label=comma, limits=c(min(df$Price), max(df$Price))) +
  theme(axis.text.x = element_text(angle = 45, hjust =  1),
        legend.position = "none")

ggarrange(bed_price_gg, bath_price_gg, 
          car_price_gg, land_price_gg,
          state_price_gg, saletype_price_gg, 
          ncol = 2, nrow = 3
          )

type_price_gg <- ggplot(data, aes(x=Price, y=Type, color=Type)) + 
  geom_point(alpha=0.6) +
  labs(title = "Property type vs property price") +
  scale_x_continuous(label=comma, limits=c(min(df$Price), max(df$Price))) +
  scale_y_discrete(labels = c('Apartment / Unit', 'Block of Units', 'Duplex', 
                              'House', 'House + Land package', 'Retirement',
                              'Semi-detached', 'Terrace', 'Townhouse', 'Land',
                              'Villa')) +
  theme(axis.text=element_text(size=12),
        axis.title = element_text(size=18, face = "bold"),
        plot.title = element_text(size=22, face = "bold"),
        legend.position = "none")
type_price_gg
