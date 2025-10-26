# AusProp_AnomalyCorrection
For masters thesis: Feature Selection and Scaling Directions for Robust Property Price Prediction

# Introduction

## Data description
The data that was collected from webscraping and the Australian Bureau of Statistics contains the following columns:
<table>
  <tr>
    <th>Order</th>
    <th>Column name</th>
    <th>Description</th>
  </tr>
  <tr>
    <td>1</td>
    <td>Date</td>
    <td>Sale date</td>
  </tr>
  </tr>
  <tr>
    <td>2</td>
    <td>Address (excluded for anonimity) </td>
    <td>Street address of property sold</td>
  </tr>
  </tr>
  <tr>
    <td>3</td>
    <td>Suburb</td>
    <td>Suburb that the property was in</td>
  </tr>
  <tr>
    <td>4</td>
    <td>State</td>
    <td>The state or territory (VIC, NSW, QLD, NT, ACT, TAS, WA, SA)</td>
  </tr>
  <tr>
    <td>5</td>
    <td>Post code</td>
    <td>The post code or ZIP code</td>
  </tr>
  </tr>
  <tr>
    <td>6</td>
    <td>Beds</td>
    <td>The number of bedrooms</td>
  </tr>
  </tr>
  <tr>
    <td>7</td>
    <td>Baths</td>
    <td>The number of bathrooms</td>
  </tr>
  <tr>
    <td>8</td>
    <td>Cars</td>
    <td>The number of car spaces (either garages or space in the drive way</td>
  </tr>
  </tr>
  <tr>
    <td>9</td>
    <td>Land</td>
    <td>The land area in m^2</td>
  </tr>
  </tr>
  <tr>
    <td>10</td>
    <td>Sale Type</td>
    <td>The way the property was sold (Auction, prior to auction, treaty, private sale, etc.</td>
  </tr>
  <tr>
    <td>11</td>
    <td>Sale Type</td>
    <td>The way the property was sold (Auction, prior to auction, treaty, private sale, etc.</td>
  </tr>
  <tr>
    <td>12</td>
    <td>Median price</td>
    <td>The median price of the property (either house or apartment/unit/tenament) in the suburb (either CBD or not)</td>
  </tr>
  <tr>
    <td>13</td>
    <td>Price</td>
    <td>Sale price</td>
  </tr>
</table>

# Project Structure
The project is split into multiple sections:
<ol>
  <li>Data Collection and Exploratory Data Analysis (EDA)</li>
  <li>Feature Selection</li>
  <li>Model fitting and comparison</li>
  <li>Scaling Directions</li>
</ol>


## Data Collection and Exploratory Data Analysis (EDA)
The purpose of this section is to collect data from the Domain website and then build an understanding of the data. The code for this section can be found in <a href="https://github.com/AarjavK/AusProp_AnomalyCorrection/tree/main/Data_Collection">Data_Collection</a>. The <a href="https://github.com/AarjavK/AusProp_AnomalyCorrection/blob/main/Data_Collection/Webscraping.py">Webscraping.py</a> code uses Playwright to access the Domain website and download individual web pages. Domain.com.au has a limit of 50 pages of sales history for each search - this amount to approximately 8,000 property sales at a time. The webscraping code was at regular intervals to create large dataset of unique properties.

The calculation of <a href="https://github.com/AarjavK/AusProp_AnomalyCorrection/blob/main/Data_Collection/dist_to_CBD.py">distance to the closest CBD / capital city code</a> uses the geopy Python package to get the latitude and longitude of each address then calculate the distance to the centre of each capital city. Given that Western Australia is very large geographically, both Perth (capital) and Broome were chosen to be the major cities. The final distance is the minimum distance from the property to Perth or Broome.

Median house prices were obtained from the <a href="https://www.abs.gov.au/statistics/economy/price-indexes-and-inflation/total-value-dwellings/latest-release">Australian Bureau of Statistics</a> and then assigned to each property through Excel. Given that the ABS only provides data for established houses and attached dwellings (any dwelling which shares a common structural component such as wall, ceiling or floor with at least one other dwelling), the median prices assigned using Excel formulas such that villas, houses, vacant land, and duplexes are considered established houses and all other property types are considered to be attached dwellings.

<a href="https://github.com/AarjavK/AusProp_AnomalyCorrection/blob/main/Data_Collection/EDA.R">Exploratory data analysis</a> looks at counts of null values, distributions of data, correlations, as well as some interesting statistics in the top sales values using the ggplot2 and dplyr R packages.

## Feature Selection
Genetic Algorithms are used for feature selection using the root mean square error of each training model to identify the optimal set of features for the specific model. The <a href="https://github.com/AarjavK/AusProp_AnomalyCorrection/blob/main/FeatureSelection/GA_FeatureSelection.R">R code for feature selection</a> uses the caret and GA packages for training models. An additional benefit of using the RMSE of the various models is that the GA also helps with comparing the different models as well.

## Anomaly Detection
There are 3 anomaly detection techniques compared in this project - k-nearest neighbours, Local Outlier Factors, and Autoencoders. The clustering models (KNN and LOF) are tuned using the number of anomalies found as a metric to select the distance threshold and the number of k-nearest neighbours to create a cluster. This particular dataset which was found to be significantly right-skewed and thus changing the distance threshold from the 80th to 99th percentile had no impact on the number of anomalies found. The only parameter that was eventually required is the neighbour count. For autoencoders, a grid search was used to identify the optimal hyperparameters - number of hidden layers, hidden nodes, and activation function.

## Model Fitting and comparisons
The models created at each of the previous steps were compared to find the optimal model for the specific problem of predicting prices and finding anomalous properties. A baseline model is created for linear regression, linear regression with consideration of interactions between features, gradient boosting model, and random forest. These baseline models are compared to each other, and to the models trained on the reduced feature set after GA. The models are retrained using the reduced feature set and after removing properties identified to be anomalous from the training set to find the optimal model and anomaly detection technique from this selection pool.

## Scaling Directions
After selecting the model and anomaly detection algorithms, the anomalous properties are excluded. The median values are calculated for each of the features and then a regression line is obtained to determine the median regressions to find out how each feature scales based on the others. The median is used so that the linear regression is not impacted by fluctuations in the values. These scaling directions allow us to deermin
