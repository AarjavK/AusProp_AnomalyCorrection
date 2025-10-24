# AusProp_AnomalyCorrection
For masters thesis: Anomaly correction for Australian property market

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

Data Collection and Exploratory Data Analysis (EDA)

Feature Selection

Anomaly Detection

Model Fitting and comparisons

Scaling Directions
