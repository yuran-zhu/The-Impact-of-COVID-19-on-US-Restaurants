# The-Impact-of-COVID-19-on-US-Restaurants

Predict US restaurant revenues and dine-in percentage, and provide data support for offering decision-makers insights on developing business.

R Shiny App Link: https://yuran-zhu.shinyapps.io/covid-restaurant-impact/

## Exploratory Data Analysis

Analyze and visualize data of restaurant revenues, dine-in percentage, community mobility, and COVID-19 cases. Explore the correlations among data, which helps in building the predcitive model.

- **Exploratory Data Analysis.Rmd**: Records codes to perform EDA.
- **EDA Data**: Stores all data sets explored.

## Predictive Model

Predict restaurant total revenues and dine-in percentage in the next 3, 6, and 18 months. According to the possible COIVD-19 and economic situation, the project assumes 7 different scenarios in total. In order to connect with economy development, we add economic indicator into the model.

- 3 months: Bad COVID & Bad Economy
- 6 months: Bad COVID & Bad Economy, Good COVID & Bad Economy, Good COVID & Good Economy
- 18 months: Bad COVID & Bad Economy, Good COVID & Bad Economy, Good COVID & Good Economy


Considering the limited data points and the fact that we haven't seen the full cycle of the pandemic, we thought that the neural network, time-series mode is not aprropriate for the project after several attemps. Eventually, we decided to use PCA to extract important features and derive unrelavant components, and then fit a linear regression model based on it.

- **Prediction_Model.Rmd**: Records codes to perfrom PCA, run regression models, make predictions, and data visualization.
- **PCA_Data**: Stores all data sets used for PCA for 7 scenarios, related to total revenues and dine-in percentage.
- **Prediction_Results.Rmd**: Stores final data sets, integrating prediction results for 7 scenarios together, which can be directly used for visualization in Shiny App.

## R Shiny App

Using R Shiny packages, we build an interactive web app for integrating visuals and predictions. Users can use the dropdown menu to select and compare future trends in different scenarios. All files in stored in the folder *covid-restaurant-impact*.

- **app.R**: R script for designing and initiating the Shiny App.

## Data Source

- Restaurant Revenue & Dining Trend (From: Toast Platform & Rally for Restaurant): https://rallyforrestaurants.com/impact-COVID-19-restaurant-insights.html
- Covid-19 Data (From: Johns Hopkins University): https://github.com/CSSEGISandData/COVID-19
- Covid-19 Date Prediction (From: Massachusetts Institute of Technology, Operations Research Center): https://www.covidanalytics.io/projections
- Community Mobility Trend (From: Google): https://www.google.com/covid19/mobility/
- Weekly Economic Index (WEI) (From: Federal Reserve Bnak of New York): https://www.newyorkfed.org/research/policy/weekly-economic-index#/interactive
- Urbanization Rate in US States: https://en.wikipedia.org/wiki/Urbanization_in_the_United_States
                                  
