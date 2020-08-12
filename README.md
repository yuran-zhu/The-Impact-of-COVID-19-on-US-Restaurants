# The-Impact-of-COVID-19-on-US-Restaurants

Predict US restaurant revenues and dine-in percentage, and provide data support for offering decision-makers insights on developing business.

R Shiny App Link: https://yuran-zhu.shinyapps.io/covid-restaurant-impact/

## Exploratory Data Analysis

Analyze and visualize data of restaurant revenues, dine-in percentage, community mobility, and COVID-19 cases. Explore the correlations among data, which helps in building the predcitive model.

## Predictive Model

Predict restaurant total revenues and dine-in percentage in the next 3, 6, and 18 months. According to the possible COIVD-19 and economic situation, the project assumes 7 different scenarios in total. In order to connect with economy development, we add economic indicator into the model.

Considering the limited data points and the fact that we haven't seen the full cycle of the pandemic, we thought that the neural network, time-series mode is not aprropriate for the project after several attemps. Eventually, we decided to use PCA to extract important features and derive unrelavant components, and then fit a linear regression model based on it.

## R Shiny App

Using R Shiny packages, we build an interactive web app for integrating visuals and predictions. Users can use the dropdown menu to select and compare future trends in different scenarios.
