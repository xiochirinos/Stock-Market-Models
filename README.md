# Stock Market Models

***

## Authors

Rene Villarreal <br>
Xiomara Chirinos <br>
Mariah Bastos <br>

***

## Overview

The purpose of this project was to create six models that consumed FAANG data and would classify a stock as going up 5% in the next 30 minutes, going down 2%, or nothing. The six models used were: 

MARS <br>
Random Forest <br>
Support Vector Machines <br>
Neural Networks <br>
AutoML <br>
Time Series <br>

The models were all fed historical data by minute from October 1, 2020 to December 31, 2020. The test/validation period was January 1, 2021 to February 26, 2021. This project gives us exposure applying machine learning models learned in the classroom to real-world situations like the stock market. 
Approach

The six models were built and measured on the Mean Squared Error (MSE) and Accuracy for each of the stocks. The accuracies were then tested for significant differences to determine which model was the best for predicting changes in FAANG stock prices. 

***

## Instructions

In order to run the codes in our folders, you will need the following libraries: 

tidyverse: with this library we used mostly dplyr for cleaning up our data. 
jsonlite: this is mostly used to read the JSON messsages whenever we execute a trade and to access our budget. 
riingo: we used this package to pull data from Tiingo using our API key. 
zoo: we included this package in the library set to use the lag function. 
lubridate: manipulate dates and timestamps. 
hms: additional manipulation of dates and timestamps. 
forecast: time series. 
kernlab: support vector machines. 
earth: MARS. 
randomForest: random forest. 
h2o: autoML. 
neuralnet: neural networks. 
caret: confusion matrices. 

The R scripts can be taken directly from our 'code' folder and run in R.
