library(tidyverse)
library(riingo)
library(forecast)
library(earth)
library(randomForest)
library(kernlab)
library(h2o)
library(neuralnet)
library(Metrics)
library(caret)
library(hms)
library(lubridate)

riingo_set_token("436c2f794016aa2e308bc04cc84b49bff63ac9dd")

Oct <- riingo_iex_prices(c("FB", "AAPL", "AMZN", "NFLX", "GOOGL"), start_date = "2020-10-01" , end_date = "2020-10-31", resample_frequency = "1min")
Nov <- riingo_iex_prices(c("FB", "AAPL", "AMZN", "NFLX", "GOOGL"), start_date = "2020-11-01" , end_date = "2020-11-30", resample_frequency = "1min")
Dec <- riingo_iex_prices(c("FB", "AAPL", "AMZN", "NFLX", "GOOGL"), start_date = "2020-12-01" , end_date = "2020-12-31", resample_frequency = "1min")
Jan <- riingo_iex_prices(c("FB", "AAPL", "AMZN", "NFLX", "GOOGL"), start_date = "2021-01-01" , end_date = "2021-01-31", resample_frequency = "1min")
Feb <- riingo_iex_prices(c("FB", "AAPL", "AMZN", "NFLX", "GOOGL"), start_date = "2021-02-01" , end_date = "2021-02-26", resample_frequency = "1min")

FAANG_Data <- rbind(Oct, Nov, Dec, Jan, Feb)
FAANG_Data$Date <- as.Date(FAANG_Data$date)
FAANG_Data$Time <- format(FAANG_Data$date, "%H:%M:%S")

FAANG_Data <- FAANG_Data %>% 
  arrange(ticker, date) %>%
  group_by(ticker) %>%
  mutate(lag_30 = lag(close, 30),
         var_30_pct = (((lag_30)/(close)) - 1)*100,
         classifier_actual = ifelse(var_30_pct <= (-2), "DOWN_2_PCT", ifelse(var_30_pct >= (5), "UP_5_PCT","NOTHING")))

FAANG_Data$classifier_actual <- factor(FAANG_Data$classifier_actual, levels=c("DOWN_2_PCT", "NOTHING", "UP_5_PCT"))

data_split <- function(df)
{
  
  set.seed(123)
  train <- df %>%
    filter(Date < "2021-01-01")
  
  test <- df %>%
    filter(Date >= "2021-01-01")
  
  list <- list(train = train, test = test)
  
  return(list)
}

stocks <- c("FB", "AAPL", "AMZN", "NFLX", "GOOGL")

Metrics_MARS<- function(df)
{
  MSE <- list()
  Accuracy <- list()
  
  for(i in stocks)
  {
    
    x <- df %>%
      filter(ticker == i, !is.na(lag_30))
    
    marsModel <- earth(var_30_pct ~ date + open + volume + close, data = data_split(x)$train)
    marsPrediction <- as.data.frame(predict(marsModel, data_split(x)$test))
    marsPredictionClass <- marsPrediction %>%
      mutate(classifier_prediction = (ifelse(marsPrediction$var_30_pct <= (-2), "DOWN_2_PCT", ifelse(marsPrediction$var_30_pct >= (5), "UP_5_PCT","NOTHING"))))
    
    marsPredictionClass$classifier_prediction <- factor(marsPredictionClass$classifier_prediction, levels=c("DOWN_2_PCT", "NOTHING", "UP_5_PCT"))
    
    Accuracy[[i]] <- confusionMatrix((data_split(x)$test)$classifier_actual, marsPredictionClass$classifier_prediction)[["overall"]][["Accuracy"]]
    
    Accuracy_Final <- as.data.frame(unique(do.call("rbind", Accuracy)))
    Accuracy_Final <- cbind(Row.Names = rownames(Accuracy_Final), Accuracy_Final)
    rownames(Accuracy_Final) <- NULL
    colnames(Accuracy_Final)[1] <- "Stock"
    colnames(Accuracy_Final)[2] <- "Accuracy"
    
    
    MSE[[i]] <- mse((data_split(x)$test)$var_30_pct, marsPrediction$var_30_pct)
    MSE_Final <- as.data.frame(unique(do.call("rbind", MSE)))
    
    
    MSE_Final <- cbind(Row.Names = rownames(MSE_Final), MSE_Final)
    rownames(MSE_Final) <- NULL
    colnames(MSE_Final)[1] <- "Stock"
    colnames(MSE_Final)[2] <- "MSE"
    
    Metrics_Final <- unique(cbind(Model = "MARS", MSE_Final, Accuracy = Accuracy_Final$Accuracy))
  }
  
  return(Metrics_Final)
  
}


Metrics_RF<- function(df)
{
  MSE <- list()
  Accuracy <- list()
  
  for(i in stocks)
  {
    
    x <- df %>%
      filter(ticker == i, !is.na(lag_30))
    
    RFModel <- randomForest(var_30_pct ~ ., data = data_split(x)$train, mtry = 3, ntree = 64)
    RFPrediction <- as.data.frame(predict(RFModel, data_split(x)$test, type = "class")) %>%
      mutate(var_30_pct = `predict(RFModel, data_split(x)$test, type = "class")`)
    RFPredictionClass <- RFPrediction %>%
      mutate(classifier_prediction = (ifelse(RFPrediction$var_30_pct <= (-2), "DOWN_2_PCT", ifelse(RFPrediction$var_30_pct >= (5), "UP_5_PCT","NOTHING"))))
    
    RFPredictionClass$classifier_prediction <- factor(RFPredictionClass$classifier_prediction, levels=c("DOWN_2_PCT", "NOTHING", "UP_5_PCT"))
    
    Accuracy[[i]] <- confusionMatrix((data_split(x)$test)$classifier_actual, RFPredictionClass$classifier_prediction)[["overall"]][["Accuracy"]]
    
    Accuracy_Final <- as.data.frame(unique(do.call("rbind", Accuracy)))
    Accuracy_Final <- cbind(Row.Names = rownames(Accuracy_Final), Accuracy_Final)
    rownames(Accuracy_Final) <- NULL
    colnames(Accuracy_Final)[1] <- "Stock"
    colnames(Accuracy_Final)[2] <- "Accuracy"
    
    
    MSE[[i]] <- mse((data_split(x)$test)$var_30_pct, RFPredictionClass$var_30_pct)
    MSE_Final <- as.data.frame(unique(do.call("rbind", MSE)))
    
    
    MSE_Final <- cbind(Row.Names = rownames(MSE_Final), MSE_Final)
    rownames(MSE_Final) <- NULL
    colnames(MSE_Final)[1] <- "Stock"
    colnames(MSE_Final)[2] <- "MSE"
    
    Metrics_Final <- unique(cbind(Model = "Random Forest", MSE_Final, Accuracy = Accuracy_Final$Accuracy))
  }
  
  return(Metrics_Final)
  
}


Metrics_SVM<- function(df)
{
  MSE <- list()
  Accuracy <- list()
  
  for(i in stocks)
  {
    
    x <- df %>%
      filter(ticker == i, !is.na(lag_30))
    
    SVMModel <- ksvm(var_30_pct ~ date + open + volume + close, data = data_split(x)$train, kernel = "rbfdot")
    SVMPrediction <- as.data.frame(predict(SVMModel, data_split(x)$test)) %>%
      mutate(var_30_pct =`V1`)
    SVMPredictionClass <- SVMPrediction %>%
      mutate(classifier_prediction = (ifelse(SVMPrediction$var_30_pct <= (-2), "DOWN_2_PCT", ifelse(SVMPrediction$var_30_pct >= (5), "UP_5_PCT","NOTHING"))))
    
    SVMPredictionClass$classifier_prediction <- factor(SVMPredictionClass$classifier_prediction, levels=c("DOWN_2_PCT", "NOTHING", "UP_5_PCT"))
    
    Accuracy[[i]] <- confusionMatrix((data_split(x)$test)$classifier_actual, SVMPredictionClass$classifier_prediction)[["overall"]][["Accuracy"]]
    
    Accuracy_Final <- as.data.frame(unique(do.call("rbind", Accuracy)))
    Accuracy_Final <- cbind(Row.Names = rownames(Accuracy_Final), Accuracy_Final)
    rownames(Accuracy_Final) <- NULL
    colnames(Accuracy_Final)[1] <- "Stock"
    colnames(Accuracy_Final)[2] <- "Accuracy"
    
    
    MSE[[i]] <- mse((data_split(x)$test)$var_30_pct, SVMPredictionClass$var_30_pct)
    MSE_Final <- as.data.frame(unique(do.call("rbind", MSE)))
    
    
    MSE_Final <- cbind(Row.Names = rownames(MSE_Final), MSE_Final)
    rownames(MSE_Final) <- NULL
    colnames(MSE_Final)[1] <- "Stock"
    colnames(MSE_Final)[2] <- "MSE"
    
    Metrics_Final <- unique(cbind(Model = "SVM", MSE_Final, Accuracy = Accuracy_Final$Accuracy))
  }
  
  return(Metrics_Final)
  
}

Metrics_NN<- function(df)
{
  MSE <- list()
  Accuracy <- list()
  
  for(i in stocks)
  {
    
    x <- df %>%
      filter(ticker == i, !is.na(lag_30))
    
    NNModel <- neuralnet(var_30_pct ~ open + high + low + volume + close, data = data_split(x)$train, hidden = 4)
    NNPrediction <- as.data.frame(predict(NNModel, data_split(x)$test)) %>%
      mutate(var_30_pct =`V1`)
    NNPredictionClass <- NNPrediction %>%
      mutate(classifier_prediction = (ifelse(NNPrediction$var_30_pct <= (-2), "DOWN_2_PCT", ifelse(NNPrediction$var_30_pct >= (5), "UP_5_PCT","NOTHING"))))
    
    NNPredictionClass$classifier_prediction <- factor(NNPredictionClass$classifier_prediction, levels=c("DOWN_2_PCT", "NOTHING", "UP_5_PCT"))
    
    Accuracy[[i]] <- confusionMatrix((data_split(x)$test)$classifier_actual, NNPredictionClass$classifier_prediction)[["overall"]][["Accuracy"]]
    
    Accuracy_Final <- as.data.frame(unique(do.call("rbind", Accuracy)))
    Accuracy_Final <- cbind(Row.Names = rownames(Accuracy_Final), Accuracy_Final)
    rownames(Accuracy_Final) <- NULL
    colnames(Accuracy_Final)[1] <- "Stock"
    colnames(Accuracy_Final)[2] <- "Accuracy"
    
    
    MSE[[i]] <- mse((data_split(x)$test)$var_30_pct, NNPredictionClass$var_30_pct)
    MSE_Final <- as.data.frame(unique(do.call("rbind", MSE)))
    
    
    MSE_Final <- cbind(Row.Names = rownames(MSE_Final), MSE_Final)
    rownames(MSE_Final) <- NULL
    colnames(MSE_Final)[1] <- "Stock"
    colnames(MSE_Final)[2] <- "MSE"
    
    Metrics_Final <- unique(cbind(Model = "Neural Networks", MSE_Final, Accuracy = Accuracy_Final$Accuracy))
  }
  
  return(Metrics_Final)
  
}

Metrics_AML<- function(df)
{
  MSE <- list()
  Accuracy <- list()
  
  for(i in stocks)
  {
    
    x <- df %>%
      filter(ticker == i, !is.na(lag_30))
    
    localH2O = h2o.init()
    
    x_h2o <- as.h2o(select(x, ticker, open, close, high, low, volume, lag_30, var_30_pct, classifier_actual) %>%
                      filter(!is.na(lag_30)))
    
    
    autoSplit <- h2o.splitFrame(data = x_h2o, ratios = c(0.75))
    train <- autoSplit[[1]]
    testValidation <- autoSplit[[2]]
    
    testValidationSplit <- h2o.splitFrame(data = testValidation, ratios = c(.75))
    test <- testValidationSplit[[1]]
    validation <- testValidationSplit[[2]]
    
    
    autoMLModel <- h2o.automl(y = "var_30_pct",
                              x = c("open", "close", "volume"),
                              training_frame = train,
                              validation_frame = validation,
                              balance_classes = TRUE,
                              max_runtime_secs = 60,
                              seed = 1234)
    
    pred = h2o.predict(object = autoMLModel, newdata = test)
    
    head(h2o.get_leaderboard(object = autoMLModel, extra_columns = 'ALL')$mse,1)
    
    MSE[[i]] <- head(h2o.get_leaderboard(object = autoMLModel, extra_columns = 'ALL')$mse,1)
    MSE_Final <- as.data.frame(unique(do.call("rbind", MSE)))
    
    
    MSE_Final <- cbind(Row.Names = rownames(MSE_Final), MSE_Final)
    rownames(MSE_Final) <- NULL
    colnames(MSE_Final)[1] <- "Stock"
    colnames(MSE_Final)[2] <- "MSE"
    
    
    Metrics_Final <- unique(cbind(Model = "autoML", MSE_Final, Accuracy = NA))
  }
  
  return(Metrics_Final)
  
}

Metrics_TS<- function(df)
{
  MSE <- list()
  Accuracy <- list()
  Precision <- list()
  Recall <- list()
  
  for(i in stocks)
  {
    
    x1 <- data_split(FAANG_Data)$test
    
    x2 <- data_split(df)$train %>%
      filter(ticker == i, !is.na(lag_30)) %>%
      select(Date, Time, var_30_pct) %>%
      mutate(Date = as.numeric(Date),
             Minute = minute(as.hms(Time)),
             Hour = hour(as.hms(Time))) %>%
      select(Date, Minute, Hour, var_30_pct)
    
    
    TSModel <- ts(x2$var_30_pct, start = c(18536, 14, 0), end = c(18627, 20, 59), frequency = 390)
    
    TSDecomposition <- decompose(TSModel)
    
    arimaModel <- auto.arima(TSModel)
    
    arimaForecast <- forecast(arimaModel, h = 30)
    
    arima_df <- as.data.frame(arimaForecast)
    
    arimaPredictionClass <- arima_df %>%
      mutate(var_30_pct = `Point Forecast`) %>%
      mutate(classifier_prediction = (ifelse(var_30_pct <= (-2), "DOWN_2_PCT", ifelse(var_30_pct >= (5), "UP_5_PCT","NOTHING"))))
    
    arimaPredictionClass$classifier_prediction <- factor(arimaPredictionClass$classifier_prediction, levels=c("DOWN_2_PCT", "NOTHING", "UP_5_PCT"))
    
    Accuracy[[i]] <- confusionMatrix(head(x1$classifier_actual,30), arimaPredictionClass$classifier_prediction)[["overall"]][["Accuracy"]]
    
    Accuracy_Final <- as.data.frame(unique(do.call("rbind", Accuracy)))
    Accuracy_Final <- cbind(Row.Names = rownames(Accuracy_Final), Accuracy_Final)
    rownames(Accuracy_Final) <- NULL
    colnames(Accuracy_Final)[1] <- "Stock"
    colnames(Accuracy_Final)[2] <- "Accuracy"
    
    MSE[[i]] <- mse(x1$var_30_pct, arimaPredictionClass$var_30_pct)
    MSE_Final <- as.data.frame(unique(do.call("rbind", MSE)))
    
    MSE_Final <- cbind(Row.Names = rownames(MSE_Final), MSE_Final)
    rownames(MSE_Final) <- NULL
    colnames(MSE_Final)[1] <- "Stock"
    colnames(MSE_Final)[2] <- "MSE"
    
    Metrics_Final <- unique(cbind(Model = "Time Series", MSE_Final, Accuracy = Accuracy_Final$Accuracy))
    
  }
  
  return (Metrics_Final)
}  

Metrics_Final<- function(df){
  
  Final_Metrics <- rbind(Metrics_MARS(df), Metrics_RF(df), Metrics_SVM(df), Metrics_NN(df), Metrics_AML(df), Metrics_TS(df))
  
  return(Final_Metrics)
}

write.csv(Metrics_Final(FAANG_Data), "final_metrics.csv")

final_metrics <- read.csv("final_metrics.csv")


aovResult <- aov(formula = MSE ~ Model, data = final_metrics)
aovResult

TukeyHSD(aovResult)


aovResult2 <- aov(formula = Accuracy ~ Model, data = final_metrics)
aovResult2

TukeyHSD(aovResult2)