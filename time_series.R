#install.packages("forecast")

##clear working directory
rm(list=ls())

#library(xts)
library(forecast) 

# Data file local directory. Data can be found in: 
data<-read.csv(file = '/Users/sajjadislam/Documents/Academic Projects/Data analytics/Project 2/covid_19_data.csv', header=TRUE, stringsAsFactors=FALSE)
View(data)

data$ObservationDate <- as.Date(data$ObservationDate, format = "%m/%d/%Y")
View(data)

data_agg <- aggregate(data[, c("Confirmed", "Deaths", "Recovered")], 
                    by = list(ObservationDate = data$ObservationDate), 
                    FUN = sum)
View(data_agg)

############### Exponential Smoothing ##########################

###dates <- seq(from = as.Date("2020-01-22"), to = as.Date("2021-05-29"), by = "day")
###newData <- xts(data_agg$Confirmed, order.by = dates)
###View(newData)

#newData <- ts(data_agg$Confirmed, start = c(2020,1), frequency=365)

newData <- ts(data_agg$Confirmed, start = c(2020,1,22), end = c(2021,5,28), frequency=365)
print(newData)
plot(newData, main = "Example Time Series Plot", xlab = "Year", ylab = "Confirmed Case")

#time(newData) <- dates

TData <- window(newData, end = c(2020.400))
VData <- window(newData, start = c(2020.401))

HUser <- ets(TData, model = "ANN", alpha=0.2, beta=0.10)
HCmp <- ets(TData, model = "ANN")

##view results with summary
summary(HUser)
summary(HCmp)

##find number of observations in the validation set
nV <- length(VData)
##make 'H' number of forecasts for the validation set
fUser <-forecast(HUser, h=nV)
fCmp <-forecast(HCmp, h=nV)
##accuracy allows us to view the performance measures
accuracy(fUser,VData)
accuracy(fCmp,VData)

##MAE = MAD in R
##we can find MSE by squaring RMSE

##finally, we can combine the training and validation sets
##and re-implement the preferred model for forecasting
HFinal <- ets(newData, model = "ANN")
##h is set to 1 for a one-year forecast period
forecast(HFinal, h=1)
## Actual: 99771101 Forcasted: 99771051

############# Linear Trend with seasonality #############
##ts creates the time series object
##specify start and end
##specify frequency (number of seasons in year)
newData <- ts(data_agg$Confirmed, start = c(2020,1,22), end = c(2021,5,28), frequency=12)
plot(newData, main = "Example Time Series Plot", xlab = "Year", ylab = "Confirmed Case")
##tslm to estimate the model
##summary to view results
TSReg <- tslm(newData ~ trend + season)
summary(TSReg)

##forecast allows us to forecast 
##with h specifying the number of forecasts performed
forecast(TSReg, h=3)


################# Holt-Winters #########################

newData <- ts(data_agg$Confirmed, start = c(2020,1,22), end = c(2021,5,28), frequency=6)
print(newData)
plot(newData, main = "Example Time Series Plot", xlab = "Year", ylab = "Confirmed Case")
##window partitions the data
##TData and VData will be training and validation, respectively
TData <- window(newData, end = c(2020.300))
VData <- window(newData, start = c(2020.301))

##ets establishes error, trend, and seasonality features
WAdd <- ets(TData, model = "AAA", restrict = FALSE)
WMlt <- ets(TData, model = "AAM", restrict = FALSE)

##view results with summary
summary(WAdd)
summary(WMlt)

##find number of observations in the validation set
nV <- length(VData)

##make 'H' number of forecasts for the validation set
fAdd <-forecast(WAdd, h=nV)
fMlt <-forecast(WMlt, h=nV)

##accuracy allows us to view the performance measures
accuracy(fAdd,VData)
accuracy(fMlt,VData)
## Note that R denotes validation set as test set, 
##MAD as MAE, and MSE is found by squaring the reported RMSE.
WFinal <- ets(newData, model = "AAM", restrict = FALSE)
forecast(WFinal, h=4)