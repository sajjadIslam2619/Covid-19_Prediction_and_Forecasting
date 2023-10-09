# Author: K M Sajjadul Islam

# install packages
#install.packages(c("forecast", "cluster"))

# clear working directory
rm(list=ls())

library(cluster)
library(forecast)


# Data file attached with the submission files. Data can be also be found in:https://www.kaggle.com/datasets/sudalairajkumar/novel-corona-virus-2019-dataset
# Please change local directory before execution. 
data<-read.csv(file = '/Users/sajjadislam/Documents/Academic Projects/Data analytics/Project 2/covid_19_data.csv', header=TRUE, stringsAsFactors=FALSE)
#dim(data)
# Remove row with empty value.
data <- na.omit(data)
#dim(data)
#View(data)

########### Start: Implement agglomerative clustering ####################
# groupby 'Country.Region' and do Hierarchical Clustering using "Confirmed", "Deaths", "Recovered" columns
# group by Country and sum columns
df_sum <- aggregate(data[, c("Confirmed", "Deaths", "Recovered")], 
                    by = list(Country.Region = data$Country.Region), 
                    FUN = sum)

# Sort by Confirmed cases 
df_sorted <- df_sum[order(-df_sum$Confirmed),]
df_sum <- df_sorted

# To vizualize the dendogram clearly taking only top 20. 
#df_sorted_20 <- df_sum[order(-df_sum$Confirmed),][1:20,]
#df_sum <- df_sorted_20

# z-score scale the data (only columns 2 through 4); excludes the 'Country.Region' variable from analysis
data2 <- scale(df_sum[, 2:4])
# similarity measures; alternative approach:"manhattan" or "binary" (for Jaccard's)
data3 <- dist(data2, method = "euclidean")
# utilize the agnes function to perform agglomerative clustering; alternative approach: "single", "complete", "average"
aResult <- agnes(data3, diss = TRUE, method = "ward")
#aResult
# Agglomerative coefficient:  0.9951197, which indicates strong clustering.Coefficients > 0.75 indicate strong clustering

# produce the banner plot and dendrogram
plot(aResult, labels = df_sum$Country.Region)

# 'k' is set to the number of desired clusters. 5 cluster best depict the result
aClusters <- cutree(aResult, k = 5)

#append the result to the original data frame
clusterData<- data.frame(df_sum, aClusters)
View(clusterData)

# summary statistics for each cluster
#summary(subset(clusterData, aClusters == 1))
#summary(subset(clusterData, aClusters == 2))
#summary(subset(clusterData, aClusters == 3))
#summary(subset(clusterData, aClusters == 4))
#summary(subset(clusterData, aClusters == 5))

# Identify the number of observations in each cluster type
summary(as.factor(aClusters))

########### End: Implement agglomerative clustering ####################


########### Start: Time series analysis #################################

df_sum <- aggregate(data[, c("Confirmed", "Deaths", "Recovered")], 
                    by = list(ObservationDate = data$ObservationDate), 
                    FUN = sum)

#View(df_sum)
############### Exponential Smoothing ##################################

exData <- ts(df_sum$Confirmed, start = c(2020,1,22), end = c(2021,5,28), frequency=365)
plot(exData, main = "Covid-19 Exponential Smoothing Time Series Plot", xlab = "Year", ylab = "Confirmed Case")

exTData <- window(exData, end = c(2020.330))
exVData <- window(exData, start = c(2020.331))

exHUser <- ets(exTData, model = "ANN", alpha=0.4, beta=0.3)
exHCmp <- ets(exTData, model = "ANN")

#results with summary
#summary(exHUser)
#summary(exHCmp)

#find number of observations in the validation set
ex_nV <- length(exVData)
#make 'H' number of forecasts for the validation set
exfUser <-forecast(exHUser, h=ex_nV)
exfCmp <-forecast(exHCmp, h=ex_nV)
#accuracy allows us to view the performance measures
accuracy(exfUser,exVData)
accuracy(exfCmp,exVData)

#Combine the training and validation sets re-implement the model for forecasting
exHFinal <- ets(exData, model = "ANN")
#h is set to 1 for a one-day forecast period
forecast(exHFinal, h=1)
# Actual: 169951560 Foretasted: 21094507


################# Holt-Winters #######################################

hwData <- ts(df_sum$Confirmed, start = c(2020,1,22), end = c(2021,4,31), frequency=12)
#print(hwData)
plot(hwData, main = "Covid-19 Time Series Plot (Holt-Winters)", xlab = "Year", ylab = "Confirmed Case")

#hwTData and hwVData will be training and validation, respectively
hwTData <- window(hwData, end = c(2020.330))
hwVData <- window(hwData, start = c(2020.331))

#ets establishes error, trend, and seasonality features
WAdd <- ets(hwTData, model = "AAA", restrict = FALSE)
WMlt <- ets(hwTData, model = "AAM", restrict = FALSE)

#Results with summary
#summary(WAdd)
#summary(WMlt)

#find number of observations in the validation set
hw_nV <- length(hwVData)

#make 'H' number of forecasts for the validation set
fAdd <-forecast(WAdd, h=hw_nV)
fMlt <-forecast(WMlt, h=hw_nV)

#accuracy allows us to view the performance measures
accuracy(fAdd,hwVData)
accuracy(fMlt,hwVData)

suppressWarnings({WFinal <- ets(hwData, model = "AAM", restrict = FALSE)})
forecast(WFinal, h=1)
# Actual: 95128170 Forcasted: 95225317
########### End: Time series analysis #################################
