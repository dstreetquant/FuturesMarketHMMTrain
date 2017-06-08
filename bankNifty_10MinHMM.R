library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(RMySQL)
library(depmixS4)

hostName <- "localhost"
dbName <- "FuturesDB"
tbName <- "FuturesIntradayTenMinTable"
numPrevDaysForBackTest <- 100
numCurrDaysForBackTest <- 5
numTotalDaysForBackTest <- numPrevDaysForBackTest + numCurrDaysForBackTest

NDayLookforwardLowHigh <- 10 #Parameter used when classifing in sample data as in a trend or not
HmmLikelihoodTestLength <- 5 #How many days of data to calculate the likehood ratio on to compare models

txnCostinPerc <- 0.00057709

# Dates
sql_conn <- dbConnect(MySQL(), user = "root", password = "", dbname = dbName, host = hostName)
dates_sql_command <- paste("SELECT DISTINCT Date FROM FuturesIntradayTenMinTable ORDER BY Date DESC LIMIT ", 
                           numTotalDaysForBackTest, ";", sep = "")
dates_sql_result <- dbGetQuery(sql_conn, dates_sql_command)
dbDisconnect(sql_conn)
lastTradingDays <- rev(dates_sql_result[,1])

prevDates <- as.character("")
for(day in 1:(length(lastTradingDays) - numCurrDaysForBackTest)){
  prevDates <- paste(prevDates, "'", lastTradingDays[day], "',", sep = "")
}
prevDates <- substr(prevDates, 1, nchar(prevDates) - 1)

currDates <- as.character("")
for(day in 1:numCurrDaysForBackTest){
  currDates <- paste(currDates, "'", lastTradingDays[numPrevDaysForBackTest + day], "',", sep = "")
}
currDates <- substr(currDates, 1, nchar(currDates) - 1)

# Symbol Data from DB
#symbol <- "NIFTY-1M"
symbol <- "BANKNIFTY-1M"
sql_conn <- dbConnect(MySQL(), user = "root", password = "", dbname = dbName, host = hostName)
prev_quote_sql_command <- paste("SELECT Date, Time, Open, High, Low, Close, Volume FROM ", dbName, ".",
                           tbName, " WHERE IchartsCode = '", symbol, "' AND Date IN (", prevDates, ");", sep = "")
prev_quote_sql_result <- dbGetQuery(sql_conn, prev_quote_sql_command)
curr_quote_sql_command <- paste("SELECT Date, Time, Open, High, Low, Close, Volume FROM ", dbName, ".",
                                tbName, " WHERE IchartsCode = '", symbol, "' AND Date IN (", currDates, ");", sep = "")
curr_quote_sql_result <- dbGetQuery(sql_conn, curr_quote_sql_command)
dbDisconnect(sql_conn)

# Data formatting in XTS
prev_dateTime <- paste(prev_quote_sql_result$Date, prev_quote_sql_result$Time, sep = " ")
prev_timeIndx <- strptime(prev_dateTime, format = "%Y-%m-%d %H:%M:%S")
prev_prices <- prev_quote_sql_result[, c("Open", "High", "Low", "Close", "Volume")]
curr_dateTime <- paste(curr_quote_sql_result$Date, curr_quote_sql_result$Time, sep = " ")
curr_timeIndx <- strptime(curr_dateTime, format = "%Y-%m-%d %H:%M:%S")
curr_prices <- curr_quote_sql_result[, c("Open", "High", "Low", "Close", "Volume")]
stockPriceXtsList <- list()
stockPriceXtsList[["prev"]] <- as.xts(prev_prices, prev_timeIndx)
stockPriceXtsList[["curr"]] <- as.xts(curr_prices, curr_timeIndx)

prev_time_index <- as.POSIXct(index(stockPriceXtsList[["prev"]]), format = "%Y-%m-%d %H:%M:%S")
stockPriceXtsList[["prev"]] <- apply(stockPriceXtsList[["prev"]], 2, as.numeric)
stockPriceXtsList[["prev"]] <- xts(stockPriceXtsList[["prev"]], prev_time_index)
banknifty_prev_PriceList <- stockPriceXtsList[["prev"]]
curr_time_index <- as.POSIXct(index(stockPriceXtsList[["curr"]]), format = "%Y-%m-%d %H:%M:%S")
stockPriceXtsList[["curr"]] <- apply(stockPriceXtsList[["curr"]], 2, as.numeric)
stockPriceXtsList[["curr"]] <- xts(stockPriceXtsList[["curr"]], curr_time_index)
banknifty_curr_PriceList <- stockPriceXtsList[["curr"]]

# Closing price vector for plotting
bankNifty_prev_Cl_Vector <- as.vector(banknifty_prev_PriceList$Close)
bankNifty_curr_Cl_Vector <- as.vector(banknifty_curr_PriceList$Close)

# Returns
banknifty_prev_logRet <- log(banknifty_prev_PriceList$Close) - log(banknifty_prev_PriceList$Open) - txnCostinPerc
banknifty_curr_logRet <- log(banknifty_curr_PriceList$Close) - log(banknifty_curr_PriceList$Open) - txnCostinPerc

# Function to convert intermediate of buy-sell trigger to remain inTrade
ConvertTofullSignal <- function(signal){
  results <- rep(0,length(signal))
  intrade <- F
  for(i in seq(1, length(signal))){
    if(signal[i] == -1){
      intrade <- F
    }
    if(signal[i] == 1 || intrade){
      results[i] <- 1
      intrade <- T
    }
  }
  return(results)
}

#Generate long trend signal
LongTrendSignal <- rep(0, nrow(banknifty_prev_PriceList))
for(i in seq(1, nrow(banknifty_prev_PriceList) - NDayLookforwardLowHigh)){
  dataBlock <- Cl(banknifty_prev_PriceList[seq(i, i+NDayLookforwardLowHigh), ])
  if(coredata(Cl(banknifty_prev_PriceList[i, ])) == min(coredata(dataBlock))){
    LongTrendSignal[i] <- 1
  }
  if(coredata(Cl(banknifty_prev_PriceList[i, ])) == max(coredata(dataBlock))){
    LongTrendSignal[i] <- -1
  }
}
LongTrendSignal <- ConvertTofullSignal(LongTrendSignal)

#Generate short trend signal
ShortTrendSignal <- rep(0, nrow(banknifty_prev_PriceList))
for(i in seq(1, nrow(banknifty_prev_PriceList) - NDayLookforwardLowHigh)){
  dataBlock <- Cl(banknifty_prev_PriceList[seq(i, i+NDayLookforwardLowHigh),])
  if(coredata(Cl(banknifty_prev_PriceList[i, ])) == max(coredata(dataBlock))){
    ShortTrendSignal[i] <- 1
  }
  if(coredata(Cl(banknifty_prev_PriceList[i, ])) == min(coredata(dataBlock))){
    ShortTrendSignal[i] <- -1
  }
}
ShortTrendSignal <- ConvertTofullSignal(ShortTrendSignal)

# # Plotting training data
# par(mfrow=c(3, 1))
# plot(bankNifty_prev_Cl_Vector, type = "l")
# plot(LongTrendSignal, type="l")
# plot(ShortTrendSignal, type="l")

# Function to Extract a list of varying length features for each signal/class label
CreateListOfMatrixFeatures <- function(features,signal){
  results <- list()
  extract <- F
  for(i in seq(1, length(signal))){
    if(signal[i] == 1 && !extract){
      startIndex <- i
      extract <- T
    }
    if(signal[i] == 0 && extract){
      endIndex <- i-1
      dataBlock <- features[startIndex : endIndex, ]
      extract <- F
      #print(dataBlock)
      results[[length(results) + 1]] <- as.matrix(dataBlock)
    }
#     if(i == length(signal) && extract){
#       endIndex <- i
#       dataBlock <- features[startIndex : endIndex, ]
#       extract <- F
#       results[[length(results) + 1]] <- as.matrix(dataBlock)
#     }
  }
  return(results)
}

#=============
#HMM Training
#=============

# Generate the features that describe the data & split into training and out of sample sets
banknifty_prev_features <- cbind(banknifty_prev_logRet, 
                                 Hi(banknifty_prev_PriceList)/Lo(banknifty_prev_PriceList),
                                 Hi(banknifty_prev_PriceList)/Op(banknifty_prev_PriceList),
                                 Hi(banknifty_prev_PriceList)/Cl(banknifty_prev_PriceList),
                                 Op(banknifty_prev_PriceList)/Cl(banknifty_prev_PriceList),
                                 Lo(banknifty_prev_PriceList)/Cl(banknifty_prev_PriceList),
                                 Lo(banknifty_prev_PriceList)/Op(banknifty_prev_PriceList))
colnames(banknifty_prev_features) <- c("logRet", "HiLo", "HiOp", "HiCl", "OpCl", "LoCl", "LoOp")
banknifty_curr_features <- cbind(banknifty_curr_logRet, 
                                 Hi(banknifty_curr_PriceList)/Lo(banknifty_curr_PriceList),
                                 Hi(banknifty_curr_PriceList)/Op(banknifty_curr_PriceList),
                                 Hi(banknifty_curr_PriceList)/Cl(banknifty_curr_PriceList),
                                 Op(banknifty_curr_PriceList)/Cl(banknifty_curr_PriceList),
                                 Lo(banknifty_curr_PriceList)/Cl(banknifty_curr_PriceList),
                                 Lo(banknifty_curr_PriceList)/Op(banknifty_curr_PriceList))
colnames(banknifty_curr_features) <- c("logRet", "HiLo", "HiOp", "HiCl", "OpCl", "LoCl", "LoOp")

# For each long / short position extract the corresponding features data and create list of them
banknifty_prev_LongFeaturesList <- CreateListOfMatrixFeatures(banknifty_prev_features, LongTrendSignal)
banknifty_prev_LongFeaturesListDF <- data.frame(stringsAsFactors = F)
for(i in 1:length(banknifty_prev_LongFeaturesList)){
  banknifty_prev_LongFeaturesListDF <- rbind(banknifty_prev_LongFeaturesListDF, banknifty_prev_LongFeaturesList[[i]])
}
banknifty_prev_ShortFeaturesList <- CreateListOfMatrixFeatures(banknifty_prev_features, ShortTrendSignal)
banknifty_prev_ShortFeaturesListDF <- data.frame(stringsAsFactors = F)
for(i in 1:length(banknifty_prev_ShortFeaturesList)){
  banknifty_prev_ShortFeaturesListDF <- rbind(banknifty_prev_ShortFeaturesListDF, banknifty_prev_ShortFeaturesList[[i]])
}

# Train the HMM models
# seedReturnsDF <- data.frame(stringsAsFactors = F)
# for(seedNum in 1:100){
# set.seed(seedNum)
set.seed(34)

longModelHMM <- depmix(list(logRet~1, HiLo~1, HiOp~1, HiCl~1, OpCl~1, LoCl~1, LoOp~1), 
                       data = banknifty_prev_LongFeaturesListDF, nstates=3, 
                       family = list(gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian()))
longModelHMMFit <- fit(longModelHMM, verbose = FALSE)

shortModelHMM <- depmix(list(logRet~1, HiLo~1, HiOp~1, HiCl~1, OpCl~1, LoCl~1, LoOp~1), 
                       data = banknifty_prev_ShortFeaturesListDF, nstates=3, 
                       family = list(gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian()))
shortModelHMMFit <- fit(shortModelHMM, verbose = FALSE)

# Will take NDayLookforwardLowHigh days of data and calculate the rolling log likelihood for each HMM model
# banknifty_prev_LongLikelihood <- rollapply(banknifty_prev_features, HmmLikelihoodTestLength, align="right",
#                                     na.pad=T, by.column=F, function(x) {
#                                             xModelHMM <- depmix(list(logRet~1, HiLo~1, HiOp~1, HiCl~1, OpCl~1, LoCl~1, LoOp~1), 
#                                                                 data = x, nstates=3, 
#                                                                 family = list(gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian()))
#                                             xModelHMM <- setpars(xModelHMM, getpars(longModelHMMFit))
#                                             forwardbackward(xModelHMM)$logLike
#                                           }
#                                     )
# banknifty_prev_ShortLikelihood <- rollapply(banknifty_prev_features, HmmLikelihoodTestLength, align="right",
#                                            na.pad=T, by.column=F, function(x) {
#                                              xModelHMM <- depmix(list(logRet~1, HiLo~1, HiOp~1, HiCl~1, OpCl~1, LoCl~1, LoOp~1), 
#                                                                  data = x, nstates=3, 
#                                                                  family = list(gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian()))
#                                              xModelHMM <- setpars(xModelHMM, getpars(shortModelHMMFit))
#                                              forwardbackward(xModelHMM)$logLike
#                                            }
#                                     )
banknifty_curr_LongLikelihood <- rollapply(banknifty_curr_features, HmmLikelihoodTestLength, align="right",
                                           na.pad=T, by.column=F, function(x) {
                                             xModelHMM <- depmix(list(logRet~1, HiLo~1, HiOp~1, HiCl~1, OpCl~1, LoCl~1, LoOp~1), 
                                                                 data = x, nstates=3, 
                                                                 family = list(gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian()))
                                             xModelHMM <- setpars(xModelHMM, getpars(longModelHMMFit))
                                             forwardbackward(xModelHMM)$logLike
                                           }
                                    )
banknifty_curr_ShortLikelihood <- rollapply(banknifty_curr_features, HmmLikelihoodTestLength, align="right",
                                            na.pad=T, by.column=F, function(x) {
                                              xModelHMM <- depmix(list(logRet~1, HiLo~1, HiOp~1, HiCl~1, OpCl~1, LoCl~1, LoOp~1), 
                                                                  data = x, nstates=3, 
                                                                  family = list(gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian(), gaussian()))
                                              xModelHMM <- setpars(xModelHMM, getpars(shortModelHMMFit))
                                              forwardbackward(xModelHMM)$logLike
                                            }
                                    )

# Create signals for plot / trading
banknifty_curr_LongTrendSignalForPlot <- 1 * (banknifty_curr_LongLikelihood > banknifty_curr_ShortLikelihood)
banknifty_curr_LongTrendSignalForPlot[banknifty_curr_LongTrendSignalForPlot == 0] <- NaN
banknifty_curr_LongTrendSignalForPlot <- banknifty_curr_LongTrendSignalForPlot * Cl(banknifty_curr_PriceList) - 10 #10 shift for plotting

banknifty_curr_ShortTrendSignalForPlot <- 1 * (banknifty_curr_LongLikelihood < banknifty_curr_ShortLikelihood)
banknifty_curr_ShortTrendSignalForPlot[banknifty_curr_ShortTrendSignalForPlot == 0] <- NaN
banknifty_curr_ShortTrendSignalForPlot <- banknifty_curr_ShortTrendSignalForPlot * Cl(banknifty_curr_PriceList) + 10 #10 shift for plotting

# Calculate Out of Sample Returns
banknifty_curr_LongReturns <- Lag((1 * (banknifty_curr_LongLikelihood > banknifty_curr_ShortLikelihood))) * (banknifty_curr_logRet)
banknifty_curr_LongReturns[is.na(banknifty_curr_LongReturns)] <- 0
banknifty_curr_CumLongReturns <- log(cumprod(1 + banknifty_curr_LongReturns))

banknifty_curr_ShortReturns <- Lag(-1 * ( 1 * (banknifty_curr_LongLikelihood < banknifty_curr_ShortLikelihood))) * (banknifty_curr_logRet)
banknifty_curr_ShortReturns[is.na(banknifty_curr_ShortReturns)] <- 0
banknifty_curr_CumShortReturns <- log(cumprod(1 + banknifty_curr_ShortReturns))

banknifty_curr_TotalReturns <- banknifty_curr_LongReturns + banknifty_curr_ShortReturns
banknifty_curr_TotalReturns[is.na(banknifty_curr_TotalReturns)] <- 0
banknifty_curr_CumTotalReturns <- log(cumprod(1 + banknifty_curr_TotalReturns))

finalRet <- as.numeric(banknifty_curr_CumTotalReturns[length(banknifty_curr_CumTotalReturns), 1])
# seedReturnsDF <- rbind(seedReturnsDF, c(seedNum, finalRet))
# colnames(seedReturnsDF) <- c("SeedNum", "FinalReturn")
# }

# # Plotting
# dev.new()
# layout(1:3)
# 
# plot(as.vector(banknifty_curr_PriceList$Close), main = "CL PRICE - HMM Trend Following - Out of Sample", type = "l")
# lines(as.vector(banknifty_curr_LongTrendSignalForPlot[, 1]), col = "green", type = "l")
# lines(as.vector(banknifty_curr_ShortTrendSignalForPlot[, 1]), col = "red", type = "l")
# legend(x = 'bottomright', c("Closing Price", "Long Signal", "Short Signal"), fill = c("black", "green", "red"), bty = 'n')
# 
# plot(as.vector(banknifty_curr_LongLikelihood[, 1]), main = "LOG LIKELIHOOD - HMM model - Out Of Sample", type = "l")
# lines(as.vector(banknifty_curr_LongLikelihood[, 1]), col = "green", type = "l")
# lines(as.vector(banknifty_curr_ShortLikelihood[, 1]), col = "red", type = "l")
# legend(x = 'bottomright', c("Long HMM Likelihood", "Short HMM Likelihood"), fill = c("green", "red"), bty = 'n')
# 
# plot(as.vector(banknifty_curr_CumTotalReturns[, 1]), main = "STRATEGY RETURNS - HMM Trend Following - Out of Sample", type = "l")
# lines(as.vector(banknifty_curr_CumLongReturns[, 1]), col = "green", type = "l")
# lines(as.vector(banknifty_curr_CumShortReturns[, 1]), col = "red", type = "l")
# legend(x = 'bottomright', c("Total Returns", "Long Trend Returns", "Short Trend Returns"), fill = c("black", "green", "red"), bty = 'n')

# # Volatility Plotting
# dev.new()
# layout(1:3)
# 
# set.seed(1)
# ATRperiod <- 10
# # plot(as.vector(banknifty_curr_PriceList$Close[-c(1:ATRperiod), ]), main = "CL PRICE - HMM Trend Following - Out of Sample", type = "l")
# # lines(as.vector(banknifty_curr_LongTrendSignalForPlot[-c(1:ATRperiod), 1]), col = "green", type = "l")
# # lines(as.vector(banknifty_curr_ShortTrendSignalForPlot[-c(1:ATRperiod), 1]), col = "red", type = "l")
# # legend(x = 'bottomright', c("Closing Price", "Long Signal", "Short Signal"), fill = c("black", "green", "red"), bty = 'n')
# 
# volATRindicator <- ATR(banknifty_curr_PriceList[,2:4], n = ATRperiod)
# volATR <- volATRindicator[,2]
# 
# volLogReturns <- log(banknifty_curr_PriceList$Close) - log(banknifty_curr_PriceList$Open)
# volModelData <- data.frame(volLogReturns, volATR)
# volModelData <- volModelData[-c(1:ATRperiod),]
# colnames(volModelData)<-c("LogReturns","ATR") #name our columns
# 
# volHMM <- depmix(list(LogReturns~1, ATR~1), data = volModelData, nstates=3, family = list(gaussian(), gaussian()))
# volHMMfit <- fit(volHMM, verbose = FALSE)
# volHMMpost<-posterior(volHMMfit)
# volHMMpostDF <- data.frame(volModelData, volHMMpost)
# 
# plot(as.vector(volATR[-c(1:ATRperiod), ]), type = "l")
# plot(as.vector(banknifty_curr_CumTotalReturns[-c(1:ATRperiod), 1]), type = "l")
# plot(as.vector(volHMMpostDF$state), type = "l")

# Sharpe Ratio
#print(SharpeRatio.annualized(banknifty_curr_TotalReturns))
