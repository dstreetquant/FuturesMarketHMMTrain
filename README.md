# FuturesMarketHMMTrain
A Hidden Markov Chain Model to train Bank Nifty Futures Market Data and predict Long/Short Signals.
This code takes into account the brokerage as well as transactions costs for trading future in NSE stock market.

The R code heavily relies on R libs - 'quantmod' and 'PerformanceAnalytics' as the base of financial modelling framework. 
At the same time, it relies on Hidden Markov Chain R lib - 'depmixS4' for modelling the HMM on actual data.

# The tuning parameters for the model
```numPrevDaysForBackTest <- 100``` - Number of previous days to consider for model
```numCurrDaysForBackTest <- 5``` - Number of previous days to consider for model

```NDayLookforwardLowHigh <- 10``` - Parameter used when classifing in sample data as in a trend or not
```HmmLikelihoodTestLength <- 5```  - How many days of data to calculate the likehood ratio on to compare models

```txnCostinPerc <- 0.00057709``` - Complete Transaction cost for each trade (considers both legs - long as well as short)

# Parameters for Data
The code relies on fetching futures data from MySQL. Below are the Database parameters
**hostName** <- "localhost" - Hostname of the database
**dbName** <- "FuturesDB" - Database Name for the futures data
**tbName** <- "FuturesIntradayTenMinTable" - Table Name for the futures data
It expects the data to be laid in the column name of Date, Time, Open, High, Low, Close, Volume for OHLCV data.
