set.seed(1000000)
### Libraries #############
library(gramEvol)
library(GA)
library(quantmod)

library(neuralnet)
library(nnet)
library(DEoptim)

library(PerformanceAnalytics)
library(BatchGetSymbols)
### Custom Functions ##########
### Data Import #############
data_start <- "2000-01-01"
data_end   <- "2026-01-01"

mystocks  <- c("BRK-B")

tickers <- c(mystocks)

# Download into a named list (preserves order)
FullDataXTS <- lapply(tickers, function(tkr) {
  getSymbols(tkr, src = "yahoo",
             from = data_start,
             to   = data_end,
             auto.assign = FALSE)
})

names(FullDataXTS) <- tickers

TrainingData <- FullDataXTS$`BRK-B`["/2020-12-31"]
TestingData  <- FullDataXTS$`BRK-B`["2021-01-01/"]
### Data Exploration & Visualization #############
# Close vs Adj Close: No difference due to no dividend payout or splits suggesting one of the two can be dropped
plot(TrainingData$`BRK-B.Close`-TrainingData$`BRK-B.Adjusted`)

# Volume: after log scaled for inference plot shows 2 volume regimes split by mid 2009
plot(log(TrainingData$`BRK-B.Volume`))

# Intra-day Variability: positive drift post 2008 which wasn't present prior motivating further volatility analysis
plot((TrainingData$`BRK-B.High` - TrainingData$`BRK-B.Low`))

# Trading week rolling SD: Inter-day drift showing posot 2008 too
roll_sd_5 <- runSD(TrainingData$`BRK-B.Close`, n = 5)

plot(roll_sd_5,
     main = "BRK-B 5-Day Rolling Standard Deviation",
     ylab = "Rolling SD",
)

# Price Plot
plot(FullDataXTS$`BRK-B`$`BRK-B.Close`)
# Log Price Plot
plot(log(FullDataXTS$`BRK-B`$`BRK-B.Close`))
# Log Returns Plot 
plot(diff(log(FullDataXTS$`BRK-B`$`BRK-B.Close`),1))


### Forecasting Engine #################
# input = Price ts
# output = Forecasts, up or down, probability confidence

# operating DF
max_lag <- 21 # 1 month of trading 

Training_Lagged_df <- data.frame(Date = index(TrainingData$`BRK-B.Close`),
                                 Price = TrainingData$`BRK-B.Close`)

for (i in 1:max_lag) {
  Training_Lagged_df[[paste0("Lag_", i)]] <- Lag(TrainingData$`BRK-B.Close`, k = i)
}
Training_Lagged_df <- na.omit(Training_Lagged_df)

# trying to get column names in the right format for var = in ForecastingRules
var_names <- paste0("Lag_", 1:max_lag)
var_rules <- lapply(var_names, function(nm) Training_Lagged_df[[nm]])

# Fitness function (RMSE)
forecastingfitnessRMSE <- function(expr) {
  result <- eval(expr)
  if (any(is.nan(result)))
    return(Inf)
  return (sqrt(mean((Training_Lagged_df$BRK.B.Close - result)^2)))
} 

# Grammar
ForecasatingRules <- list(expr = grule(op(expr, expr), func(expr), var),
                          func = grule(sin, cos, exp, log),
                          op = grule('+', '-', '*', '/', '^'),
                          var = do.call(grule, var_rules))
ForecastingGrammar <- CreateGrammar(ForecasatingRules)

# Run 
ge <- GrammaticalEvolution(ForecastingGrammar, 
                           forecastingfitnessRMSE, 
                           terminationCost = 0.05, 
                           max.depth = 3)
# Evaluation
ForecastingModel <- ge$best$expressions
eval(ForecastingModel) 
# RMSE 1.5 is not bad at all for a first attempt and seems entirely usable given scale of prices.. 
# Almost too good to be true so please sense check

# Evaluation over Test Data
Testing_Lagged_df <- data.frame(Date = index(TestingData$`BRK-B.Close`),
                                Price = TestingData$`BRK-B.Close`)

for (i in 1:max_lag) {
  Testing_Lagged_df[[paste0("Lag_", i)]] <- Lag(TestingData$`BRK-B.Close`, k = i)
}
Testing_Lagged_df <- na.omit(Testing_Lagged_df)

# not sure why its stilling using Training_Laggeddf here 
pred_test <- with(Testing_Lagged_df, eval(ForecastingModel))

rmse_test <- sqrt(mean((Testing_Lagged_df$BRK.B.Close - pred_test)^2))
rmse_test
### EMA & RSI #############
