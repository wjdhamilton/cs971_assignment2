# set.seed(1000000)
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

tkr  <- "BRK-B"


# Download into a named list (preserves order)
FullDataXTS <- getSymbols(tkr, src = "yahoo",
                          from = data_start,
                          to   = data_end,
                          auto.assign = FALSE)



### Forecasting Engine #################
# input = Price ts
# output = Forecasts, up or down, probability confidence

# operating DF
max_lag <- 21 # 1 month of trading


prefixes = c("Open", "High", "Low", "Close", "Volume")
var_names <- character(0)

lagged_data <- FullDataXTS
lagged_data[, "BRK-B.Volume"] <- log(lagged_data[, "BRK-B.Volume"]) # Scale the volume to a more amenable number

for (f in prefixes) {
  for (i in 1:max_lag) {
    label <- paste0(f, "_Lag_", i)
    var_names <- c(var_names, label)
    column <- grep(paste0("\\.", f, "$"), colnames(lagged_data), value = TRUE)
    lag <- Lag(lagged_data[, column], k = i)
    colnames(lag) <- label
    lagged_data <- merge(lagged_data, lag)
  }
}

lagged_data <- na.omit(lagged_data)

TrainingData <- lagged_data["/2020-12-31"]
TestingData  <- lagged_data["2021-01-01/"]

# trying to get column names in the right format for var = in ForecastingRules
var_rules <- lapply(var_names, as.symbol)

# These functions wrap functions that can return NA and rturn 0 instead. Using
# Inf doesn't work since it makes the trig functions return NA.
p_log <- \(num) {
  if (any(num < 0, na.rm = TRUE)) {
    0
  } else {
    log(num)
  }
}

p_sin <- \(num) {
  if(any(!is.finite(num))) {
    0
  } else {
    sin(num)
  }
}

p_cos <- \(num) {
  if(any(!is.finite(num))) {
    0
  } else {
    cos(num)
  }
}



# This makes the Lag_1, Lag_2 etc. argument find its way to the TrainingData frame
env <- list2env(as.list(TrainingData), parent = .GlobalEnv)

# Fitness function (RMSE)
forecastingfitnessRMSE <- function(expr) {
  result <- eval(expr, envir = env)
  if (any(is.nan(result)))
    Inf
  else
    sqrt(mean((TrainingData$BRK.B.Close - result)^2))
}

# Grammar
ForecastingRules <- list(expr       = grule(op(arithmetic, arithmetic),
                                            reducer(lists),
                                            func(arithmetic),
                                            var),
                         arithmetic = grule(op(arithmetic, arithmetic),
                                            func(arithmetic),
                                            reducer(lists),
                                            data,
                                            var), # Things that are acceptable to an arithmetic function in R
                         func       = grule(p_sin, p_cos, exp, p_log),
                         op         = grule('+', '-', '*', '/'),
                         reducer    = grule(rowMeans),
                         lists      = grule(merge(data)),
                         data       = do.call(grule, lapply(var_names, as.name)),
                         var        = gvrule(1:200)
                         )
ForecastingGrammar <- CreateGrammar(ForecastingRules)

# Run
ge <- GrammaticalEvolution(ForecastingGrammar,
                           forecastingfitnessRMSE,
                           terminationCost = 0.05,
                           max.depth = 5)
# Evaluation
ForecastingModel <- ge$best$expressions[1]
eval(ForecastingModel, envir = env)
# RMSE 1.5 is not bad at all for a first attempt and seems entirely usable given scale of prices.. 
# Almost too good to be true so please sense check

pred_test <- with(TrainingData, eval(ForecastingModel))
# pred_test <- lag.xts(pred_test, k = -1)

cls <- TrainingData$BRK.B.Close

comparator <- merge(cls, prediction = pred_test)

rmse_test <- sqrt(mean((cls - pred_test)^2))

print(tail(comparator, 10))
print(ForecastingModel)
print(rmse_test)
