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



run_GP <- \(grammar,
            fit_fun,
            min_lag,
            max_lag,
            data_start,
            data_end,
            tkr,
            pre_processor  = identity,
            post_processor = identity) {

  # Download into a named list (preserves order)
  FullDataXTS <- getSymbols(tkr, src = "yahoo",
                            from = data_start,
                            to   = data_end,
                            auto.assign = FALSE) |> pre_processor()

  ########## Change '-' in ticker names to '.' because R has a delightful habit of
  ########## unexpectedly sanitising the column names so they can be used as symbols
  tkr <- gsub("-", ".", tkr, fixed = TRUE)
  colnames(FullDataXTS) <- make.names(colnames(FullDataXTS), unique = TRUE)

  ### Forecasting Engine #################
  # input = Price ts
  # output = Forecasts, up or down, probability confidence

  make_field <- \(f) paste0(tkr, ".", f)
  volume <- paste0(tkr, ".Volume")
  open   <- paste0(tkr, ".Open")
  high   <- paste0(tkr, ".High")
  low    <- paste0(tkr, ".Low")
  close  <- paste0(tkr, ".Close")

  fields <- c(volume, open, high, low, close)
  var_names <- character(0)

  lagged_data <- FullDataXTS

  for (f in fields) {
    for (i in min_lag:max_lag) {
      label <- paste0(f, "_Lag_", i)
      var_names <- c(var_names, label)
      lag <- Lag(lagged_data[, f], k = i)
      colnames(lag) <- label
      lagged_data <- merge(lagged_data, lag)
    }
  }

  lagged_data <- na.omit(lagged_data)
  halfway <- mean(as.Date(c(data_start, data_end)))
  training_end <- paste0("/", halfway)
  testing_start <- paste0(as.Date(halfway) + 1, "/")

  TrainingData <- lagged_data[training_end]
  TestingData  <- lagged_data[testing_start]

  # trying to get column names in the right format for var = in ForecastingRules
  # TODO is this actually used? see line 81
  var_rules <- lapply(var_names, as.symbol)

  # This makes the Lag_1, Lag_2 etc. argument find its way to the TrainingData frame
  env <- list2env(as.list(TrainingData), parent = .GlobalEnv)
  grammar$data <- do.call(grule, lapply(var_names, as.name))

  ForecastingGrammar <- CreateGrammar(grammar)

  # Run
  ge <- GrammaticalEvolution(ForecastingGrammar,
                             \(expr) fit_fun(env, TrainingData[, close], expr),
                             terminationCost = 0.05,
                             max.depth = 5)

  # Evaluation
  ForecastingModel  <- ge$best$expressions[1]
  # eval(ForecastingModel, envir = env)
  pred_test         <- with(TestingData, eval(ForecastingModel))
  cls               <- TestingData[, close]
  comparator        <- merge(cls, prediction = pred_test) |> post_processor()
  rmse_test         <- sqrt(mean((cls - pred_test)^2)) |> post_processor()

  print(tail(comparator, 5))
  print(ForecastingModel)
  print(rmse_test)
}

# Helper function that returns a scalar from the cor function. Cannot be used 
# with xts objects or other collections with more than 2 columns
cor_scalar <- function(x, y) as.numeric(cor(x, y))
var_scalar <- function(x) as.numeric(var(x))


# These functions wrap functions that can return NA and rturn 0 instead. Using
# Inf doesn't work since it makes the trig functions return NA.
p_log <- \(num) {
  if (any(num < 0, na.rm = TRUE)) {
    Inf
  } else {
    log(num)
  }
}

p_sin <- \(num) {
  if (any(!is.finite(num))) {
    Inf
  } else {
    sin(num)
  }
}

p_cos <- \(num) {
  if (any(!is.finite(num))) {
    Inf
  } else {
    cos(num)
  }
}



# Grammar - note that the data rule, rhich references the columns of the lagged
# dataset is introduced during processing
ForecastingRules <- list(expr       = grule(op(arithmetic, arithmetic),
                                            reducer(lists),
                                            func(arithmetic),
                                            data
                                            ),
                         arithmetic = grule(op(arithmetic, arithmetic), # Things that are acceptable to an arithmetic function in R
                                            func(arithmetic),
                                            reducer(lists),
                                            stats(data),
                                            stats2(data, data),
                                            const),
                         func       = grule(p_sin, p_cos, exp, p_log),
                         op         = grule('+', '-', '*', '/'),
                         reducer    = grule(rowMeans),
                         stats      = grule(var_scalar, mean),
                         stats2     = grule(cor_scalar),
                         lists      = grule(merge(data, lists), merge(data, data)),
                         const      = gvrule(seq(0, 200, by = 0.1))
                         )

# Fitness function (RMSE)
fitness <- \(env, training, expr) {
  result <- eval(expr, envir = env)
  # Get rid of NaNs, and sometimes the expression returns a single value
  if (any(is.nan(result)) || length(result) != length(training))
    Inf
  else
    sqrt(mean((training - result)^2))
}

# operating DF
max_lag <- 200 # 1 month of trading
min_lag <- 1

log_processor       <- \(data) log(data)
log_diff_processor  <- \(data) data |> log() |> diff() |> na.omit()

log_post_processor      <- \(data) exp(data)
log_diff_post_processor <- \(data) data |> cumsum() |> exp()

tkr  <- "KGF.L"

run_GP(ForecastingRules,
       fitness,
       min_lag,
       max_lag,
       data_start,
       data_end,
       tkr,
       pre_processor  = log_processor,
       post_processor = log_post_processor)

############################# Technical Analysis Indicators ####################
sma <- \(data, n) rowMeans(data[, 2:n], na.rm = TRUE)

indicator_fit <- \(env, training, expr) {
  result <- eval(expr, envir = env)
  if (any(is.nan(result)) || length(result) != length(training))
    Inf
  else
    sum(training[, close] * result)
}

indicator_rules <- list(expr        = grule(activation(func)),
                        activation  = grule(plogis, tanh),
                        func        = grule(op(func, func),
                                            indic(data)
                                            ),
                        op          = grule('+', '-', '*', '/'),
                        indic       = grule(sma(aspects, const)),
                        aspects     = do.call(grule, c(open, high, low, close, volume)),
                        const       = gvrule(1:200)
                        )

run_GP(indicator_rules,
       indicator_fit,
       min_lag, 
       max_lag,
       data_start,
       data_end,
       tkr)
