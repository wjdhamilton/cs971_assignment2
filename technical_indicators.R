# set.seed(1000000)
### Libraries #############
library(gramEvol)
library(quantmod)
library(xts)

library(PerformanceAnalytics)
library(BatchGetSymbols)
### Custom Functions ##########
### Data Import #############
data_start <- "2000-01-01"
data_end   <- "2026-01-01"

tkr <- "BZ=F"

# Download into a named list (preserves order)
FullDataXTS <- getSymbols(tkr, src = "yahoo",
                          from = data_start,
                          to   = data_end,
                          auto.assign = FALSE) |> na.omit()

FullDataXTS   <- log(FullDataXTS)

halfway       <- mean(as.Date(c(data_start, data_end)))
training_end  <- paste0("/", halfway)
testing_start <- paste0(as.Date(halfway) + 1, "/")

########## Change '-' in ticker names to '.' because R has a delightful habit of
########## unexpectedly sanitising the column names so they are compatible as symbols
tkr <- make.names(tkr)
colnames(FullDataXTS) <- make.names(colnames(FullDataXTS), unique = TRUE)

TrainingData <- FullDataXTS[training_end]
TestingData  <- FullDataXTS[testing_start]

### Forecasting Engine #################
# input = Price ts
# output = Forecasts, up or down, probability confidence

make_field <- \(f) paste0(tkr, ".", f)
#TODO this should use make_field
volume <- paste0(tkr, ".Volume")
open   <- paste0(tkr, ".Open")
high   <- paste0(tkr, ".High")
low    <- paste0(tkr, ".Low")
close  <- paste0(tkr, ".Close")

fields <- c(volume, open, high, low, close)
field_rules <- lapply(fields, as.symbol)

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

############################# Technical Analysis Indicators ####################

# Create an environment with all the variables up to this point that can be 
# accessed using column names as symbols
env <- list2env(as.list(TrainingData), parent = .GlobalEnv)

sma <- \(data, n) (TTR::SMA(data, n) |> na.omit())
ema <- \(data, n) (TTR::EMA(data, n) |> na.omit())
dema <- \(data, n, v) (TTR::DEMA(data, n, v) |> na.omit())
evwma <- \(data, v, n) (TTR::EVWMA(data, v, n) |> na.omit())

# Gaussian Kernel

# KDE framework, based on Lo, Mamansky & Wang (2000)
k_regress <- \(d, h, k, y) {
  m <- \(d, h, k, y, i) {
    x <- d[i]
    v <- k((d - x) / h) # Scaled Kernel Regressor
    g <- sum(v)
    w <- v / g
    sum(w * y)
  }
  result <- rep(NA_real_, NROW(d))
  for (i in 2:NROW(d)) {
    result[i] <- m(d[1:i], h, k, y, i)
  }
  as.xts(result, order.by = index(d))
}

comp <- \(x,y) x > y

indicator_rules <- list(expr        = grule(compare(aspect, transform)),
                        compare     = grule(comp),
                        transform   = grule(op(transform, transform),
                                            op(transform, indic),
                                            # func(transform),
                                            # func(indic),
                                            op(transform, aspect),
                                            indic
                                            # k_regress(aspect, const, kernel, regress_f)
                                            ),
                        # regress_f   = grule(func(aspect), op(regress_f, regress_f)),
                        # kernel      = grule(dnorm),
                        # func        = grule(p_sin, p_cos, p_log, sinh, cosh),
                        op          = grule('+', '-', '*'),
                        indic       = grule(sma(aspect, const),
                                            ema(aspect, const),
                                            dema(aspect, const, dema_v)
                                            ),
                        aspect      = do.call(grule, field_rules),
                        dema_v      = gvrule(seq(0,0.9,by = 0.1)),
                        const       = gvrule(1:200)
                      )

forecasting_grammar <- CreateGrammar(indicator_rules)

######################## Fitness Function and GP

apply_expression <- \(result, data) {
  browser()
  idx             <- index(result)
  next_day        <- data[idx] # Force the training data to align with result
  forecast_trend  <- sign(diff(result)) |> na.omit()
  actual_trend    <- sign(diff(data))   |> na.omit()
  if(abs(sum(forecast_trend)) == NROW(forecast_trend)) return(Inf)
  misses          <- forecast_trend != actual_trend
  n_trades        <- sum(diff(forecast_trend) != 0, na.rm = TRUE)
  # Multiply by 3 since tanh is meaningfully in (-3,3) and the penalties are in [0,1]
  # recall gramevol minimises cost
  l <- length(result)
  3 * (sum(misses) / l + n_trades / l)
}

# Lag target by 1. Since the signal will be generated on close, it cannot be
# used until the next day.
training_close  <- TrainingData[, close]
next_day        <- stats::lag(training_close, 1) |> diff() |> na.omit()

indicator_fit <- \(expr) {
  result <- eval(expr, envir = env) |> na.omit() 
  if (is.null(result) || any(!is.finite(result)) || length(result) <= 1) # Sometimes all that is returned is a scalar, but we don't want those
  {
    return(Inf)
  }
  else {
    # if (sd(result, na.rm = TRUE) < 1e-6) return(Inf) # Don't allow very stable results through - they will just copy the asset
    cost <- apply_expression(result, next_day)
    if (is.na(cost)) browser()
    cost
  }
}


# Run
ge <- GrammaticalEvolution(forecasting_grammar,
                           indicator_fit,
                           terminationCost = 0.05,
                           verbose = TRUE,
                           iterations = 100,
                           max.depth = 5)

# Evaluation
ForecastingModel  <- ge$best$expressions[1]
result            <- with(TestingData, eval(ForecastingModel))
error             <- apply_expression(result, TestingData[, close])

# Trading strategy, assuming we blindly apply:
#
# LONG if result is positive
# SHORT otherwise
test_set      <- stats::lag(TestingData[, close], 1) |> diff() |> na.omit()
common_dates  <- intersect(index(result), index(test_set))
result        <- result[common_dates]
test_set      <- test_set[common_dates]
signal        <- (sign(result) * test_set) |> cumsum()
plot(signal)
lines(cumsum(test_set), col = 2)
print(ForecastingModel)

long_only_signal <- cumsum(test_set)
model_signal <- cumsum(sign(result) * test_set)
no_shorts_signal <- cumsum(ifelse(sign(result) > 0, 1, 0) * test_set)


################ Shuffle Test - from ChatGPT

sig <- sign(zoo::na.fill(stats::lag(result, 1), 0))

shuffled_test <- xts::xts(
  sample(as.numeric(test_set)),
  order.by = index(test_set)
)

actual_pnl <- sig * test_set
shuffled_pnl <- sig * shuffled_test

print(head(cbind(
  Actual = as.numeric(test_set),
  Shuffled = as.numeric(shuffled_test),
  Actual.PnL = as.numeric(actual_pnl),
  Shuffled.PnL = as.numeric(shuffled_pnl)
)))

plot(cumsum(actual_pnl), col = "black")
lines(cumsum(shuffled_pnl), col = "blue")
lines(cumsum(test_set), col = "red")

tail(cumsum(actual_pnl), 1)
tail(cumsum(shuffled_pnl), 1)
tail(cumsum(test_set), 1)
