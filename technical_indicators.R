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

fields <- c(open, high, low, close)
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

comp <- function(a, b) {
  ab <- merge(a, b, join = "left")
  out <- ifelse(ab[, 1] > ab[, 2], 1L, 0L)
  out[is.na(out)] <- 0L
  ab[, 1] <- out
  ab[, 1, drop = FALSE]
}

indicator_rules <- list(expr        = grule(compare(aspect, transform)),
                        compare     = grule(comp),
                        transform   = grule(op(transform, indic),
                                            op(transform, aspect),
                                            indic
                                            ),
                        op          = grule('+', '-', '*'),
                        indic       = grule(sma(aspect, const),
                                            ema(aspect, const),
                                            dema(aspect, const, dema_v)
                                            ),
                        aspect      = do.call(grule, field_rules),
                        dema_v      = gvrule(seq(0,0.9,by = 0.1)),
                        const       = gvrule(2:200),
                        vol         = grule(volume)
                      )

forecasting_grammar <- CreateGrammar(indicator_rules)

######################## Fitness Function and GP

assess_fit <- \(result, data) {
  idx             <- index(result)
  next_day        <- stats::lag(data, 1) |> na.omit()
  next_day        <- next_day[idx] # Force the training data to align with result
  actual_trend    <- sign(diff(next_day))   |> na.omit()
  forecast        <- sign(result)
  misses          <- forecast != actual_trend
  n_trades        <- sum(diff(forecast) != 0, na.rm = TRUE)
  buy_and_hold_return <- sum(next_day)
  strat_return    <- sum(next_day * forecast)
  l <- length(result)
  (sum(misses) / l + n_trades / l + (strat_return - buy_and_hold_return))
}

# Lag target by 1. Since the signal will be generated on close, it cannot be
# used until the next day.
training_close  <- TrainingData[, close]

# Written by ChatGPT - the list of corner cases was beyond my experience
validate_result <- function(result, training) {
  if (is.null(result)) return(FALSE)
  if (!xts::is.xts(result)) return(FALSE)
  if (NROW(result) != NROW(training)) return(FALSE)
  if (NCOL(result) != 1) return(FALSE)
  if (NROW(result) == 0) return(FALSE)
  if (all(is.na(result))) return(FALSE)
  if (any(is.infinite(as.numeric(result)))) return(FALSE)
  s <- sign(as.numeric(result))
  if (!any(s != 0, na.rm = TRUE)) return(FALSE)
  TRUE
}

indicator_fit <- \(expr) {
  result <- eval(expr, envir = env)
  if (!validate_result(result, training_close))
  {
    return(Inf)
  }
  else {
    # if (sd(result, na.rm = TRUE) < 1e-6) return(Inf) # Don't allow very stable results through - they will just copy the asset
    cost <- assess_fit(result, training_close)
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
error             <- assess_fit(result, TestingData[, close])

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
