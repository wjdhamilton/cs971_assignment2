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

tkr <- "AAPL"

# Download into a named list (preserves order)
FullDataXTS <- getSymbols(tkr, src = "yahoo",
                          from = data_start,
                          to   = data_end,
                          auto.assign = FALSE) |> na.omit()

halfway       <- mean(as.Date(c(data_start, data_end)))
training_end  <- paste0("/", halfway)
testing_start <- paste0(as.Date(halfway) + 1, "/")

########## Change '-' in ticker names to '.' because R has a delightful habit of
########## unexpectedly sanitising the column names so they are compatible as symbols
tkr <- make.names(tkr)
colnames(FullDataXTS) <- make.names(colnames(FullDataXTS), unique = TRUE)

### Forecasting Engine #################

make_field <- \(f) paste0(tkr, ".", f)
volume <- make_field("Volume")
open   <- make_field("Open")
high   <- make_field("High")
low    <- make_field("Low")
close  <- make_field("Close")

price_aspects <- c(open, high, low, close)
price_rules <- lapply(price_aspects, as.symbol)

# Sometimes vol is 0 in commodity indexes, this gets converted into -Inf which
# will poison the entire result. Get rid of it here
vol_bad <- !is.finite(FullDataXTS[, volume])
FullDataXTS[vol_bad, volume] <- NA
FullDataXTS[, volume] <- zoo::na.approx(FullDataXTS[, volume], na.rm = FALSE)
FullDataXTS[, volume] <- zoo::na.locf(FullDataXTS[, volume], na.rm = FALSE)
FullDataXTS[, volume] <- zoo::na.locf(FullDataXTS[, volume], fromLast = TRUE)

TrainingData <- FullDataXTS[training_end]
TestingData  <- FullDataXTS[testing_start]

# These functions wrap functions that can return NA and return 0 instead. Using
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

# To guard against inappropriate parameters or meaningless values such as NULL
# poisoning the GP process, indicators return either a valid series or an empty
# one that can be handled by other (particularly the comp) function.

mk_guarded_indic <- \(f) {
  function(data) {
    tryCatch(f(data), error = \(e) return(data[0]))
  }
}

clv <- mk_guarded_indic(TTR::CLV)

# Guard for indicators that take a price and 1 extra param
mk_guarded_indic_1 <- \(f) {
  function(data, n) {
    tryCatch(f(data, n), error = \(e) return(data[0]))
  }
}

sma   <- mk_guarded_indic_1(TTR::SMA)
ema   <- mk_guarded_indic_1(TTR::EMA)
hma   <- mk_guarded_indic_1(TTR::HMA)
obv   <- mk_guarded_indic_1(TTR::OBV)
vhf   <- mk_guarded_indic_1(TTR::VHF)
cmo   <- mk_guarded_indic_1(TTR::CMO)

mk_guarded_indic_2 <- \(f) {
  function(data, a, b) {
    tryCatch(f(data, a, b), error = \(e) { return(data[0])}
    )
  }
}

# MFI ignored since it requires a merged HLC series
# SNR ignored for same reason
# and:
# EMV, 
# SAR ignored because it requires a merged HL series
evwma  <- mk_guarded_indic_2(TTR::EVWMA)
vwap   <- mk_guarded_indic_2(TTR::VWAP)
cmf    <- mk_guarded_indic_2(TTR::CMF)
rsi    <- mk_guarded_indic_2(TTR::RSI)

# Although ZLEMA has a third ratio parameter, it is overridden by its second, 
# the lag which is what will be focussed on
zlema <- mk_guarded_indic_1(TTR::ZLEMA)

mk_guarded_indic_3 <- \(f) {
  function(data, a, b, c) {
    tryCatch(f(data, a, b, c), error = \(e) return(data[0]))
  }
}

alma <- mk_guarded_indic_3(TTR::ALMA)
dema  <- mk_guarded_indic_3(TTR::DEMA)

# MACD requires its own wrapper since it returns multiple columns
macd <- function(data, a, b, c, d) {
  tryCatch({
    TTR::MACD(data, a, b, c, maType = d)[, "macd"]
  }, error = function(e) data[0])
}

g_tail <- \(data, n) {
  if (n >= NROW(data)) {
    data[0]
  }
  tail(data, n)
}

# Grouped aspects for various indicators that require them
high_low_close <- as.call(c(
                            as.name("merge"),
                            lapply(c(high, low, close), as.name)
                            ))

comp <- function(aspect, strategy) {
  tryCatch({
    if(class(aspect)[1] == "integer") {
      signal <- xts(rep(0L, NROW(strategy)), order.by = index(strategy))
      signal[aspect > strategy] <-  1L
      signal[aspect < strategy] <- -1L
      return(signal)
    } else {
    ab <- merge(aspect, strategy, join = "left")
    # Flag rows with NA in them. This usually refers to the lag period for 
    # indicators
    ok <- is.finite(ab[, 1]) & is.finite(ab[, 2])
    signal <- xts(rep(0L, NROW(ab)), order.by = index(ab))
    # Go long
    signal[ok & ab[,1] > ab[,2]] <-  1L
    # Go short
    signal[ok & ab[,1] < ab[,2]] <- -1L
    # The series now has 1 or -1 for the signal, and 0 if the row is invalid
    signal
    }
  }, error = \(e) {
    browser()
    return(aspect[0])
  }
  )
}

indicator_rules <- list(expr        = grule(compare(aspect, signal),
                                            compare(const, signal ), # necessary for MACD etc
                                            compare(signal, signal ) # necessary for MACD etc
                                            ),
                        compare     = grule(comp),
                        signal      = grule(op(signal, indic),
                                            op(signal, aspect),
                                            indic
                                            ),
                        op          = grule('+', '-', '*', '/'),
                        indic       = grule(sma(aspect, lag), # Trend
                                            ema(aspect, lag), # Trend
                                            hma(aspect, lag), # Trend
                                            vhf(aspect, lag), # Trend
                                            dema(aspect, lag, ratio, bool), # Trend
                                            zlema(aspect, lag), # Trend
                                            evwma(aspect, vol, lag), # Trend
                                            vwap(aspect, vol, lag), #Volume
                                            obv(aspect, vol), # Volume
                                            cmf(hlc, vol, lag), #Volume
                                            cmo(aspect, lag), #Momentum
                                            rsi(aspect, lag, maType),#Momentum
                                            macd(aspect, lag, lag, lag, maType),
                                            clv(hlc) # Hard to categorise; "Pressure"
                                            ),
                        aspect      = do.call(grule, price_rules),
                        ratio       = gvrule(seq(0.1, 1.0, by = 0.1)),
                        # These are commonly used window sizes in technical analysis
                        lag         = gvrule(c(2L, 3L, 5L, 10L, 14L, 20L, 50L, 100L, 200L)),
                        const       = gvrule(1:50),
                        bool        = grule(TRUE, FALSE),
                        vol         = do.call(grule, list(as.symbol(volume))),
                        hlc         = do.call(grule, list(high_low_close)),
                        # Can't use WMA here
                        maType      = grule("EMA", "SMA", "DEMA", "ZLEMA", "HMA")
                      )

forecasting_grammar <- CreateGrammar(indicator_rules)

g_roll <- \(data, n, f) zoo::rollapply(data, n, f)

# indicator_components <- list(expr       = grule(compare(left_side, right_side)),
#                              compare    = grule(comp),
#                              left_side  = grule(aspect, series),
#                              right_side = grule(series),
#                              scalar     = grule(op(scalar, scalar), 
#                                                 reducer(series),
#                                                 const
#                                                 ),
#                              reducer    = grule(sum, mean, sd),
#                              series     = grule(aspect,
#                                                 g_roll(aspect, const, reducer),
#                                                 op(series, series),
#                                                 ema(series, const)
#                                                 ),
#                              op         = grule('+', '-', '*', '/'),
#                              aspect     = do.call(grule, price_rules),
#                              const      = gvrule(2:200)
#                              )

# forecasting_grammar <- CreateGrammar(indicator_components)

######################## Fitness Function and GP

# Create an environment with all the variables up to this point that can be 
# accessed using column names as symbols
env <- list2env(as.list(TrainingData), parent = .GlobalEnv)

target_trade_rate <- 1/21

assess_strat <- \(result, data) {
  stopifnot(is.xts(result) && is.xts(data))
  stopifnot(NROW(result) == NROW(data))
  next_day        <- stats::lag(data, -1)  |> na.omit()
  idx             <- index(result)
  next_day        <- next_day[idx] # Force the training data to align with result
  actual_returns  <- diff(log(next_day))       |> na.omit()
  actual_trend    <- sign(actual_returns)
  forecast        <- sign(result) # Just in case we need to preprocess result in future versions
  misses          <- sum(forecast != actual_trend)
  n_trades        <- sum(diff(forecast) != 0, na.rm = TRUE)
  # Remove low-trading strategies
  if (n_trades <= (NROW(data) * target_trade_rate)) return(Inf)
  buy_and_hold    <- sum(actual_returns)
  forecast        <- forecast[index(actual_returns)]
  return_path     <- as.numeric(forecast) * as.numeric(actual_returns)
  strat_return    <- sum(return_path, na.rm = TRUE)
  trade_rate      <- n_trades / length(result)
  trade_penalty   <- ((trade_rate - target_trade_rate) / target_trade_rate)^2
  fee             <- 0.01
  cost            <- (buy_and_hold - strat_return) 
                     + n_trades * fee 
                     + trade_penalty
                     + misses/NROW(actual_trend)
  if (!is.finite(cost) || is.na(cost)) {
    Inf
  } else {
  cost
  }
}

# Lag target by 1. Since the signal will be generated on close, it cannot be
# used until the next day.
training_close  <- TrainingData[, close]

# Written by ChatGPT - the list of corner cases was beyond my experience
validate_result <- function(result, training) {
  if (length(result) == 0L) return(FALSE) # Complement of guard in EMA
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
    cost <- assess_strat(result, training_close)
    if(length(cost) > 1) browser()
    cost
  }
}

monitor <- data.frame(
  iteration = integer(),
  best_cost = numeric(),
  finite_mean = numeric(),
  unique_costs = integer()
)

monitor_func <- function(result) {
  iteration <- result$population$currentIteration
  best      <- paste(deparse(result$best$expressions[1]), collapse = " ")
  best_cost <- result$best$cost
  evals <- result$population$evaluations
  finite_evals <- evals[is.finite(evals)]
  finite_mean   <- if (length(finite_evals) > 0) {
    mean(finite_evals)
  }
  uniques <- length(unique(evals))
  monitor <<- rbind(
                    monitor,
                    data.frame(
                               iteration = iteration,
                               best_cost = best_cost,
                               finite_mean = finite_mean,
                               unique_costs = uniques)
  )
}


ge <- GrammaticalEvolution(forecasting_grammar,
                           indicator_fit,
                           terminationCost = -Inf,
                           monitorFunc = monitor_func,
                           iterations = 1000,
                           max.depth = 10)

e_to_s <- \(ex) paste(deparse(ex), collapse = " ")


shuffle <- \(data) {
  xts::xts(
           sample(as.numeric(data)),
           order.by = index(data)
  )
}

e_to_s <- \(ex) paste(deparse(ex), collapse = " ")

forecasting_model  <- ge$best$expressions[1]

train_signal       <- with(TrainingData, eval(forecasting_model))
cat("Model: ", e_to_s(forecasting_model))

train_close   <- TrainingData[, close]
next_day      <- stats::lag(train_close, -1) |> log() |> diff() |> na.omit()
train_return  <- cumsum(train_signal * next_day)

max_test <- 1000

shuffle_train <- replicate(max_test, sum(train_signal * shuffle(next_day)))
shuffle_mean <- mean(shuffle_train)
shuffle_sd   <- sd(shuffle_train)
prob_ret     <- pnorm(shuffle_mean, shuffle_sd, lower.tail = FALSE)

plot(train_return)
lines(train_close, col = 2, on = 1)

hist(shuffle_train, prob = TRUE)
lines(density(shuffle_train))
abline(v = prob_ret, col = 2)

# Test Evaluation
test_signal        <- with(TestingData, eval(forecasting_model))

test_close    <- TestingData[, close]
test_set      <- stats::lag(test_close, -1) |> log() |> diff() |> na.omit()
test_ret      <- (test_signal * test_set)

plot(cumsum(test_ret))
lines(cumsum(test_set), col = 2)
print(forecasting_model)

shuffle_test  <- replicate(max_test, sum(train_signal * shuffle(next_day)))

shuffle_mean  <- mean(shuffle_test)
shuffle_sd    <- sd(shuffle_test)
actual_ret    <- sum(test_ret)

p_value <- pnorm(actual_ret, shuffle_mean, shuffle_sd, lower.tail = FALSE)
print(p_value)

hist(shuffle_test, prob = TRUE)
lines(density(shuffle_test))
abline(v = actual_ret, col = 2)
