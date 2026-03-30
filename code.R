library(quantmod)
library(GA)
library(TTR)

berk_s <- "BRK-B"
getSymbols("BRK-B", src = "yahoo")
berk_s <- get(berk_s)
closes <- berk_s$`BRK-B.Close`

# Fitness functions
# ------------------------------------------------------------------------------
# These ought to be combined into a single function for the purposes of 
# optimisation.

# Fitness for trade execution
library(PerformanceAnalytics) # for the Sortino Ratio

trade_penalty <- \(n) n/(n+1)
drawdown <- \(trades) NaN
mean_return <- \(trades) NaN

risk_engine_f <- function(alpha, beta, gamma, delta, trades) {
  alpha * SortinoRatio(trades, min_return) + beta * trade_penalty - gamma * drawdown + delta * mean_return
}

# Market server
# This simulates a stream of incoming data from the market. It works through a
# callback function that gives the main application access to the dataset as
# though it were coming from a market server.

market <- function(listener) {
  for (i in closes) {
    listener(i)
  }
}


mk_signal <- function(close, signal, time) {
  list(close = close, signal = signal, time = time)
}

# Signal
# This function takes market data and generates a signal which it broadcasts to
# the risk engine
forecaster <- function(calc_forecast, notify_risk_engine) {
  # Store for price data (can be changed)
  prices <- numeric(0)

  # This is the part that the market communicates with. It's a closure, which
  # means that the forecaster function enclosing it is its environment and
  # information can be saved into the forecaster environment with the <<- operator.
  function(ohlcv) {
    prices <- 1 # TODO need to append a row to an xts object
    # TODO need a function that converts prices to an object that forecast expects
    f <- with(prices, calc_forecast) # Assuming that prices is an xts object of the same shape and format as that which the expression expects
    signal <- mk_signal(signal = f, time = t)
    notify_risk_engine(signal)
  }
}

# A trade object, which is a record of how much, at what price, and when.
# Negative units indicate a sale, positive a buy. Charges are updated by the
# executor
make_trade <- function(price, units, time) {
  list(price = price, units = units, time = time, charge = NULL)
}

# Same idea as above, the risk engine is another closure which will keep a track
# of the risk engine's state. TODO: needs to know the market price as well in
# order to calculate running profits and decide whether or not to open / close
# positions
risk_engine <- function(executor) {
  trades <- c()
  account <- 100 # Dummy value for MVP

  assess_signal <- \(signal) {
    "BUY"
  }

  assess_trade <- \(signal, trade) {
    "HOLD"
  }

  function(signal) {
    action <- assess_signal(signal)
    if (action == "LONG") {
      go_long
    } else if (action == "SHORT") {
      go_short
    }

    for (t in trades) {
      action <- assess_trade(t)
      if (action == "CLOSE") {
        cl <- close_trade(t)
        
      }
    }

    # do something with the executor to check price of trade and then execute
    # trade if OK.
    trade <- make_trade(signal$price, 1, signal$time)
    trades <- c(trades, trade)
    cat(str(executor(trade)))
  }
}

trade_executor <- function() {
  function(instruction) {
    instruction$charge <- 0.02
    instruction
  }
}

trade_executor() |> risk_engine() |> forecaster() |> market()


# Forecasting
# We want to create a system that indicates whether or not the stock is likely
# rise or fall. Whether this is a direct forecast of the stock price or derived
# from an indicator, the implication is the same. So, for compatibility with the risk
# engine, the forecaster should give a signal about the forecast direction of the
# stock for the next day with some representation of the strength of the change
# and / or the strength of its conviction in the move.

# Risk engine
# Objective: to protect the account by minimising the drawdown and ensuring 
# that the portfolio of positions are non-correlating (how??)
# Size positions to reflect the risk of the trade: Given a forecasting signal,
# and the current portfolio, should the system act on that signal and how large
# should the response (in terms of units purchased or proportion of the account)
# be?

# Trade execution
# Can't really do much here withot an exchange. Could probably just pop it in as
# a nod to the idea that a real system would need one.
