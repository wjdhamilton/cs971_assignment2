library(quantmod)
library(GA)
library(TTR)

# Fitness functions
# ------------------------------------------------------------------------------
# These ought to be combined into a single function for the purposes of 
# optimisation.

# Fitness for trade execution
library(PerformanceAnalytics) # for the Sortino Ratio

mean_return   <- \(trades)  stop("undefined")
drawdown      <- \(trades)  stop("undefined")
trade_penalty <- \(traders) stop("undefined")
sortino       <- \(min_return, trades) stop("undefined")

risk_engine_f <- function(alpha, beta, gamma, delta, trades) {
  st  <- sortino(trades)
  mr  <- mean_return(trades)
  d   <- drawdown(trades)
  tp  <- trade_penalty(trades)
  alpha * st + beta * mr + gamma * d + delta * tp
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


mk_signal <- function(price, signal, time, profit) {
  list(price = price, signal = signal, time = time, profit = profit)
}

get_close <- \(ohlcv) stop("undefined")

get_timestamp <- \(timestamp) stop("undefined")

# Signal
# This function takes market data and generates a signal which it broadcasts to
# the risk engine
forecaster <- function(notify_risk_engine, calc_signal) {
  # Store for price data (can be changed)
  prices <- numeric(0)


  # This is the part that the market communicates with. It's a closure, which
  # means that the forecaster function enclosing it is its environment and
  # information can be saved into the forecaster environment with the <<- operator.
  function(ohlcv) {
    prices <<- c(prices, ohlcv)
    c <- get_close(ohlcv)
    s <- calc_signal(prices)
    t <- get_timestamp(ohlcv)
    signal <- mk_signal(price = ohlcv, signal = s, time = t)
    notify_risk_engine(signal)
  }
}

# A trade object, which is a record of how much, at what price, and when.
# Negative units indicate a sale, positive a buy. Charges are updated by the
# executor. Trades can be either 'OPEN' or 'Closed'
make_trade <- function(price, units, time) {
  list(price = price, units = units, time = time, charge = NULL, status = "OPEN")
}

# Same idea as above, the risk engine is another closure which will keep a track
# of the risk engine's state. TODO: needs to know the market price as well in
# order to calculate running profits and decide whether or not to open / close
# positions

# Things that can be done with an asset

trades <- c()
account <- 100 # Dummy value for MVP

risk_engine <- function(executor) {

  assess_signal <- \(signal) stop("undefined")
  assess_trade  <- \(trade)  stop("undefined")
  size_position <- \() stop("undefined")

  function(signal) {
    assessment <- assess_signal(signal)
    if (assessment == "BUY") {
      size <- size_position()
      trade <- make_trade(signal$price, size, signal$time)
      price <- executor(trade)
      trades <- c(trades, trade)

    } else if (assessment == "SELL") {
      size <- size_position()
      trade <- make_trade(signal$price, size, signal$time)
      trades <- c(trades, trade)
    }

    # Check if any of the trades need to be changed
    for (t in trades) {
      assessment <- asssess_trade(signal)
      if (assessment == "CLOSE") {
        trade <- make_trade(signal$price, -(t$size), signal$time)
        trade <- exeutor(trade)
        trade$status <- "CLOSED"
        # TODO will this mutation propogate to the trade in the list?
        # TODO how will the system know if the trades are open or not?
      }
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
