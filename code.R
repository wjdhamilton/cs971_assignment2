library(quantmod)
library(GA)
library(TTR)

berk_s <- "BRK-B"

getSymbols("BRK-B", src = "yahoo")

berk_s <- get(berk_s)

closes <- berk_s$`BRK-B.Close`

# Market server
# This simulates a stream of incoming data from the market. It works through a
# callback function that gives the main application access to the dataset as
# though it were coming from a market server.

market <- function(listener) {
  for (i in closes) {
    listener(i)
  }
}

# Signal
# This function takes market data and generates a signal which it broadcasts to
# the risk engine
mk_signal <- function(notify_risk_engine) {
  # Store for price data (can be changed)
  prices <- numeric(0)

  calc_signal <- function(prices) {
    if (length(prices) < 15) {
      NULL
    } else {
      tail(RSI(as.numeric(prices)), 1)
    }
  }

  # This is the part that the market communicates with. It's a closure, which 
  # means that the mk_signal function enclosing it is its environment and 
  # information can be saved into the mk_signal environment with the <<- operator.
  function(price) {
    price <- as.numeric(price)
    if (length(prices) < 15) {
      prices <<- c(prices, price)
    } else {
      prices <<- c(prices[-1], price)
    }
    signal <- calc_signal(prices)
    notify_risk_engine(signal)
  }
}

# Same idea as above, the risk engine is another closure which will keep a track
# of the risk engine's state. TODO: needs to know the market price as well in 
# order to calculate running profits and decide whether or not to open / close
# positions
risk_engine <- function(executor) {
  trades <- c()
  account <- 100 # Dummy value for MVP
  function(signal) {
    # do something with the executor to check price of trade and then execute
    # trade if OK.
    cat(signal, "\r")
  }
}

trade_executor <- function() {

  function(instruction) {
    0.02 # dummy cost
  }
}

risk_engine(NULL) |> mk_signal() |> market()


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
