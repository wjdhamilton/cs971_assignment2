library(quantmod)
library(GA)

berk_s <- "BRK-B"

getSymbols("BRK-B", src = "yahoo")

berk_s <- get(berk_s)

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
