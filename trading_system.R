# Assume that technical_indicators has already run. 
library(quantmod)

strategy <- ForecastingModel

data_set <- TestingData

closed_trades <- list()
# Any strategy will require more than 1 data point, so taking the first one at
# this point isn't going to change anything but make accumulating the historical
# data with xts.rbind a lot easier
past_data     <- data_set[1] 

# Construct a trade object. The close_price should never be empty on a 
# closed trade; close_price == NULL represents a program failure
mk_trade <- \(direction, open_price) {
  list(direction = direction, open = open_price, close_price = NULL)
}

open_position <- mk_trade(direction = 0, open_price = 0)

for (i in 2:NROW(data_set)) {
  # Any real-world strategy must generate a signal using yesterday's data
  model <- tryCatch(
          with(past_data, eval(strategy)),
           error = function(e) {
             if (grepl("outside valid range", e$message)) {
               NULL
             } else {
               # have a fit
               stop(e)
             }
           }
  )
  # Signals on early data are likely to produce NA since the strategy will probably
  # use moving averages. Skip these, but keep adding to past_data
  latest_data <- data_set[i]
  # Add today's close to the past_data series
  past_data <-rbind.xts(past_data,latest_data)
  if (is.null(model)) { next }
  # Latest signal is the last signal in the model
  signal <- as.numeric(last(model))
  # Do some trading
  current_direction <- open_position$direction
  latest_close <- as.numeric(latest_data[, close])
  if(current_direction != signal){
    #Close the current position
    open_position$close_price <- latest_close
    closed_trades <- c(closed_trades, list(open_position))
    # Open a new position
    open_position <- mk_trade(direction = signal, open_price = latest_close)
  } 
  # If we've reached the end of the dataset, just close the final trade
  if(i == NROW(data_set)) {
    open_position$close_price <- latest_close
    closed_trades <- c(closed_trades, list(open_position))
  }
}

trade_profit <- \(trade) trade$direction * (trade$close_price - trade$open)

total_profit <- lapply(closed_trades, trade_profit) |> as.numeric() |> sum()

print(total_profit)
