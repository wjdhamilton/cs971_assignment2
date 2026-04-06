### Buy & Hold Strategy As a benchamrk
Strategy_BuyNHold <- function(prediction_df){
  ### Get the rpices in the test period
  BuyNHold <- prediction_df
  
  # create trade column placing buy in 1st position and sell in the last
  trades <- array(0, dim = c(nrow(BuyNHold), 1))
  trades[1, 1] <- 1
  trades[nrow(BuyNHold), 1] <- -1
  
  BuyNHold <- cbind(BuyNHold, trades)
  colnames(BuyNHold)[ncol(BuyNHold)] <- "trades"
  
  # calculate cumulative return at each time step
  BuyNHold$In_Position <- cumsum(BuyNHold$trades)
  BuyNHold$returns <- c(NA, diff(BuyNHold$Target) / head(BuyNHold$Target, -1))
  BuyNHold$cum_returns <- cumprod(1 + replace(BuyNHold$returns, is.na(BuyNHold$returns), 0))
  
  # Plot strategy Cumulative treturns 
  plot(BuyNHold$Date,BuyNHold$cum_returns, type = 'l')
  
  # Return the prediction dataframe extended with strategy specific columns
  return(BuyNHold)
}

### Naive Buy when returns over x days +ve; Sell when returns overy y days -ve
Strategy_Naive <- function(prediction_df,x=10,y=10){
  ### Get the rpices in the test period
  TradeStrategy <- prediction_df
  
  ### create trade column
  # predicted returns from the predicted price column
  TradeStrategy$pred_return <- c(NA, diff(TradeStrategy$preds) / head(TradeStrategy$preds, -1))
  
  # initialise Trades column
  TradeStrategy$Trades <- 0
  
  ### placing buffers so trades aren't constantly triggered
  buy_buffer <- x
  sell_buffer <- y
  buffer <- min(buy_buffer,sell_buffer)
  
  # looop for building trade signals.
  i <- max(buy_buffer, sell_buffer) + 1
  
  while (i <= nrow(TradeStrategy)) {
    
    if (all(TradeStrategy$pred_return[(i-buy_buffer+1):i] > 0, na.rm = TRUE)) {
      TradeStrategy$Trades[i] <- 1
      i <- i + buffer
    } else if (all(TradeStrategy$pred_return[(i-sell_buffer+1):i] < 0, na.rm = TRUE)) {
      TradeStrategy$Trades[i] <- -1
      i <- i + buffer
    } else {
      TradeStrategy$Trades[i] <- 0
      i <- i + 1
    }
  }
  
  # close all positions on the final day
  TradeStrategy$Trades[nrow(TradeStrategy)] <- -sum(TradeStrategy$Trades[-nrow(TradeStrategy)], na.rm = TRUE)
  
  # calculate cumulative return at each time step
  TradeStrategy$Position <- cumsum(TradeStrategy$Trades)
  
  #idk
  TradeStrategy$AccountValue <- 0
  
  for (i in 1:nrow(TradeStrategy)){
    if(TradeStrategy$Trades[i] == 1){
      TradeStrategy$AccountValue[i] <- -TradeStrategy$Target[i] 
    } else if(TradeStrategy$Trades[i] == -1){
      TradeStrategy$AccountValue[i] <- TradeStrategy$Target[i] 
    }
  }
  TradeStrategy

}

TradingStratN <- Strategy_Naive(results_test,10,10)
plot(cumsum(TradingStratN$AccountValue), type = 'l')

