### Buy & Hold Strategy As a benchamrk
### Get the rpices in the test period
BuyNHold <- results_test

# create trade column
trades <- array(0, dim = c(nrow(BuyNHold), 1))
trades[1, 1] <- 1
trades[nrow(BuyNHold), 1] <- -1

# add as a new column
BuyNHold <- cbind(BuyNHold, trades)
colnames(BuyNHold)[ncol(BuyNHold)] <- "trades"

head(BuyNHold)
tail(BuyNHold)

# calculate cumulative return at each time step
BuyNHold$In_Position <- cumsum(BuyNHold$trades)
BuyNHold$returns <- c(NA, diff(BuyNHold$Target) / head(BuyNHold$Target, -1))
BuyNHold$cum_returns <- cumprod(1 + replace(BuyNHold$returns, is.na(BuyNHold$returns), 0))

plot(BuyNHold$Date,BuyNHold$cum_returns, type = 'l')

