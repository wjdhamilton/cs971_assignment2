### Introduction
- Assest Selection Criteria -
o low sensitivity to news shocks 
( exogenous shock unlearnable by a model. high sensitivity could negatively impact model performance as it attemps to learn from these shocks )
o no derivates ( keeping in simple as derivatives would encourage analysis in to underlying )
o no dividends ( affects price in no systematic way learnable by a model)
o Making dataset stationary with diff(log()) didn't seem to change much. Now thinks Lag_16 is best predictor
o The lagged dataset only works with close, i.e. omits ohl and vol
o Just now, the functions "mean" and "sum" affect the whole Lag. Is Lag 
really equivalent to a moving window?
o Another thing: because the syntax of R applies vector transformations with
the same operators as unitary transformations, `-result` in the fitness function
will work if result is either a vector or a scalar without warning. We don't
really want the latter, and we get it often. 
  o The problem is that the aggregators work on columns, whereas the "memory" of
  the dataset works in lagged rows. So, say you have vectors {Lag_0, Lag_1,
  Lag_2}. If you apply mean to the dataset you get { mean(Lag_0), mean(Lag_1), 
  mean(Lag_2) } which should all be approximately equal since the lags are just
  Lag_0 minus 1 and 2 datapoints respectively
o Since it now works with a single dataframe, we should make all the column
refereces dynamic so changing ticker means just changing its name at the
start

# Activation Function
o The sigmoid function (p_logis) is always positive, so, in this context will 
always return a long / buy and hold position. Removed. 

# Indicators
o BRK-B and AAPL both trend upwards almost constantly. Correct strategy = buy and
hold. Therefore, any function that returns ~ 1 for all values will score well. 

We need less predictable data. => This did not help. The fitness function simply
rewards the most positive overall result, and suspect that it cannot be
two-sided. It either biases long or short, and if the overall trend is long then
long wins. Going to try directional ratio (i.e. indicator's nlong/nbars v
asset's nlong/nbars)

If the GP can just copy the asset, it probably will. Need to remove solutions 
that are either always so large tanh is always positive or are very consistently
positive with the same effect. 

# TODO
o Change length() to NROW() since length counts all the _elements_ in a
collection


# Noteworhy Runs
expression(tanh(High - p_log(exp(ema(Low, 183L))))) - high performance on
Apple and BZ=F
