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
