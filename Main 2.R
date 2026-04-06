set.seed(1000000)
# =========================
# Genetic Programming Price Forecaster in R
# Predicts n steps ahead using EWMA features
# =========================
library(quantmod)
library(gramEvol)
library(xts)
library(zoo)
library(TTR)

# -------------------------
# 1. User settings
# -------------------------
ticker      <- "BRK-B"     
data_start  <- "2000-01-01"
data_end    <- "2026-01-01"

n_ahead     <- 1            # forecast horizon: predict t + n_ahead

# EWMA lengths to use as inputs
ema_lengths <- c(2, 3, 5, 8, 10, 15, 21)

# GP / GE settings
pop_size    <- 200
num_iter    <- 150
max_depth   <- 5
stop_rmse   <- 1e-6

# -------------------------
# 2. Download data
# -------------------------
FullDataXTS <- getSymbols(ticker, src = "yahoo",
                          from = data_start,
                          to   = data_end,
                          auto.assign = FALSE)

# Drop Adjusted close
FullDataXTS <- FullDataXTS[, -ncol(FullDataXTS)]

# -------------------------
# 3. Build supervised dataset
#    Features: EWMA columns of OHLCV
#    Target:   Close shifted -n_ahead
# -------------------------
prefixes  <- c("Open", "High", "Low", "Close", "Volume")
var_names <- character(0)

# Start with empty xts object indexed like the raw data
feature_data <- xts(order.by = index(FullDataXTS))

# Log-transform volume first
FullDataXTS[, "BRK-B.Volume"] <- log(FullDataXTS[, "BRK-B.Volume"])

for (f in prefixes) {
  column <- grep(paste0("\\.", f, "$"), colnames(FullDataXTS), value = TRUE)
  
  for (L in ema_lengths) {
    label <- paste0(f, "_EMA_", L)
    var_names <- c(var_names, label)
    
    ema_col <- EMA(FullDataXTS[, column], n = L)
    colnames(ema_col) <- label
    
    feature_data <- merge(feature_data, ema_col)
  }
}

# Target = future close price at t + n_ahead
target <- lag(FullDataXTS$`BRK-B.Close`, n = -n_ahead)
colnames(target) <- "Target"

# Combine features and target
lagged_data <- merge(feature_data, target)

# Remove rows with NA values caused by EMA warm-up and forward target shift
lagged_data <- na.omit(lagged_data)
# -------------------------
# 4. Train / test split
# -------------------------
TrainingData <- lagged_data["/2020-12-31"]
TestingData  <- lagged_data["2021-01-01/"]

var_rules <- lapply(var_names, as.symbol)
env <- list2env(as.list(TrainingData), parent = .GlobalEnv)

cat("Training rows:", nrow(TrainingData), "\n")
cat("Testing rows: ", nrow(TestingData), "\n")

# -------------------------
# 5. Safe functions for GP
#    These reduce blow-ups from division/log/exp
# -------------------------
safe_div <- function(a, b) {
  ifelse(abs(b) < 1e-8, a, a / b)
}

safe_log <- function(x) {
  log(abs(x) + 1e-8)
}

safe_sqrt <- function(x) {
  sqrt(abs(x))
}

safe_exp <- function(x) {
  exp(pmin(x, 20))   # cap exponent to avoid overflow
}

# -------------------------
# 6. Fitness function
#    Minimise RMSE on training data
# -------------------------
fitness_fun <- function(expr) {
  pred <- try(with(lagged_data, eval(expr)), silent = TRUE)
  
  if (inherits(pred, "try-error")) return(Inf)
  if (length(pred) != nrow(lagged_data)) return(Inf)
  if (any(!is.finite(pred))) return(Inf)
  
  rmse <- sqrt(mean((lagged_data$Target - pred)^2))
  return(rmse)
}

forecastingfitnessRMSE <- function(expr) {
  result <- eval(expr, envir = env)
  if (any(is.nan(result)))
    Inf
  else
    sqrt(mean((TrainingData$BRK.B.Close - result)^2))
}
# -------------------------
# 7. Grammar
#    Variables are the lagged prices
# -------------------------
grammarRules <- list(
  expr = grule(
    op(expr, expr),
    func(expr),
    var,
    data
  ),
  func = grule(
    sin,
    cos,
    safe_log,
    safe_sqrt,
    safe_exp
  ),
  op = grule(
    `+`,
    `-`,
    `*`,
    safe_div
  ),
  reducer    = grule(rowMeans),
  lists      = grule(merge(data)),
  data       = do.call(grule, lapply(var_names, as.name)),
  var        = gvrule(1:200)
)

grammar <- CreateGrammar(grammarRules)
# -------------------------
# 8. Train genetic programme
# -------------------------
gp_model <- GrammaticalEvolution(
  grammarDef      = grammar,
  evalFunc        = fitness_fun,
  iterations      = num_iter,
  popSize         = pop_size,
  max.depth       = max_depth,
  terminationCost = stop_rmse,
  verbose         = TRUE
)

cat("\nBest evolved expression:\n")
print(gp_model$best$expression)

cat("\nTraining RMSE of best expression:\n")
print(gp_model$best$cost)
# -------------------------
# 9. Predict on train and test
# -------------------------
best_expr <- gp_model$best$expression

train_pred <- with(TrainingData, eval(best_expr))
colnames(train_pred) <- 'preds'
test_pred  <- with(TestingData,  eval(best_expr))
colnames(test_pred) <- 'preds'
# -------------------------
# 10. Accuracy metrics
# -------------------------
rmse <- function(actual, pred) sqrt(mean((actual - pred)^2))
mae  <- function(actual, pred) mean(abs(actual - pred))

train_rmse <- rmse(lagged_data$Target, train_pred)
train_mae  <- mae(lagged_data$Target, train_pred)

test_rmse  <- rmse(lagged_data$Target, test_pred)
test_mae   <- mae(lagged_data$Target, test_pred)

cat("\n===== Performance =====\n")
cat("Train RMSE:", train_rmse, "\n")
cat("Train MAE: ", train_mae, "\n")
cat("Test RMSE: ", test_rmse, "\n")
cat("Test MAE:  ", test_mae, "\n")

# -------------------------
# 11. Store results
# -------------------------
results_train <- data.frame(
  Date   <- index(TrainingData),
  Actual <- TrainingData$Target,
  Pred   <- train_pred
)

results_test <- data.frame(
  Date   = index(TestingData),
  Actual = TestingData$Target,
  Pred   = test_pred
)
# -------------------------
# 12. Plot test forecasts
# -------------------------
plot(
  results_test$Date, results_test$Target,
  type = "l", lwd = 2,
  main = paste(ticker, "-", n_ahead, "Step Ahead GP Forecast"),
  xlab = "Date", ylab = "Price"
)
lines(results_test$Date, results_test$Pred, lwd = 2, lty = 2)
legend(
  "topleft",
  legend = c("Actual", "Predicted"),
  lty = c(1, 2),
  lwd = 2,
  bty = "n"
)

# -------------------------
# 13. Optional: inspect first few forecasts
# -------------------------
head(results_test, 10)

