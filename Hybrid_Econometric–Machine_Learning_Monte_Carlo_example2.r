# =============================================
# Hybrid Econometric–Machine Learning Monte Carlo
# Tri-variate Simulation: Oil, Carbon, Electricity
# =============================================

library(forecast)
library(randomForest)
library(MASS)
library(ggplot2)
library(copula)

set.seed(42)

# -------------------------------
# 1. Simulate historical data
# -------------------------------
n <- 200
time <- 1:n
oil_price <- 60 + cumsum(rnorm(n, 0.2, 1.5))
carbon_price <- 35 + 0.3 * oil_price + rnorm(n, 0, 2)
electricity_price <- 50 + 0.4 * oil_price + 0.6 * carbon_price + rnorm(n, 0, 3)

data <- data.frame(time, oil_price, carbon_price, electricity_price)

# -------------------------------
# 2. Econometric Models (ARIMA)
# -------------------------------
fit_oil <- auto.arima(data$oil_price)
fit_carbon <- auto.arima(data$carbon_price)
fit_elec <- auto.arima(data$electricity_price)

h <- 12  # Forecast horizon (months)
eco_forecasts <- list(
  oil = forecast(fit_oil, h = h),
  carbon = forecast(fit_carbon, h = h),
  elec = forecast(fit_elec, h = h)
)

eco_means <- data.frame(
  oil = as.numeric(eco_forecasts$oil$mean),
  carbon = as.numeric(eco_forecasts$carbon$mean),
  elec = as.numeric(eco_forecasts$elec$mean)
)

# -------------------------------
# 3. Machine Learning Residual Models (Random Forest)
# -------------------------------
train_rf <- function(dep, indep1, indep2) {
  model <- randomForest(dep ~ indep1 + indep2, ntree = 500)
  return(model)
}

rf_oil <- train_rf(residuals(fit_oil),
                   lag(data$carbon_price, -1)[1:(n-1)],
                   lag(data$electricity_price, -1)[1:(n-1)])

rf_carbon <- train_rf(residuals(fit_carbon),
                      lag(data$oil_price, -1)[1:(n-1)],
                      lag(data$electricity_price, -1)[1:(n-1)])

rf_elec <- train_rf(residuals(fit_elec),
                    lag(data$oil_price, -1)[1:(n-1)],
                    lag(data$carbon_price, -1)[1:(n-1)])

# Generate ML-based residual corrections for forecast horizon
future_oil <- seq(tail(data$oil_price, 1), by = 0.3, length.out = h)
future_carbon <- seq(tail(data$carbon_price, 1), by = 0.2, length.out = h)
future_elec <- seq(tail(data$electricity_price, 1), by = 0.4, length.out = h)

ml_resid <- data.frame(
  oil = predict(rf_oil, data.frame(indep1 = future_carbon, indep2 = future_elec)),
  carbon = predict(rf_carbon, data.frame(indep1 = future_oil, indep2 = future_elec)),
  elec = predict(rf_elec, data.frame(indep1 = future_oil, indep2 = future_carbon))
)

# -------------------------------
# 4. Monte Carlo Simulation (Copula-based)
# -------------------------------
n_sims <- 2000

# Estimate correlation matrix from historical returns
returns <- data.frame(
  oil = diff(log(data$oil_price)),
  carbon = diff(log(data$carbon_price)),
  elec = diff(log(data$electricity_price))
)
corr_mat <- cor(na.omit(returns))

# Gaussian copula for correlated draws
cop <- normalCopula(param = P2p(corr_mat), dim = 3, dispstr = "un")
u <- rCopula(n_sims, cop)

# Transform to normal residual shocks
shocks <- qnorm(u)

# Scale shocks to variable-specific volatilities
vols <- apply(returns, 2, sd)
shocks_scaled <- t(t(shocks) * vols)

# -------------------------------
# 5. Generate Monte Carlo Forecasts
# -------------------------------
mc_results <- list(oil = matrix(NA, h, n_sims),
                   carbon = matrix(NA, h, n_sims),
                   elec = matrix(NA, h, n_sims))

for (i in 1:n_sims) {
  mc_results$oil[, i] <- eco_means$oil + ml_resid$oil + shocks_scaled[i, 1]
  mc_results$carbon[, i] <- eco_means$carbon + ml_resid$carbon + shocks_scaled[i, 2]
  mc_results$elec[, i] <- eco_means$elec + ml_resid$elec + shocks_scaled[i, 3]
}

# -------------------------------
# 6. Visualization
# -------------------------------
plot_mc <- function(mc_matrix, title) {
  df <- data.frame(
    Month = rep(1:h, n_sims),
    Price = as.vector(mc_matrix)
  )
  ggplot(df, aes(x = Month, y = Price)) +
    geom_line(alpha = 0.03, color = "steelblue") +
    stat_summary(fun = mean, geom = "line", color = "red", size = 1.2) +
    labs(title = title, y = "Price", x = "Forecast Horizon (Months)") +
    theme_minimal()
}

p1 <- plot_mc(mc_results$oil, "Monte Carlo Forecast: Oil Price (USD/barrel)")
p2 <- plot_mc(mc_results$carbon, "Monte Carlo Forecast: Carbon Price (EUR/tCO2)")
p3 <- plot_mc(mc_results$elec, "Monte Carlo Forecast: Electricity Price (EUR/MWh)")
