rm(list = ls())

# ===============================
# Hybrid Econometric–ML–Monte Carlo Simulation
# ===============================
#Created by chatGPT

# Required libraries
library(forecast)      # For ARIMA econometric modeling
library(randomForest)  # For machine learning residual model
library(MASS)          # For multivariate normal draws
library(ggplot2)

set.seed(123)

# -------------------------------
# 1. Generate synthetic data
# -------------------------------
n <- 200
time <- 1:n
oil_price <- 60 + cumsum(rnorm(n, 0.2, 1.5))          # synthetic oil prices
carbon_price <- 40 + 0.3 * oil_price + rnorm(n, 0, 2) # correlated carbon prices
electricity_price <- 50 + 0.5 * oil_price + 
  0.7 * carbon_price + rnorm(n, 0, 3)

data <- data.frame(time, oil_price, carbon_price, electricity_price)

# -------------------------------
# 2. Step 1: Econometric Model (ARIMA example)
# -------------------------------
fit_eco <- auto.arima(data$electricity_price)
eco_forecast <- forecast(fit_eco, h = 12)
eco_mean <- as.numeric(eco_forecast$mean)
eco_resid <- residuals(fit_eco)

# -------------------------------
# 3. Step 2: Machine Learning Model on Residuals
# -------------------------------
train_data <- data.frame(
  oil = tail(data$oil_price, length(eco_resid)),
  carbon = tail(data$carbon_price, length(eco_resid)),
  resid = eco_resid
)

rf_model <- randomForest(resid ~ oil + carbon, data = train_data, ntree = 500)

# Predict residual correction for next 12 months
future_oil <- seq(tail(data$oil_price, 1), by = 0.5, length.out = 12)
future_carbon <- seq(tail(data$carbon_price, 1), by = 0.3, length.out = 12)
ml_pred <- predict(rf_model, data.frame(oil = future_oil, carbon = future_carbon))

# -------------------------------
# 4. Step 3: Monte Carlo Simulation around ML-corrected forecast
# -------------------------------
sigma <- sd(eco_resid)
n_sims <- 1000

mc_matrix <- matrix(NA, nrow = 12, ncol = n_sims)
for (i in 1:n_sims) {
  shocks <- rnorm(12, 0, sigma)
  mc_matrix[, i] <- eco_mean + ml_pred + shocks
}

# -------------------------------
# 5. Visualize simulation results
# -------------------------------
mc_df <- data.frame(
  Month = rep(1:12, n_sims),
  Price = as.vector(mc_matrix)
)

ggplot(mc_df, aes(x = Month, y = Price)) +
  geom_line(alpha = 0.05, color = "steelblue") +
  stat_summary(fun = mean, geom = "line", color = "red", size = 1.2) +
  labs(title = "Hybrid Econometric–ML–Monte Carlo Simulation",
       y = "Electricity Price (€/MWh)", x = "Forecast Horizon (Months)") +
  theme_minimal()