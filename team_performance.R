# Loading necessary libraries

library(forecast)
library(corrplot)

library(tseries)
library(ggplot2)
library(Metrics)
library(stringr)

library(dplyr)
library(caret)
library(zoo)



##------------------------TEAM PERFORMANCE------------------


# Read the CSV file for Team Totals
team_data <- read.csv("ODI_Match_Totals.csv",sep=';')

# Convert 'Start.Date' column to Date format
team_data$Start.Date <- as.Date(team_data$Start.Date, format="%d/%m/%Y")

# Remove the "v " prefix from the Opposition column
team_data$Opposition <- gsub("^v ", "", team_data$Opposition)

# Filter for India's first innings
india_first_innings <- team_data %>% filter(Country == "India" & Inns == 1)

# Extract the runs from the Score column
india_first_innings <- india_first_innings %>%
  mutate(Runs = as.numeric(sub("/.*", "", Score)))

# Filtering only necessary columns
india_first_innings <- india_first_innings[, c( "Overs", "RPO", "Target", "Inns", "Runs")]

# Split the data into train and test sets
team_train_data <- india_first_innings %>% filter(Start.Date <= as.Date("2018-12-31"))
team_test_data <- india_first_innings %>% filter(Start.Date > as.Date("2018-12-31"))

#------------------------EDA-----------------------------------------------

# Calculate summary statistics for team train data
summary_stats_team <- summary(team_train_data)

# Print the summary statistics
print(summary_stats_team)


# Density plot for Score
ggplot(team_train_data, aes(x = Runs)) +
  geom_density(fill = "brown", alpha = 0.5) +
  labs(title = "Density Plot of Runs Scored", x = "Score", y = "Density")


# Boxplot of Score
boxplot(team_train_data$Runs, main = "Boxplot of Score", ylab = "Score")

#---------------------------MODEL FITTING---------------------------------------


# Create dummy variables for Ground and Opposition using model.matrix
ground_dummies <- model.matrix(~ Ground - 1, data = india_first_innings)
opposition_dummies <- model.matrix(~ Opposition - 1, data = india_first_innings)

# Combine the dummy variables with the original data
combined_regressors <- cbind(ground_dummies, opposition_dummies)


# Check for multicollinearity and remove redundant columns
corr_matrix <- cor(combined_regressors)
highly_correlated <- findCorrelation(corr_matrix, cutoff = 0.9)
regressors <- combined_regressors[ , -highly_correlated]


# Convert Runs to time series
india_runs_ts <- zoo(team_train_data$Runs)

# Plot time series data
plot(india_runs_ts, main = "Time Series Data for Team Score", xlab = "Time Step", ylab = "Score")

# Plot the autocorrelation function (ACF)
acf(india_runs_ts, main = "Autocorrelation Function for Team runs scored")

# Plot the partial autocorrelation function (PACF)
pacf(india_runs_ts, main = "Partial Autocorrelation Function for Team runs scored")

# Augmented Dickey-Fuller (ADF) Test
team_adf_result <- adf.test(india_runs_ts)
print(team_adf_result)

# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) Test
team_kpss_result <- kpss.test(india_runs_ts)
print(team_kpss_result)

# Apply differencing to remove trend
log_india_runs_ts <- log(india_runs_ts)

diff_india_runs_ts <- diff(log_india_runs_ts)

team_kpss_result <- kpss.test(diff_india_runs_ts)
print(team_kpss_result)

# Convert regressors to matrix
regressors_matrix <- as.matrix(regressors)

regressor_train <- regressors_matrix[1:60,]

# Remove columns with all zeros from regressors
regressors_matrix <- regressor_train[, colSums(regressor_train != 0) > 0]
regressors_matrix <- regressors_matrix[,-1]
regressors_matrix <- regressors_matrix[,-36]

# Fit ARIMA model with regressors
team_model <- auto.arima(diff_india_runs_ts, xreg = regressors_matrix[-1,-1],
                         stepwise = FALSE, approximation = FALSE)
summary(team_model)
AIC(team_model)
BIC(team_model)


# Trying Other Models using different p,d,q values
team_arima <- Arima(diff_india_runs_ts, c(4,0,2), xreg=regressors_matrix[-1,-1])
AIC(team_arima)
BIC(team_arima)
summary(team_arima)


# Ensure the columns in test data matches train data
# Get the column names from both datasets
train_cols_team <- colnames(regressors_matrix)
test_cols_team <- colnames(regressors_matrix_test)

# Find columns in test data that are not in train data
extra_cols <- setdiff(test_cols_team, train_cols_team)

# Remove extra columns from test data
regressor_test <- regressors_matrix_test[, !(colnames(regressors_matrix_test) %in% extra_cols)]

# Filtering regressors of test data
regressor_test <- regressor_test[61:65,]


# Forecast the next few matches (next 5 matches)
forecasted_scores <- forecast(team_arima, xreg = regressor_test[-1,-1], h = 4)


# Plot the forecast
plot(forecasted_scores, type = "l")

# Extract the time index from the forecasted object
forecast_index <- time(forecasted_scores$mean)

# Extract the correct starting index from the forecast index
start_index <- start(forecast_index)[1]

log_test_runs_ts <- log(team_test_data$Runs)
diff_test_runs_ts <- diff(log_test_runs_ts)

# Ensure bowl_test_data has the same time index
team_test_index <- seq(from = start_index, by = 1, length.out = length(diff_test_runs_ts))

# Add the actual wkts line to the forecast plot
lines(team_test_index, diff_test_runs_ts, col="red")

# Print forecasted scores
print(forecasted_scores)

print(diff_test_runs_ts)

#Checking residuals
modelResiduals <- residuals(team_arima)

# Histogram and QQ-plot
hist(modelResiduals, main = "Histogram of Residual(ARIMA(4,0,2))")
qqnorm(modelResiduals, main = "Normal Q-Q Plot")
qqline(modelResiduals, col="red")


# Extract predicted values
y_pred_team <- as.numeric(forecasted_scores$mean)
y_true_team <- as.numeric(diff_test_runs_ts)


# Calculate MSE
mse_team <- mse(y_true_team, y_pred_team)
print(paste("Mean Squared Error:", mse_team))

# Mean Absolute Error (MAE)
mae_team <- mae(y_true_team, y_pred_team)
print(paste("Mean Absolute Error:", mae_team))


# Model Summary

# | Model        | AIC     | BIC     |  MSE    |  MAE  |
# |--------------|---------|---------|---------|--------
# | ARIMA(3,0,2) | -63.81  | 29.684  | 1.8543  | 1.206 |
# | ARIMA(1,0,1) | 28.728  | 118.06  | 2.3267  | 1.358 |
# | ARIMA(2,0,1) | 14.476  | 76.935  | 4.6763  | 1.750 |
# | ARIMA(2,0,2) | -58.64  | 34.853  | 4.8353  | 1.822 |
# | ARIMA(4,0,2) | -82.64  | 15.001  | 0.5150  | 0.627 |
# | ARIMA(4,0,3) | -64.32  | 35.397  | 0.3117  | 0.491 |