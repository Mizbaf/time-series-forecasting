
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



rm(list=ls())

# Loading data from csv
bat_stats = read.csv("Player_Team_Data//Batsman_Data.csv", header=TRUE, sep=";")


# Convert 'Start.Date' column to Date format
bat_stats$Start.Date <- as.Date(bat_stats$Start.Date, format="%d/%m/%Y")



#------------------------------BAT STATS-----------------------------------------------------

# Filtering Virat Kohli's data to perform model fitting & forecasting
bat_stats_filtered <- subset(bat_stats, Batsman == "Virat Kohli ")


# Filtering only necessary columns
bat_stats_filtered <- bat_stats_filtered[, c("Runs", "BF", "SR", "X4s", "X6s","Opposition", "Ground",
                                             "Start.Date")]


# Convert other potentially numeric columns to numeric, as appropriate
numeric_columns <- c("Runs", "BF", "SR", "X4s", "X6s")
bat_stats_filtered[numeric_columns] <- lapply(bat_stats_filtered[numeric_columns], 
                                              function(x) as.numeric(gsub("-", NA, x)))

# Remove the "v " prefix from the Opposition column
bat_stats_filtered$Opposition <- gsub("^v ", "", bat_stats_filtered$Opposition)


# Defining a vector of home grounds
home_grounds <- c('Ahmedabad', 'Bengaluru', 'Chennai', 'Delhi', 'Guwahati', 'Gwalior',
                  'Hyderabad (Deccan)', 'Indore', 'Jaipur', 'Kanpur', 'Kochi', 'Kolkata', 'Mohali',
                  'Mumbai (BS)', 'Nagpur', 'Pune', 'Rajkot', 'Ranchi', 'Thiruvananthapuram', 'Vadodara',
                  'Visakhapatnam')

# Add Home/Away column as binary 1-Home, 0-Away
bat_stats_filtered <- bat_stats_filtered %>% 
  mutate(Home_Away = ifelse(Ground %in% home_grounds, 'Home', 'Away'))

# Convert categorical variables to factors
bat_stats_filtered$Opposition <- as.factor(bat_stats_filtered$Opposition)
bat_stats_filtered$Ground <- as.factor(bat_stats_filtered$Ground)


# Converting missing values in Runs column to NAs
bat_stats_filtered$Runs[bat_stats_filtered$Runs == "-"] <- NA


# Removing NAs from test and train data
bat_stats_filtered <- bat_stats_filtered[!is.na(bat_stats_filtered$Runs), ]
bat_stats_filtered <- bat_stats_filtered[!is.na(bat_stats_filtered$SR), ]


# Split data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(bat_stats_filtered), 0.7*nrow(bat_stats_filtered))
bat_train_data <- bat_stats_filtered[train_indices, ]
bat_test_data <- bat_stats_filtered[-train_indices, ]

# Preparing regressors
# Convert factors to dummy variables
dummies_opposition_train <- model.matrix(~ Opposition + SR - 1, data = bat_train_data)
dummies_opposition_test <- model.matrix(~ Opposition + SR  - 1, data = bat_test_data)
dummies_home_away_train <- model.matrix(~ Home_Away - 1, data = bat_train_data)
dummies_home_away_test <- model.matrix(~ Home_Away - 1, data = bat_test_data)

# Combine the two sets of regressors to one variable
combined_train_data <- cbind(dummies_opposition_train, dummies_home_away_train)
combined_test_data <- cbind(dummies_opposition_test, dummies_home_away_test)


# Combine dummy variables with numeric variables
numeric_vars_train <- bat_train_data %>% select_if(is.numeric)
numeric_vars_test <- bat_test_data %>% select_if(is.numeric)

# Ensure combined data is a numeric matrix and retain column names
combined_train_data_matrix <- as.matrix(combined_train_data)
combined_test_data_matrix <- as.matrix(combined_test_data)

#---------------------------EDA BAT STATS--------------------------------------------

# Calculate summary statistics for filtered_bat_stats
summary_stats <- summary(bat_train_data)

# Print the summary statistics
print(summary_stats)


# Select numeric variables for correlation analysis
numeric_vars <- bat_train_data %>% select_if(is.numeric)

# Print the names of numeric variables
names(numeric_vars)

# Compute the correlation matrix
cor_matrix <- cor(numeric_vars, use = "complete.obs")

# Print the correlation matrix
print(cor_matrix)

# Visualize the correlation matrix using corrplot
corrplot(cor_matrix, method = "shade", type = "lower", tl.cex = 0.8, tl.col = "black", number.cex = 0.7)

# Boxplot of Runs
boxplot(bat_train_data$Runs, main = "Boxplot of Runs Scored", ylab = "Runs")

# Histogram for Runs
ggplot(bat_train_data, aes(x = Runs)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Runs", x = "Runs", y = "Frequency")

# Density plot for Runs
ggplot(bat_train_data, aes(x = Runs)) +
  geom_density(fill = "brown", alpha = 0.5) +
  labs(title = "Density Plot of Runs", x = "Runs", y = "Density")

# Bar plot for Opposition
ggplot(bat_train_data, aes(x = Opposition)) +
  geom_bar(fill = "darkgreen", color = "black", alpha = 0.5) +
  labs(title = "Matches by Opposition", x = "Opposition", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Bar plot for Ground
ggplot(bat_train_data, aes(x = Ground)) +
  geom_bar(fill = "brown", color = "black", alpha = 0.5) +
  labs(title = "Matches by Ground", x = "Ground", y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Runs by Opposition
ggplot(bat_train_data, aes(x = Opposition, y = Runs)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Runs by Opposition", x = "Opposition", y = "Runs") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Runs by Ground
ggplot(bat_train_data, aes(x = Ground, y = Runs)) +
  geom_boxplot(fill = "pink", color = "black") +
  labs(title = "Runs by Ground", x = "Ground", y = "Runs") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#------------------------------MODEL FITTING----------------------------------------


# Check for missing values in 'Start.Date' and 'Runs' and remove rows with NA values
bat_train_data <- bat_train_data %>%
  filter(!is.na(Start.Date) & !is.na(Runs))

# Ensure 'Runs' is numeric
bat_train_data$Runs <- as.numeric(bat_train_data$Runs)

# Converting data to time series using zoo, since the irregularity in time interval
batting_ts_zoo <- zoo(bat_train_data$Runs)

# Augmented Dickey-Fuller (ADF) Test
adf_result <- adf.test(batting_ts_zoo)
print(adf_result)

# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) Test
kpss_result <- kpss.test(batting_ts_zoo)
print(kpss_result)

#### Both tests suggest the data is **stationary**

# Plot the time series object
plot(batting_ts_zoo, main = "Time Series Data for Runs", xlab = "Time Step", ylab = "Runs")


# Plot the autocorrelation function (ACF)
acf(batting_ts_zoo, main = "Autocorrelation Function for Runs Scored")

# Plot the partial autocorrelation function (PACF)
pacf(batting_ts_zoo, main = "Partial Autocorrelation Function for Runs Scored")

# Fit an ARIMA model without external regressors
bat_model_wr <- auto.arima(batting_ts_zoo, stepwise = FALSE, approximation = FALSE)

# Print summary of model
summary(bat_model_wr)

# Forecasting runs and plotting the forecasted runs using the model
forecasted_runs_wr <- forecast(bat_model_wr, h = 10)
print(forecasted_runs_wr)
plot(forecasted_runs_wr)

# Model fitting using regressors
# Check for multicollinearity and remove redundant columns in regressor matrix
corr_matrix <- cor(combined_train_data_matrix)
highly_correlated <- findCorrelation(corr_matrix, cutoff = 0.9)
regressors <- combined_train_data_matrix[ , -highly_correlated]

# Remove columns with all zeros from combined_train_data_matrix
filtered_regressors <- regressors[, colSums(regressors != 0) > 0]


# Fit an ARIMA model to the batting time series data
bat_model <- auto.arima(batting_ts_zoo, xreg=filtered_regressors[,-1],
                        stepwise = FALSE, approximation = FALSE)

# Print summary of fitted model
summary(bat_model)

# Ensure the columns in test data matches train data
# Get the column names from both datasets
train_cols <- colnames(filtered_regressors)
test_cols <- colnames(combined_test_data_matrix)

# Find columns in test data that are not in train data
extra_cols <- setdiff(test_cols, train_cols)

# Remove extra columns from test data
filtered_test_regressors <- combined_test_data_matrix[, !(colnames(combined_test_data_matrix) %in% extra_cols)]

# Forecast runs for the test data
forecasted_runs <- forecast(bat_model,xreg=filtered_test_regressors[,-1])
print(forecasted_runs)
forecasted_plot <- plot(forecasted_runs)

# Replace negative predictions with zero using ifelse
forecasted_runs$mean <- ifelse(forecasted_runs$mean < 0, 0, forecasted_runs$mean)
forecasted_plot <- plot(forecasted_runs)


# Extract the time index from the forecasted object
forecast_index <- time(forecasted_runs$mean)

# Extract the correct starting index from the forecast index
start_index <- start(forecast_index)[1]

# Ensure bat_test_data has the same time index
bat_test_index <- seq(from = start_index, by = 1, length.out = length(bat_test_data$Runs))

# Add the actual runs line to the forecast plot
lines(bat_test_index, bat_test_data$Runs, col="red")

#-------------MODEL EVALUATION-----------------------------------


# Checking residuals
modelResiduals <- residuals(bat_model)

# Plot Histogram and QQ-plot
hist(modelResiduals, main = "Histogram of Residual(ARIMA(1,0,1)")
qqnorm(modelResiduals, main = "Normal Q-Q Plot")
qqline(modelResiduals, col="red")


# Extract predicted values
y_pred <- as.numeric(forecasted_runs$mean)
y_true <- as.numeric(bat_test_data$Runs)


# Calculate MSE
mse_value <- mse(bat_test_data$Runs, y_pred)
print(paste("Mean Squared Error:", mse_value))

# Mean Absolute Error (MAE)
mae_value <- mae(y_true, y_pred)
print(paste("Mean Absolute Error:", mae_value))

# Trying other models with different p,d,q values
runs_arima <- Arima(batting_ts_zoo, c(1,0,0), xreg=filtered_regressors[,-1])
AIC(runs_arima)
BIC(runs_arima)
summary(runs_arima)

# Forecast runs for the next games
forecasted_runs <- forecast(runs_arima, xreg=filtered_test_regressors[,-1], h=10)

# Replace negative predictions with zero using ifelse
forecasted_runs$mean <- ifelse(forecasted_runs$mean < 0, 0, forecasted_runs$mean)

# Plot forecasted runs
print(forecasted_runs)
plot(forecasted_runs)

# Extract the time index from the forecasted object
forecast_index <- time(forecasted_runs$mean)

# Extract the correct starting index from the forecast index
start_index <- start(forecast_index)[1]

# Ensure bat_test_data has the same time index
bat_test_index <- seq(from = start_index, by = 1, length.out = length(bat_test_data$Runs))

# Add the actual runs line to the forecast plot
lines(bat_test_index, bat_test_data$Runs, col="red")


# Extract predicted values
y_pred <- as.numeric(forecasted_runs$mean)
y_true <- as.numeric(bat_test_data$Runs)


# Calculate MSE
mse_value <- mse(y_true, y_pred)
print(paste("Mean Squared Error:", mse_value))

# Mean Absolute Error (MAE)
mae_value <- mae(y_true, y_pred)
print(paste("Mean Absolute Error:", mae_value))


# Summary of models tried

# | Model        | AIC     | BIC     |  MSE    |  MAE  |
# |--------------|---------|---------|---------|--------
# | ARIMA(1,0,1) | 1524.54 | 1569.9  | 1253.73 | 26.03 |
# | ARIMA(1,0,0) | 1528.14 | 1570.48 | 1286.11 | 25.66 |
# | ARIMA(0,0,1) | 1528.14 | 1570.47 | 1286.62 | 25.66 |
# | ARIMA(1,1,1) | 1524.61 | 1566.85 | 1282.99 | 25.71 |
# | ARIMA(2,0,1) | 1524.82 | 1641.09 | 1276.32 | 26.13 |


#---------------------------FORECASTING-----------------------------------------
# Forecasting future 10 matches
future_data <- read.csv("Future_Data_Virat_Kohli.csv")
future_data


# Add Home/Away column as binary 1-Home, 0-Away
future_data <- future_data %>% 
  mutate(Home_Away = ifelse(Ground %in% home_grounds, 'Home', 'Away'))

# Convert categorical variables to factors
future_data$Opposition <- as.factor(future_data$Opposition)
future_data$Ground <- as.factor(future_data$Ground)

# Preparing regressors
# Convert factors to dummy variables
dummies_opposition_forecast <- model.matrix(~ Opposition + SR - 1, data = future_data)
dummies_home_away_forecast <- model.matrix(~ Home_Away-1, data = future_data)

# Combine the two sets of regressors to one variable
combined_forecast_data <- cbind(dummies_opposition_forecast, dummies_home_away_forecast)

# Ensure combined data is a numeric matrix and retain column names
combined_forecast_data_matrix <- as.matrix(combined_forecast_data)

# Check for multicollinearity and remove redundant columns in regressor matrix
corr_matrix_forecast <- cor(combined_forecast_data_matrix)
highly_correlated_forecast <- findCorrelation(corr_matrix_forecast, cutoff = 0.9)
forecast_regressors <- combined_forecast_data_matrix[ , -highly_correlated_forecast]

# Forecast runs for the next games
future_forecasted_runs <- forecast(bat_model, xreg=forecast_regressors, h=10)
future_forecasted_runs$mean <- round(future_forecasted_runs$mean)
print(future_forecasted_runs)
plot(future_forecasted_runs)




##------------------------PREDICTION------------------------

# Using linear regression to predict runs scored by Virat Kohli without using time series
bat_train_predict <- bat_train_data
bat_test_predict <- bat_test_data[1:10,]

# Convert necessary columns to factors (if they are categorical)
bat_train_predict$Opposition <- as.factor(bat_train_predict$Opposition)
bat_train_predict$Ground <- as.factor(bat_train_predict$Ground)

# Fit the linear regression model
linear_model <- lm(Runs ~ Opposition + Ground + SR, data = bat_train_predict)

# Display the summary of the model to check coefficients and model statistics
summary(linear_model)


# Convert necessary columns to factors (if they are categorical)
bat_test_predict$Opposition <- factor(bat_test_predict$Opposition, levels = levels(bat_train_predict$Opposition))
bat_test_predict$Ground <- factor(bat_test_predict$Ground, levels = levels(bat_train_predict$Ground))

# Predict the runs using the fitted model
bat_test_predict$Predicted_Runs <- predict(linear_model, newdata = bat_test_predict)


# Actual Vs Predicted runs
ggplot(bat_test_predict, aes(x = 1:nrow(bat_test_predict))) +
  geom_line(aes(y = Runs, color = "Actual Runs", group = 1), size = 1) +
  geom_line(aes(y = Predicted_Runs, color = "Predicted Runs", group = 1), linetype = "dashed", size = 1) +
  scale_color_manual(name = "Legend", values = c("Actual Runs" = "blue", "Predicted Runs" = "red")) +
  labs(title = "Actual vs. Predicted Runs for Virat Kohli", x = "Match Index", y = "Runs") +
  theme(legend.position = "bottom")


# Calculate the residuals
bat_test_predict$residuals <- bat_test_predict$Runs - bat_test_predict$Predicted_Runs

# Calculate the squared residuals
bat_test_predict$squared_residuals <- bat_test_predict$residuals^2

# Calculate the Mean Squared Error (MSE)
mse <- mean(bat_test_predict$squared_residuals)

# Print the MSE
print(mse)

# Including Balls Faced(BF) also as regressor
new_linear_model <- lm(Runs ~ Opposition + Ground + SR + BF, data = bat_train_predict)


# Predict the runs using the fitted model
bat_test_predict$Predicted_Runs <- predict(new_linear_model, newdata = bat_test_predict)

# Actual Vs Predicted runs
ggplot(bat_test_predict, aes(x = 1:nrow(bat_test_predict))) +
  geom_line(aes(y = Runs, color = "Actual Runs", group = 1), size = 1) +
  geom_line(aes(y = Predicted_Runs, color = "Predicted Runs", group = 1), linetype = "dashed", size = 1) +
  scale_color_manual(name = "Legend", values = c("Actual Runs" = "blue", "Predicted Runs" = "red")) +
  labs(title = "Actual vs. Predicted Runs for Virat Kohli", x = "Match Index", y = "Runs") +
  theme(legend.position = "bottom")


# Calculate the residuals
bat_test_predict$residuals <- bat_test_predict$Runs - bat_test_predict$Predicted_Runs

# Calculate the squared residuals
bat_test_predict$squared_residuals <- bat_test_predict$residuals^2

# Calculate the Mean Squared Error (MSE)
mse <- mean(bat_test_predict$squared_residuals)

# Print the MSE
print(mse)


# Model Summary
# | Model                               |   MSE     |  
# |-------------------------------------|---------- |
# | Linear Regression with 3 regressors |  1153.001 |
# | Linear Regression with 4 regressors |  229.8828 |


