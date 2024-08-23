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



# Loading data from csv
ball_stats = read.csv("Player_Team_Data//Bowler_data.csv", header=TRUE, sep=";")



# Convert 'Start.Date' column to Date format
ball_stats$Start.Date <- as.Date(ball_stats$Start.Date, format="%d/%m/%Y")


#---------------------BOWLER DATA------------------------------

# Filtering Ravindra Jadeja's data to perform model fitting & forecasting
bowl_stats_filtered <- subset(ball_stats, Bowler == "Ravindra Jadeja")

# Filtering only necessary columns
bowl_stats_filtered <- bowl_stats_filtered[, c("Wkts", "Overs", "Mdns", "Runs", 
                                               "Econ", "Ave", "SR", "Opposition", "Ground",
                                               "Start.Date")]


# Omit NA values
bowl_stats_filtered <- na.omit(bowl_stats_filtered)

# Convert other potentially numeric columns to numeric
numeric_columns <- c("Overs", "Mdns", "Runs", "Wkts", "Econ", "Ave", "SR")
bowl_stats_filtered[numeric_columns] <- lapply(bowl_stats_filtered[numeric_columns], 
                                               function(x) as.numeric(gsub("-", NA, x)))

# Remove the "v " prefix from the Opposition column
bowl_stats_filtered$Opposition <- gsub("^v ", "", bowl_stats_filtered$Opposition)

# Define a vector of home grounds
home_grounds <- c('Ahmedabad', 'Bengaluru', 'Chennai', 'Delhi', 'Guwahati', 'Gwalior',
                  'Hyderabad (Deccan)', 'Indore', 'Jaipur', 'Kanpur', 'Kochi', 'Kolkata', 'Mohali', 'Mumbai',
                  'Mumbai (BS)', 'Nagpur', 'Pune', 'Rajkot', 'Ranchi', 'Thiruvananthapuram', 'Vadodara',
                  'Visakhapatnam')

# Add Home/Away column as binary 1-Home, 0-Away
bowl_stats_filtered <- bowl_stats_filtered %>% 
  mutate(Home_Away = ifelse(Ground %in% home_grounds, 'Home', 'Away'))

# Convert categorical variables to factors
bowl_stats_filtered$Opposition <- as.factor(bowl_stats_filtered$Opposition)
bowl_stats_filtered$Ground <- as.factor(bowl_stats_filtered$Ground)


# Converting '-' in Runs column to NAs
bowl_stats_filtered$Runs[bowl_stats_filtered$Runs == "-"] <- NA


# Removing NAs from test and train data
bowl_stats_filtered <- bowl_stats_filtered[!is.na(bowl_stats_filtered$Runs), ]
bowl_stats_filtered <- bowl_stats_filtered[!is.na(bowl_stats_filtered$Wkts), ]


# Split data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(bowl_stats_filtered), 0.7*nrow(bowl_stats_filtered))
bowl_train_data <- bowl_stats_filtered[train_indices, ]
bowl_test_data <- bowl_stats_filtered[-train_indices, ]


# Preparing test and train dummy variables
# Convert factors to dummy variables
dummies_opposition_train <- model.matrix(~ Opposition + Mdns - 1, data = bowl_train_data)
dummies_opposition_test <- model.matrix(~ Opposition + Mdns - 1, data = bowl_test_data)
dummies_home_away_train <- model.matrix(~ Home_Away - 1, data = bowl_train_data)
dummies_home_away_test <- model.matrix(~ Home_Away - 1, data = bowl_test_data)

# Combine the two sets of regressors to one variable 
combined_train_data <- cbind(dummies_opposition_train, dummies_home_away_train)
combined_test_data <- cbind(dummies_opposition_test, dummies_home_away_test)


# Ensure combined data is a numeric matrix and retain column names
combined_train_data_matrix <- as.matrix(combined_train_data)
combined_test_data_matrix <- as.matrix(combined_test_data)

#------------------------EDA-----------------------------------------------

# Calculate summary statistics for filtered bowling stats
summary_stats_bowl <- summary(bowl_train_data)

# Print the summary statistics
print(summary_stats_bowl)


# Select numeric variables for correlation analysis
numeric_vars_bowl <- bowl_train_data %>% select_if(is.numeric)

# Print the names of numeric variables
names(numeric_vars_bowl)

# Compute the correlation matrix
cor_matrix <- cor(numeric_vars_bowl, use = "complete.obs")

# Print the correlation matrix
print(cor_matrix)

# Plot correlation matrix
corrplot(cor_matrix, method = "shade", type = "lower", tl.cex = 0.8, tl.col = "black", number.cex = 0.7)


# Density plot for Wickets
ggplot(bowl_train_data, aes(x = Wkts)) +
  geom_density(fill = "brown", alpha = 0.5) +
  labs(title = "Density Plot of Wkts", x = "Wkts", y = "Density")


#Boxplot of Wickets
boxplot(bowl_train_data$Wkts, main = "Boxplot of Wickets", ylab = "Wickets")


#-------------------------MODEL FITTING--------------------------------

# Sorting the data by start_date
bowl_train_data <- bowl_train_data[order(bowl_train_data$Start.Date), ]


# Ensure 'Wkts' is numeric
bowl_train_data$Wkts <- as.numeric(bowl_train_data$Wkts)

# Converting data to time series using zoo, since the irregularity in time interval
bowling_ts_zoo <- zoo(bowl_train_data$Wkts)


# Plot the zoo object
plot(bowling_ts_zoo, main = "Time Series Data for Wickets", xlab = "Time Step", ylab = "Wickets")


# Plot the autocorrelation function (ACF)
acf(bowling_ts_zoo, main = "Autocorrelation Function for Wickets taken")

# Plot the partial autocorrelation function (PACF)
pacf(bowling_ts_zoo, main = "Partial Autocorrelation Function for Wickets taken")

# Model Fitting without regressors
fit <- auto.arima(bowling_ts_zoo, stepwise = FALSE, approximation = FALSE)
summary(fit) # Fitted model is ARIMA(0,0,0) with non-zero mean


# Model fitting using regressors
# Check for multicollinearity and remove redundant columns in regressor matrix
corr_matrix <- cor(combined_train_data_matrix)
highly_correlated <- findCorrelation(corr_matrix, cutoff = 0.9)
regressors_ball <- combined_train_data_matrix[ , -highly_correlated]

# Remove columns with all zeros from regressors
filtered_regressors_ball <- regressors_ball[, colSums(regressors_ball != 0) > 0]


# Fit an ARIMA model to the bowling time series data
fit <- auto.arima(bowling_ts_zoo, xreg=filtered_regressors_ball[,-1],
                  stepwise = FALSE, approximation = FALSE)

# Print summary of fitted model
summary(fit)
AIC(fit)
BIC(fit)



# Ensure the columns in test data matches train data
# Get the column names from both datasets
train_cols_ball <- colnames(filtered_regressors_ball)
test_cols_ball <- colnames(combined_test_data_matrix)

# Find columns in test data that are not in train data
extra_cols <- setdiff(test_cols_ball, train_cols_ball)

# Remove extra columns from test data
filtered_test_regressors <- combined_test_data_matrix[, !(colnames(combined_test_data_matrix) %in% extra_cols)]

# Forecast wickets for the test data
forecasted_wkts <- forecast(fit,xreg=filtered_test_regressors[,-1], h = 10)
print(forecasted_wkts)

# Round the forecasted values to the nearest whole number
forecasted_wkts$mean <- round(forecasted_wkts$mean)

plot(forecasted_wkts)

# Extract the time index from the forecasted object
forecast_index <- time(forecasted_wkts$mean)

# Extract the correct starting index from the forecast index
start_index <- start(forecast_index)[1]

# Ensure bowl_test_index has the same time index
bowl_test_index <- seq(from = start_index, by = 1, length.out = length(bowl_test_data$Runs))

# Add the actual test data line to the forecast plot
lines(bowl_test_index, bowl_test_data$Wkts, col="red")

# MODEL EVALUATION

#Checking residuals
modelResiduals <- residuals(fit)

# Histogram and QQ-plot
hist(modelResiduals, main = "Histogram of Residual(ARIMA(0,0,1))")
qqnorm(modelResiduals, main = "Normal Q-Q Plot")
qqline(modelResiduals, col="red")


# Extract predicted values
y_pred <- as.numeric(forecasted_wkts$mean)
y_true <- as.numeric(bowl_test_data$Wkts)


# Calculate MSE
mse_value <- mse(y_true, y_pred)
print(paste("Mean Squared Error:", mse_value))

# Mean Absolute Error (MAE)
mae_value <- mae(y_true, y_pred)
print(paste("Mean Absolute Error:", mae_value))

#-------------------------------------------------------------------------------
# Trying other ARIMA models with different p,d,q values
wkts_arima <- Arima(bowling_ts_zoo, c(1,0,0), xreg=filtered_regressors_ball[,-1])
AIC(wkts_arima)
BIC(wkts_arima)
summary(wkts_arima)

# Forecast wickets for the test data with new model
forecasted_wkts <- forecast(wkts_arima, xreg=filtered_test_regressors[,-1], h=10)


# Round the forecasted values to the nearest whole number
forecasted_wkts$mean <- round(forecasted_wkts$mean)

print(forecasted_wkts)
plot(forecasted_wkts)

# Extract the time index from the forecasted object
forecast_index <- time(forecasted_wkts$mean)

# Extract the correct starting index from the forecast index
start_index <- start(forecast_index)[1]

# Ensure bowl_test_data has the same time index
bowl_test_index <- seq(from = start_index, by = 1, length.out = length(bowl_test_data$Runs))

# Add the actual test data line to the forecast plot
lines(bowl_test_index, bowl_test_data$Wkts, col="red")


# Extract predicted values
y_pred <- as.numeric(forecasted_wkts$mean)
y_true <- as.numeric(bowl_test_data$Wkts)


# Calculate MSE
mse_value <- mse(y_true, y_pred)
print(paste("Mean Squared Error:", mse_value))

# Mean Absolute Error (MAE)
mae_value <- mae(y_true, y_pred)
print(paste("Mean Absolute Error:", mae_value))


# Model Summary

# | Model        | AIC     | BIC     |  MSE    |  MAE  |
# |--------------|---------|---------|---------|--------
# | ARIMA(0,0,1) | 342.32  | 379.07  | 1.756   | 1.000 |
# | ARIMA(1,0,0) | 343.171 | 382.546 | 2.000   | 1.067 |
# | ARIMA(0,0,1) | 343.007 | 382.382 | 2.000   | 1.067 |
# | ARIMA(1,1,1) | 345.272 | 384.499 | 2.000   | 1.067 |
# | ARIMA(2,0,1) | 346.786 | 391.411 | 2.067   | 1.088 |



#---------------------------FORECASTING-----------------------------------------

# Forecasting future 10 matches
future_data_bowl <- read.csv("Future_Data_Jadeja.csv")
future_data_bowl

# Add Home/Away column as binary 1-Home, 0-Away
future_data_bowl <- future_data_bowl %>% 
  mutate(Home_Away = ifelse(Ground %in% home_grounds, 'Home', 'Away'))

# Convert categorical variables to factors
future_data_bowl$Opposition <- as.factor(future_data_bowl$Opposition)
future_data_bowl$Ground <- as.factor(future_data_bowl$Ground)

# Preparing regressors
# Convert factors to dummy variables
dummies_opposition_forecast <- model.matrix(~ Opposition + Mdns - 1, data = future_data_bowl)
dummies_home_away_forecast <- model.matrix(~ Home_Away-1, data = future_data_bowl)

# Combine the two sets of regressors to one variable
combined_forecast_data <- cbind(dummies_opposition_forecast, dummies_home_away_forecast)

# Ensure combined data is a numeric matrix and retain column names
combined_forecast_data_matrix <- as.matrix(combined_forecast_data)

# Check for multicollinearity and remove redundant columns in regressor matrix
corr_matrix_forecast <- cor(combined_forecast_data_matrix)
highly_correlated_forecast <- findCorrelation(corr_matrix_forecast, cutoff = 0.9)
forecast_regressors <- combined_forecast_data_matrix[ , -highly_correlated_forecast]

# Get the names of regressors from the model fitting
model_regressors <- colnames(filtered_regressors_ball[,-1])

# Identify missing regressors in the test data
missing_regressors <- setdiff(model_regressors, colnames(forecast_regressors))

# Add missing regressors to the test data(adding 0)
forecast_regressors <- as.data.frame(forecast_regressors)

for (regressor in missing_regressors) {
  forecast_regressors[[regressor]] <- rep(0, nrow(forecast_regressors))
}

# Ensure the order of columns in the test data matches the training data
forecast_regressors <- forecast_regressors[, model_regressors, drop = FALSE]

forecast_regressors <- as.matrix(forecast_regressors)

# Forecast and plot wickets for the next games
future_forecasted_wkts <- forecast(fit, xreg=forecast_regressors, h=10)
future_forecasted_wkts$mean <- round(future_forecasted_wkts$mean)
print(future_forecasted_wkts)
plot(future_forecasted_wkts)

