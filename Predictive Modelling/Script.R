library(forecast)
library(tseries)
library(car)

# Load the data of mortality count of Ireland by gender
df_mortality <- read.csv("Data/IE Gender Mortality.csv", header = TRUE, na.strings = c("", "NA"), stringsAsFactors = FALSE)
str(df_mortality)

# Create timeseries data from the male and female mortality count
ts_male_mortality <- ts(as.numeric(df_mortality[1,][-1]), start = c(1960, 1), end = c(2017, 3), frequency = 4)
ts_female_mortality <- ts(as.numeric(df_mortality[2,][-1]), start = c(1960, 1), end = c(2017, 3), frequency = 4)

ts_male_mortality
ts_female_mortality

time(ts_male_mortality)
time(ts_female_mortality)

# Store the default parameter values of par() function in a variable opar
opar <- par(no.readonly = TRUE)

#divide graph area in 2 columns
par(mfrow = c(1, 2))

plot(ts_male_mortality)
plot(ts_female_mortality)

# Decompose the data to show original, trend, seasonal and randomness of data
decompose_ts_male <- decompose(ts_male_mortality)
decompose_ts_male
plot(decompose_ts_male)

decompose_ts_female <- decompose(ts_female_mortality)
decompose_ts_female
plot(decompose_ts_female)

ggseasonplot(ts_male_mortality)
ggseasonplot(ts_female_mortality)


# Remove stationarity
ndiffs(ts_male_mortality)
ndiffs(ts_female_mortality)

ts_d_male <- diff(ts_male_mortality, lag = frequency(ts_male_mortality), differences = ndiffs(ts_male_mortality))
ts_d_female <- diff(ts_female_mortality, lag = frequency(ts_female_mortality), differences = ndiffs(ts_female_mortality))
plot(ts_d_male)
plot(ts_d_female)

# Verify stationarity using the p value of Augmented Dickey-Fuller Test
adf.test(ts_d_male)
adf.test(ts_d_female)

plot(ts_d_male, type = "l", main = "Differenced and Stationary")
plot(ts_d_female, type = "l", main = "Differenced and Stationary")


# Check for correlation between the gender mortality count
cor.test(ts_male_mortality, ts_female_mortality)

# Autocorrelation test
acf(ts_d_male)
acf(ts_d_female)

pacf(ts_d_male)
pacf(ts_d_female)

# ARIMA Model
fit_male_arima <- auto.arima(ts_male_mortality)
fit_female_arima <- auto.arima(ts_female_mortality)

fit_male_arima
fit_female_arima


# ARIMA Model forecast
forecast_male_arima <- forecast(fit_male_arima, 10)
forecast_male_arima
plot(forecast_male_arima)

forecast_female_arima <- forecast(fit_female_arima, 10)
forecast_female_arima
plot(forecast_female_arima)



# Exponential smoothing model
fit_male_ets <- ets(ts_male_mortality)
fit_female_ets <- ets(ts_female_mortality)
fit_male_ets
fit_female_ets


# Exponential smoothing Model forecast
forecast_male_ets <- forecast(fit_male_ets, 10)
forecast_male_ets
plot(forecast_male_ets)

forecast_female_ets <- forecast(fit_female_ets, 10)
forecast_female_ets
plot(forecast_female_ets)

# Accuracy measure of ARIMA model
accuracy(fit_male_arima)
accuracy(fit_female_arima)

# Accuracy measure of Exponential smoothing model
accuracy(fit_male_ets)
accuracy(fit_female_ets)


# Verify the residuls for the validity of the model
Box.test(fit_male_arima$residuals, type = "Ljung-Box")
Box.test(fit_female_ets$residuals, type = "Ljung-Box")

plot(fit_male_arima$residuals)
plot(fit_female_ets$residuals)

acf(fit_male_arima$residuals)
acf(fit_female_ets$residuals)
