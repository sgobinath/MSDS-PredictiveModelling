ts_data <- EuStockMarkets[, 1]
opar <- par()
par(mfrow = c(1, 2))
decomposed_result <- decompose(ts_data, type = "mult")
plot(decomposed_result)
decomposed_result <- decompose(ts_data, type = "additive")
plot(decomposed_result)
seasonal_trend_error <- stl(ts_data, s.window = "periodic")
par <- opar
seasonal_trend_error$time.series

head(EuStockMarkets[, 1])
lagged_ts <- lag(ts_data, 3)
head(ts_data)
head(lagged_ts)
length(ts_data)
length(lagged_ts)
tail(ts_data)
tail(lagged_ts)

install.packages("DataCombine")
library(DataCombine)
my_df <- as.data.frame(ts_data)
my_df <- slide(my_df, "x", NewVar = "xLag1", slideBy = -1)
my_df <- slide(my_df, "x", NewVar = "xLead1", slideBy = 1)
head(my_df)
tail(my_df)

acf_res <- acf(AirPassengers)
pacf_res <- pacf(AirPassengers)

plot(JohnsonJohnson)
trained_model <- lm(JohnsonJohnson ~ c(1:length(JohnsonJohnson)))
plot(resid(trained_model), type = "l")

str(JohnsonJohnson)


install.packages("forecast")
library(forecast)
? stl
ts_decompose <- stl(AirPassengers, "periodic")
ts_seasonal_adjust <- seasadj(ts_decompose)
plot(AirPassengers, type = "l")
plot(ts_seasonal_adjust, type = "l")

seasonplot(ts_seasonal_adjust, 12, col = rainbow(12), year.labels = TRUE, main = "Seasonal Plot: Airpassengers")

library(tseries)
adf.test(ts_data)
? kpss.test
kpss.test(ts_data)

nsdiffs(AirPassengers)
ndiffs(AirPassengers)

str(AirPassengers)
head(AirPassengers,100)

AirPassengers_seasdiff <- diff(AirPassengers, lag = frequency(AirPassengers), differences = 1)
plot(AirPassengers_seasdiff, type = "l", main = "Seasonally Differenced")
plot(AirPassengers, type = "l", main = "Original")

nsdiffs(AirPassengers_seasdiff)
AirPassengersTS <- diff(AirPassengers_seasdiff, lag = frequency(AirPassengers_seasdiff), differences = 1)
plot(AirPassengersTS, type = "l", main = "Differenced and Stationary")

frequency(AirPassengers)
frequency(AirPassengers_seasdiff)

Nile
str(Nile)
head(Nile)
plot(Nile)
ndiffs(Nile)

d_nile <- diff(Nile)
plot(d_nile)

adf.test(d_nile)
acf(d_nile)
pacf(d_nile)

fit <- Arima(Nile, order = c(0, 1, 1))
fit
? Arima
accuracy(fit)

help("qqnorm")
qqnorm(fit$residuals)
qqline(fit$residuals)

Box.test(fit$residuals, type = "Ljung-Box")

my_forecast <- forecast(fit, 10)
plot(my_forecast)

fit <- auto.arima(Nile)
fit
accuracy(fit)

qqnorm(fit$residuals)
qqline(fit$residuals)

auto_forecast <- forecast(fit, 20)
plot(auto_forecast)
