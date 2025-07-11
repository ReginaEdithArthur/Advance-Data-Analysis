#Set working directory
setwd("C:/Users/LENOVO/Desktop/Advance Data Analysis Exam/Covid_19_Time_Series_Modelling_Project")

# Install these packages if you don't have them yet
install.packages(c("forecast", "tseries", "urca", "ggplot2"))

# Load libraries
library(forecast)
library(tseries)
library(urca)
library(ggplot2)

# Load the data
data <- read.csv("WHO-COVID-19-global-daily-data.csv")

# Convert Date_reported to Date type
data$Date_reported <- as.Date(data$Date_reported, format="%d/%m/%Y")

# Aggregate Global New Cases
global_cases <- aggregate(New_cases ~ Date_reported, data=data, sum)

# Replace any negative or NA values (sometimes corrections in COVID data)
global_cases$New_cases[global_cases$New_cases < 0] <- 0
global_cases$New_cases[is.na(global_cases$New_cases)] <- 0

# Plot Global Daily New Cases
ggplot(global_cases, aes(x=Date_reported, y=New_cases)) +
  geom_line(color = "blue") +
  labs(title="Global Daily New COVID-19 Cases", x="Date", y="New Cases") +
  theme_minimal()

# Convert to time series object
ts_cases <- ts(global_cases$New_cases, frequency = 365) # daily data

# Augmented Dickey-Fuller Test (ADF)
adf.test(ts_cases)

# Phillips-Perron Test (PP)
pp.test(ts_cases)

# KPSS Test
kpss.test(ts_cases)

# ACF and PACF plots
acf(ts_cases, main="ACF of Differenced Series")
pacf(ts_cases, main="PACF of Differenced Series")

# Apply first differencing if non-stationary
diff_ts_cases <- diff(ts_cases)

# Plot differenced series
plot(diff_ts_cases, main="Differenced Global COVID-19 Cases", ylab="Differenced Cases", xlab="Time")

# Re-run stationarity tests
adf.test(diff_ts_cases)
pp.test(diff_ts_cases)
kpss.test(diff_ts_cases)

# ACF and PACF plots
acf(diff_ts_cases, main="ACF of Differenced Series")
pacf(diff_ts_cases, main="PACF of Differenced Series")

auto.arima(diff_ts_cases)

# Example: Fit ARIMA(p, d, q)
# Replace p, d, q with actual numbers from auto.arima() or your inspection
model <- arima(ts_cases, order=c(5,0,5)) 

# Summary of model
summary(model)

# Residuals plot
tsdisplay(residuals(model), lag.max=40, main="Model Residuals")

# Ljung-Box test
Box.test(residuals(model), lag=20, type="Ljung-Box")

# Forecast next 365 days (1 year)
forecast_cases <- forecast(model, h=365)

# Plot forecast
plot(forecast_cases)

# View forecasted values
print(forecast_cases)
