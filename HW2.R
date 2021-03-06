#=============== FORECASING HOMEWORK 2 ===============
# Environment
library(ggplot2)
library(fpp)
library(readxl)

# Import data, enhance tile data, and make variable names more R-friendly
data <- read_excel("HW2data.xlsx")
colnames(data) = c("Time", "Delta_d", "d", "Delta_ps", "ps", "d_ps", "Delta_d_ps", "Delta_p", "p", "d_p", "Delta_d_p")
data$Time = as.Date(gsub("\\)", "-01",ifelse(nchar(data$Time)>7, gsub("\\(", "-", data$Time), gsub("\\(", "-0", data$Time))), "%Y-%m-%d")


# Quick global look at the data
for (i in 2:ncol(data)){
  gr = ggplot(data = data, aes_string(x = "Time", y = colnames(data)[i])) +
    geom_line()
  print(gr)
}


# Q1 -------------
# Stationarity and integration
# ACF and PACF of p
tsdisplay(data$p)
# The slowly decreasing ACF and the PACF show a stationary process

# ACF and PACF of Delta p
tsdisplay(data$Delta_p)
# The rapidly decreasing ACF shows a stationary process, without a trend

# p seems to be stationary of order 1

tsdisplay(data$ps)
# stationary
tsdisplay(data$Delta_ps)
# stationary

# ps seems to be stationary of order 1 as well

tsdisplay(data$d)
# stationary
tsdisplay(data$Delta_d)
# shows a seasonality of 3 months (logical with quarterly dividends)

# d seems to be stationary, but delta d shows a seasonality

# Regression between price and dividend
fit_p_d <- lm(data$p~data$d)
summary(fit_p_d)
plot(data$d, data$p)
abline(fit_p_d)

# Forecasting d/p

plot(data$d_p)
# downward trend
# then a simple exponential smoothing method will not be enough
# let's then use holt linear trend method to start with
fitdp <- holt(data$d_p[1:843], h=12)
plot(fitdp, type="l")
lines(data$d_p, col="black")

plot(data$Delta_d_p, type="l")
tsdisplay(data$Delta_d_p)
# suggests an AR(1) on the diff
fitddp <- Arima(data$d_p[1:843], c(1, 1, 0))
summary(fitddp)
plot(forecast(fitddp, h=12))
lines(data$d_p, col="red")

plot(forecast(fitddp, h=12), xlim=c(837, 855))
lines(data$d_p, col="red")
plot(forecast(fitdp, h=12), xlim=c(837, 855))
lines(data$d_p, col="red")
# they both seem to give good results on a short time frame
# it would be interesting to compare the accuracy of both methods (visually, holt seems to do better)


