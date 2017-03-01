#=============== FORECASING HOMEWORK 2 ===============

library(ggplot2)
library(fpp)
library(readxl)
data <- read_excel("HW2data.xlsx")
colnames(data) = c("Time", "Delta_d", "d", "Delta_ps", "ps", "d_ps", "Delta_d_ps", "Delta_p", "p", "d_p", "Delta_d_p")
data$Time = as.Date(gsub("\\)", "-01",ifelse(nchar(data$Time)>7, gsub("\\(", "-", data$Time), gsub("\\(", "-0", data$Time))), "%Y-%m-%d")


# Quick global look at the data
for (i in 2:ncol(data)){
  gr = ggplot(data = data, aes_string(x = "Time", y = colnames(data)[i])) +
    geom_line()
  print(gr)
}


# ACF and PACF of p
tsdisplay(data$p)
# The decreasing ACF shows a stationary process

# PACF suggest an AR(1)
# Let's fit an ARIMA(1, 0, 0) and try variations such as ARIMA(2, 0, 0) and ARIMA(3, 0, 0)
fit1 <- Arima(data$p, c(1, 0, 0))
summary(fit1)
fit2 <- Arima(data$p, c(2, 0, 0))
summary(fit2)
fit3 <- Arima(data$p, c(3, 0, 0))
summary(fit3)
# AR(3) coefficient is not significant (doing a t-test)
Acf(fit1$residuals)
# Some autocorrelation left
Acf(fit2$residuals)
# No significant autocorrelation left
# AR(2) seems to be a good fit


# ACF and PACF of Delta p
tsdisplay(data$Delta_p)
# The rapidly decreasing ACF shows a stationary process

# PACF suggest an AR(1)
# Let's fit an ARIMA(1, 0, 0) and try variations such as ARIMA(2, 0, 0) and ARIMA(3, 0, 0)
fitdelta1 <- Arima(data$Delta_p, c(1, 0, 0))
summary(fitdelta1)
fitdelta2 <- Arima(data$Delta_p, c(2, 0, 0))
summary(fitdelta2)
fitdelta3 <- Arima(data$Delta_p, c(3, 0, 0))
summary(fitdelta3)
# AR(3) and AR(2) coefficient is not significant (doing a t-test)
Acf(fitdelta1$residuals)
Acf(fitdelta2$residuals)
# No significant autocorrelation left
# AR(1) seems to be a good fit

summary(fitdelta1)
summary(fit2)
# fitdelta1 has the lowest AICc, so we may want to choose this model

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
fitdp <- holt(data$d_p[1:645], h=205)
plot(fitdp, type="l")
lines(data$d_p, col="black")

