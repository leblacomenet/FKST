#=============== FORECASING HOMEWORK 2 ===============

library(ggplot2)
library(fpp)

setwd("~/MScDSBA/FKST/HW2")
library(readxl)
data <- read_excel("HW2data.xlsx")
colnames(data) = c("Time", "Delta_d", "d", "Delta_ps", "ps", "d-ps", "Delta_d-ps", "Delta_p", "p", "d-p", "Delta_d-p")
data$Time = as.Date(gsub("\\)", "-01",ifelse(nchar(data$Time)>7, gsub("\\(", "-", data$Time), gsub("\\(", "-0", data$Time))), "%Y-%m-%d")


# Quick global look at the data
for (i in 2:ncol(data)){
    gr = ggplot(data = data, aes_string(x = "Time", y = colnames(data)[i])) +
        geom_line()
    print(gr)
}


# ACF and PACF of Delta p
tsdisplay(data$Delta_p)
# The rapidly decreasing ACF shows a stationary process

# PACF suggest an AR(1)
# Let's fit an ARIMA(1, 0, 0) and try variations such as ARIMA(2, 0, 0) and ARIMA(3, 0, 0)
fit1 <- Arima(df$p, c(1, 0, 0))
summary(fit1)
fit2 <- Arima(df$p, c(2, 0, 0))
summary(fit2)
# intercept s.e is NaN???
fit3 <- Arima(df$p, c(3, 0, 0))
summary(fit3)
# AR(3) coefficient is not significant (doing a t-test)
Acf(fit1$residuals)
# Some autocorrelation left
Acf(fit2$residuals)
# No significant autocorrelation left
# AR(2) seems to be a good fit