###############################################
# This file loads the seasonally adjusted
# monthly alcohol sales from January 1992
# to April 2021.
# This data will be used to forecast the
# monthly alcohol sales for the next 2 years.
#
# Created by: Abegail Romas
# AMAT 132 - Section E
###############################################

# Clear all variables in the workspace.
rm(list=ls())

# Load the forecasting package.
library(fpp2)

# Load the dataset.
Data <- read.csv("C:/Users/ASUS/Downloads/S4248SM144NCEN.csv")

# Declare data as time series data.
y <- ts(Data[,2], start = c(1992,1), frequency = 12)


###############################################
# Perform preliminary analysis on data
###############################################

# TIME PLOT (Raw data)
autoplot(y) +
  ggtitle("Time Plot: Monthly Alcohol Sales") +
  xlab("Year") +
  ylab("Millions of dollars")

# Data has a strong trend, and the variance increases over time.
# Investigate transformations.

# First, use the Box Cox Transformation to stabilize the variance.
# 1. Find the best value of lambda.
lambda <- BoxCox.lambda(y)
print(lambda)

# 2. TIME PLOT
plot.ts(BoxCox(y, lambda=lambda))
BC <- BoxCox(y, lambda=lambda)

# Investigate seasonality.
ggseasonplot(DiffData, polar = TRUE) +
  ggtitle("Seasonal Plot: Change in Alcohol Sales") +
  ylab("Millions of dollars")

#####################################################
# SUMMARY OF PRELIMINARY ANALYSIS
# The series y has an increasing variance,
# but there is no seasonality.
# To stabilize the variance, the Box Cox
# transformation was performed.
#####################################################

##############################
# VARIOUS FORECASTING METHODS
##############################

# 1. Exponential Smoothing method   # Residual SD = 3e-04 = 0.0003
fit_ets <- ets(BC)
print(summary(fit_ets))
checkresiduals(fit_ets)

# 2. Fit on ARIMA Model             # Residual SD = 0.0011
fit_arima <- auto.arima(BC, d = 1, D = 1, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)

#####################################
# FORECAST WITH EXPONENTIAL SMOOTHING
#####################################
fcst <- forecast(fit_ets, h=24)
autoplot(fcst)
print(summary(fcst))

######################################################
# SUMMARY OF FORECASTING
# The series has been fitted to 2 different methods:
# 1) the exponential smoothing method and
# 2) the ARIMA model. The exponential smoothing had
# less residual SD, so it was the best fit.
# Hence, it was used to  forecast the alcohol sales
# for the next two years.
######################################################