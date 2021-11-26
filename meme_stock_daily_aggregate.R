library(forecast)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(seasonal)
library(urca)


#WSB Data from NLP in Python
df <-  read.csv("~/Desktop/R_ECON_138/df.csv")

#adding additional date only column
df$Date <- as.Date(df$timestamp)

#GME stock price data
price <- read.csv("~/Desktop/R_ECON_138/GME.csv")

price$Date <- as.Date(price$Date)

price <- rename(price, gme_close = Close)

price <- price[, -c(2:4)] # delete columns 2 through 4
price <- price[, -c(3:4)]

#NYSE index data
nyse <- read.csv("~/Desktop/R_ECON_138/nyse.csv")

nyse$Date <- as.Date(nyse$Date)

nyse <- rename(nyse, nyse_close = Close)

nyse <- nyse[, -c(2:4)] # delete columns 2 through 4
nyse <- nyse[, -c(3:4)]

#####Transformations###

#aggregating polarity, number of comments and closing price by day

#creating df_agg for daily values
df_agg <- aggregate(df["TB_polarity"], by=df["Date"], sum)

#creating df_agg for subjectivity values
temp <- aggregate(df["subjectivity"], by=df["Date"], sum)

#joining aggregated subjectivity values
df_agg <- left_join(
  df_agg,
  temp,
  by = 'Date',
  copy = TRUE,
  keep = FALSE,
  na_matches = c("na", "never")
)

#joining GME stock price data
df_agg <- left_join(
  df_agg,
  price,
  by = 'Date',
  copy = TRUE,
  keep = FALSE,
  na_matches = c("na", "never")
)

#joining nyse index data to df_agg
df_agg <- left_join(
  df_agg,
  nyse,
  by = 'Date',
  copy = TRUE,
  keep = FALSE,
  na_matches = c("na", "never")
)

#converting to ts
df_agg_ts <- as.ts(df_agg)

#plotting daily aggregated polarity
ggplot(data = df_agg, aes(x = Date, y = TB_polarity)) + 
  geom_line(color = "Green") +
  theme_light() +
  labs(
    title = "Daily Aggregated Polarity Scores of WSB Posts about $GME",
    x = "Time",
    y = "Polarity of Posts")

#plotting stock price 
ggplot(data = df_agg, aes(x = Date, y = gme_close)) + 
  geom_line(color = "Blue") +
  theme_light() +
  labs(
    title = "GameStop Stock Closing Price",
    x = "Time",
    y = "Stock Price")


#plotting daily aggregated polarity
ggplot(data = df_agg, aes(x = Date, y = nyse_close)) + 
  geom_line(color = "orange") +
  theme_light() +
  labs(
    title = "NYSE Index Closing Price",
    x = "Time",
    y = "Index Price")

#Linear model with aggregated values
agg_model <- tslm(gme_close ~
                  TB_polarity +
                  nyse_close,
                  data = df_agg_ts)
summary(agg_model)

#not a good model

#Linear model with aggregated values omitting nyse idex
agg_model <- tslm(gme_close ~
                    TB_polarity,
                  data = df_agg_ts)
summary(agg_model)

#######ARIMA with aggregated data###########

# Create matrix of numeric predictors
xreg <- cbind(date = df_agg$timestamp,
              pol = df_agg$TB_polarity)

# Remove intercept
xreg <- xreg[,-1]

# Target Variable to be modeled
price_agg_ts <- ts(df_agg$gme_close, frequency=1)

# Find ARIMA model
modArima <- auto.arima(price_agg_ts, xreg=xreg)
summary(modArima)

checkresiduals(modArima)

#addding fitted value to df_agg
df_agg$fitted <- modArima$fitted

#plotting gamestop stock price and fitted value
ggplot() + 
  geom_line(data = df_agg, aes(x = Date, y = gme_close), color = "blue") +
  geom_line(data = df_agg, aes(x = Date, y = fitted), color = "red") +
  theme_grey() +
  labs(
    title = "Game Stop Stock Price Fitted vs. Actual",
    x = "Time",
    y = "$GME Close Price")

write.csv(df_agg,"~/Desktop/R_ECON_138/df_agg.csv", row.names = FALSE)

