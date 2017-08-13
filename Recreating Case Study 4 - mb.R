# Libraries needed:  for getting the ticker data.
# frontier for the analysis,
# Let's use Hexbin for the heatmap

# Helpful sites:
# https://flowingdata.com/2012/10/04/more-on-making-heat-maps-in-r/
# http://blog.revolutionanalytics.com/2015/08/plotting-time-series-in-r.html
# https://learnxinyminutes.com/docs/r/

setwd("~/GitHub/Quantifying the World/QTW_FinalProject")

#
# install.packages("frontier")
# install.packages("PerformanceAnalytics")
# install.packages("zoo")
# install.packages("tseries")
# install.packages("hexbin")

library(zoo)
library(PerformanceAnalytics)
library(tseries)
library(quantmod)
library(hexbin)
library(frontier)
library(ggplot2)


# 'FB', 'NFLX', 'TWTR', 'AMZN', 'NVDA'
# Set variables to the csv's and single out Closing price
Dates <- as.Date(fb$Date)
fb <- read.csv("data/FB.csv")
fbc <- fb$Close

nflx <- read.csv("data/NFLX.csv")
nflxc <- nflx$Close

twtr <- read.csv("data/TWTR.csv")
twtrc <- twtr$Close

amzn <- read.csv("data/AMZN.csv")
amznc <- amzn$Close

nvda <- read.csv("data/NVDA.CSV")
nvdac <- nvda$Close

df <- data.frame(Dates, fbc, nflxc, twtrc, amznc, nvdac)
summary(df) # lookin' good

# This is just plotting the raw data
ggplot(df, aes(Dates)) +
  geom_line(aes(y=fbc, color = "fb")) +
  geom_line(aes(y=nflxc, color = "nflx")) +
  geom_line(aes(y=twtrc, color = "twtr")) +
  geom_line(aes(y=amznc, color = "amzn")) +
  geom_line(aes(y=nvdac, color = "nvda")) +
  scale_color_manual("",
                      breaks = c("fb", "nflx", "twtr", "amzn", "nvda"),
                      values = c("green","orange","purple", "blue","red")) +# These go top to bottom.
  ggtitle("Raw Closing values by Day") +
  theme(plot.title = element_text(lineheight=.7, face="bold")) +
  xlab("") +
  ylab("Value in USD")

# Let's create a daily cumulative return function.
dailyRets <- function(stocks) {
  prices <- stocks[,"Adj.Close", drop=FALSE]
  n <- nrow(stocks)
  returns <- ((prices[2:n,1] - prices[1:(n-1),1])/prices[1:(n-1),1]) # sbux_ret (from example)
  gret <- 1 + returns
  fv <- cumprod(gret) - 1
  names(fv) <- stocks[2:n,1]
  return(fv)
}

# Apply to all stocks
fbr <- dailyRets(fb)
nflxr <- dailyRets(nflx)
twtrr <- dailyRets(twtr)
amznr <- dailyRets(amzn)
nvdar <- dailyRets(nvda)
retDate <- Dates[2:len(Dates)]
dfr <- data.frame(retDate, fbr, nflxr, twtrr, amznr, nvdar)

ggplot(dfr, aes(retDate)) +
  geom_line(aes(y=fbr, color = "fb")) +
  geom_line(aes(y=nflxr, color = "nflx")) +
  geom_line(aes(y=twtrr, color = "twtr")) +
  geom_line(aes(y=amznr, color = "amzn")) +
  geom_line(aes(y=nvdar, color = "nvda")) +
  scale_color_manual("",
                     breaks = c("fb", "nflx", "twtr", "amzn", "nvda"),
                     values = c("blue","orange", "green","red","purple")) +# These go top to bottom.
  ggtitle("Returns by Day") +
  theme(plot.title = element_text(lineheight=.7, face="bold")) +
  xlab("Date") +
  ylab("")


# Time to work on the Sharpe Ratio
#PerformanceAnalytics Package has this built in. Not 100% sure on how to handle it.
SharpeRatio(nflxr, Rf=0.95, scale = 252, VaR = 1)

SharpeRatio.annualized(fbr)
x <- SharpeRatio(fbr, Rf = 0)
x

SharpeRatio(dfr[,2:6], Rf = dfr[,2,drop=FALSE]) # Still working with this one. Not sure where to take it.

#
# Everything below this line is a WIP. It may not be needed
#

# Let's move onto how they do everything in Python for Data Analysis. (This is failing spectacularly)

pctchange <- function(val){
  return(val/lag(val,-1) - 1)
}

lag <- 1

calc_mom <- function(price, lookback, lag){
  mom_ret <- shift(price, n = 1)
  mom_ret <- mom_ret.pctchange(lookback)
  ranks <- mom_ret.rank(axis = 1, ascending = FALSE)
  demeaned <- ranks.subtract(ranks.mean(axis = 1))
  return(demeaned.divide(demeaned.std(axis=1), axis = 0))
}

compound <- function(x){
  return(prod(1+x) - 1)
}

daily_sr <- function(x){
  return(mean(x)/stdDev(x))
}

strat_sr <- function(prices, lb, hold){
  # Compute the portfolio weights
  freq <- as.integer("%d") %% hold
  port <- calc_mom(prices, lb)
  daily_rets <- pctchange(prices)

  # Compute the portfolio returns
  port <- port.shift(1)
  port <- port.resample(freq)
  port <- first(port)

  returns <- daily_rets.resample(freq)
  returns <- returns.apply(compound)

  port_rets <- sum(port * returns,axis=1)

  return(daily_sr(port_rets) * sqrt(252 / hold))
}

strat_sr(fbc, 70, 30)
