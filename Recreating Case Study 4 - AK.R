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
# How to get the ticker data. We will be focusing on the date range 1/1/2016 - 5/1/2017
# Set variables to the csv's
fb <- read.csv("data/FB.csv")
nflx <- read.csv("data/NFLX.csv")
twtr <- read.csv("data/TWTR.csv")
amzn <- read.csv("data/AMZN.csv")
nvda <- read.csv("data/NVDA.CSV")

# Let's start with one stock at a time
fb$Date <- as.Date(fb$Date)

ggplot(fb, aes(fb$Date, fb$Close)) +
  geom_line(aes(color="fb")) +
  labs(color="Legend") +
  scale_color_manual("", breaks = c("fb"),
                     values = c("blue")) +
  ggtitle("Daily Closing Stock Prices: Facebook") +
  theme(plot.title = element_text(lineheight = .7, face = "bold"))




