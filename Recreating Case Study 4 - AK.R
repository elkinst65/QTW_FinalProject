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

# Let's start with one stock at a time
Dates <- as.Date(fb$Date)

df <- data.frame(Dates, fbc, nflxc, twtrc, amznc, nvdac)

# This is just plotting the raw data
ggplot(df, aes(Dates)) +
  geom_line(aes(y=fbc, color = "fb")) +
  geom_line(aes(y=nflxc, color = "nflx")) +
  geom_line(aes(y=twtrc, color = "twtr")) +
  geom_line(aes(y=amznc, color = "amzn")) +
  geom_line(aes(y=nvdac, color = "nvda")) +
  scale_color_manual("", 
                      breaks = c("fb", "nflx", "twtr", "amzn", "nvda"),
                      values = c("green","orange","purple", "blue","red")) # These go top to bottom. 

#
