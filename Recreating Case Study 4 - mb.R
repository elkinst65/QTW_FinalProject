


setwd("~/Google Drive/smu/quantifying/QTW_FinalProject")

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
retDate <- Dates[2:length(Dates)]
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


# Below is work reproduced from https://rviews.rstudio.com/2016/11/09/reproducible-finance-with-r-the-sharpe-ratio/
# and modified for our stock grouping.
library(PerformanceAnalytics)
library(quantmod)
library(dygraphs)

monthly_stock_returns <- function(ticker, start_year) {
  # Download the data from Yahoo finance
  symbol <- getSymbols(ticker, src = 'yahoo', auto.assign = FALSE, warnings = FALSE)
  # Tranform it to monthly returns using the periodReturn function from quantmod
  data <- periodReturn(symbol, period = 'monthly', subset=paste(start_year, "::", sep = ""),
                       type = 'log')

  # Let's rename the column of returns to something intuitive because the column name is what
  # will eventually be displayed on the time series graph.
  colnames(data) <- as.character(ticker)

  # We want to be able to work with the xts objects that result from this function
  # so let's explicitly put them to the global environment with an easy to use
  # name, the stock ticker.
  assign(ticker, data, .GlobalEnv)
}

year = 2016
monthly_stock_returns('FB', year)
monthly_stock_returns('NFLX', year)
monthly_stock_returns('TWTR', year)
monthly_stock_returns('AMZN', year)
monthly_stock_returns('NVDA', year)

merged_returns <- merge.xts(FB, NFLX, TWTR, AMZN, NVDA)

dygraph(merged_returns, main = "Facebook v Netflix v Twitter v Amazon v NVidia") %>%
  dyAxis("y", label = "% Change") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(5, "Set2"))

# We have the 5 monthly returns saved in 1 object.
# Now, let's choose the respective weights of those 5.
w <- c(.2, .2, .2, .2, .2)
# Now use the built in PerformanceAnalytics function Return.portfolio
# to calculate the monthly returns on the portfolio, supplying the vector of weights 'w'.
portfolio_monthly_returns <- Return.portfolio(merged_returns, weights = w)

# Use dygraphs to chart the portfolio monthly returns.
dygraph(portfolio_monthly_returns, main = "Portfolio Monthly Return") %>%
  dyAxis("y", label = "%")

# Add the wealth.index = TRUE argument and, instead of returning monthly returns,
# the function will return the growth of $1 invested in the portfolio.
dollar_growth <- Return.portfolio(merged_returns, weights = w, wealth.index = TRUE)

# Use dygraphs to chart the growth of $1 in the portfolio.
dygraph(dollar_growth, main = "Growth of $1 Invested in Portfolio") %>%
  dyAxis("y", label = "$")

# the Sharpe Ratio measures excess returns per unit of volatility, where we take the
# standard deviation to represent portfolio volatility. The Sharpe Ratio was brought
# to us by Bill Sharpe - arguably the most important economist for modern investment
# management as the creator of the Sharpe Ratio, CAPM and Financial Engines, a forerunner
# of today’s robo-advisor movement.

# using a risk-free rate of 0.03% which is mean of 1-month T bill
sharpe_ratio <- round(SharpeRatio(portfolio_monthly_returns, Rf = .0003), 4)

# result
sharpe_ratio[1,]
