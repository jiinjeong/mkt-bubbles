# Jiin. Aug 11, 2021.

library(geckor)  # Collector for Coingecko API
library(ggplot2)  # Plotting
library(tseries)

setwd("Desktop/jiin-justin/mkt-bubbles")
source("common.R")  # Fns for stylized facts

options(scipen = 999)

# =================== STEP 1. Collect historic data. ===================
btc = coin_history(
  coin_id = "bitcoin",
  vs_currency = "usd",
  days = "max",
  interval = "daily")

btc = btc[order(as.Date(btc$timestamp, format="%Y-%m-%d")),]  # Order by date

# Calculate simple and CC (log) returns
ccReturn = diff(log(btc$price))
simpleReturn = exp(ccReturn) - 1
date = as.Date(btc$timestamp[2:length(btc$timestamp)])

btc.df = data.frame(
  "date" = date,
  "price" = as.vector(btc$price[2:length(btc$price)]),
  "simple" = simpleReturn,
  "cc" = ccReturn
)

btc.ts = crypto_to_ts(btc.df)  # Time series data

# =================== STEP 2. Basic Plots ===================
# Time series plot of historical Bitcoin price and returns
ggplot(btc.df, aes(x=date, y=price)) + geom_line()
ggplot(btc.df, aes(x=date)) + 
  geom_line(aes(y=ccReturn), color="red") +
  geom_line(aes(y=simpleReturn), color="black")

# Histogram + QQPlot + More
par(mfrow=c(2,2))
hist(btc.df$cc,main="BTC Daily Returns",
     xlab="btc", probability=T, col="slateblue1")
boxplot(btc.df$cc,outchar=T,col="slateblue1")
plot(density(btc.df$cc), main="smoothed density", 
     type="l",xlab="daily return",
     ylab="density estimate")
qqnorm(btc.df$cc)
qqline(btc.df$cc)
par(mfrow=c(1,1))

# Autocorrelation
acf_abs = acf(abs(btc.ts), lwd=2, type = c("correlation"), lag.max=100)  # Autocorrelation
acf_sq = acf(btc.ts * btc.ts, lwd=2, type = c("correlation"), lag.max=100)

# =================== STEP 3. Basic Stats ===================
sd(btc.ts)  # Std Dev
skewness(btc.ts)
kurtosis(btc.ts)
summary(btc.ts)

calc_q_ratio(btc.ts)
calc_hurst(btc.ts)
calc_hill(btc.ts)
calc_jb(btc.ts)
calc_adf(btc.ts)

# =================== STEP 4. Block bootstrap ===================
# https://rdrr.io/cran/tseries/man/tsbootstrap.html
boot = tsbootstrap(btc.ts, type="block", b=200,
                   statistic=mean)
