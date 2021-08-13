# Jiin. Aug 11, 2021.

library(geckor)  # Collector for Coingecko API
library(ggplot2)  # Plotting
library(tseries)

setwd("Desktop/jiin-justin/mkt-bubbles")
source("common.R")  # Fns for stylized facts

options(scipen = 999)

# =================== STEP 1. Collect historic data. ===================
# ----------- a. Bitcoin Daily Data in USD
btc = coin_history(
  coin_id = "bitcoin",
  vs_currency = "usd",
  days = "max",
  interval = "daily")

btc = btc[order(as.Date(btc$timestamp, format="%Y-%m-%d")),]  # Order by date

# Calculate simple and CC (log) returns
btc.cc = diff(log(btc$price))
btc.simple = exp(btc.cc) - 1
btc.date = as.Date(btc$timestamp[2:length(btc$timestamp)])

btc.df = data.frame(
  "date" = btc.date,
  "price" = as.vector(btc$price[2:length(btc$price)]),
  "simple" = btc.simple,
  "cc" = btc.cc
)

btc.ts = crypto_to_ts(btc.df)  # Time series data

# ----------- b. Vanguard S&P 500 Index Daily Data
sp500 = get.hist.quote(instrument="vfinx", quote="AdjClose",
                      provider="yahoo", origin="1970-01-01",
                      compression="d", retclass="zoo")
# start=start.date, end=end.date, 
sp500
sp500.cc = diff(log(sp500$Adjusted))
sp500.simple = exp(sp500.cc) - 1
sp500.date = as.Date(index(sp500)[2:length(sp500$Adjusted)])
sp500.df = data.frame(
  "date" = sp500.date,
  "price" = as.vector(sp500$Adjusted[2:length(sp500$Adjusted)]),
  "simple" = sp500.simple,
  "cc" = sp500.cc
)
rownames(sp500.df) = NULL
sp500.df
sp500.ts = crypto_to_ts(sp500.df)  # Time series data


# =================== STEP 2. Basic Plots ===================
# Time series plot of historical Bitcoin price and returns
ggplot(btc.df, aes(x=date, y=price)) + geom_line()
ggplot(btc.df, aes(x=date)) + 
  geom_line(aes(y=btc.cc), color="red") +
  geom_line(aes(y=btc.simple), color="black")

ggplot(sp500.df, aes(x=date, y=price)) + geom_line()

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

# 0. Reduce the time series.
# 1. Subdivide the time series into X non-overlapping number of blocks.
# 2. Construct a new series, from X random draws with replacement
# 3. Compute moments of the new series from 2.
# 4. Repeat stpes 1-3 for 5k times. Get a frequency distribution for each of the moments. (Ideally, empirical in the center)

k = 5000

btc.boot = tsbootstrap(btc.ts, type="block", b=500, nb=k)  # Increase to 5k eventually.
btc.orig.mean = mean(btc.ts)
btc.orig.mean
btc.moments.matrix = matrix(nrow=k, ncol=3)
btc.mean.matrix = matrix(nrow=k)
btc.std.matrix = matrix(nrow=k)
btc.q.matrix = matrix(nrow=k)

btc.moments.orig = matrix(c(mean(btc.ts),
                            sd(btc.ts),
                            calc_q_ratio(btc.ts)),
                          nrow=1, ncol=3)

# Do a for loop and get the moments from each boot
for (i in 1:k) {
  btc.mean.matrix[i] = mean(btc.boot[,i])
  btc.std.matrix[i] = sd(btc.boot[,i])
  btc.q.matrix[i] = calc_q_ratio(btc.boot[,i])
}

# Combined matrix of all moments (5000 rows x 3 col)
btc.moments.matrix = cbind(btc.mean.matrix,
                           btc.std.matrix,
                           btc.q.matrix)
# Matrix of the mean of all moments (1 row x 3 col)
btc.moments.mean.matrix = cbind((mean(btc.mean.matrix)),
                                (mean(btc.std.matrix)),
                                (mean(btc.q.matrix)))

plot(density(btc.mean.matrix), main="smoothed density", 
     type="l",xlab="daily return",
     ylab="density estimate")

total_sum = 0

for (i in 1:k) {
  mb = btc.moments.matrix[i,] - btc.moments.mean.matrix
  mbar = btc.moments.mean.matrix
  # (1 x 3) * (3 x 1) becomes an int
  total_sum = total_sum + ((mb - mbar) %*% t(mb - mbar))
}

total_sum / k  # Estimate of the moments' variance-covariance matrix


##### ACTUAL MOMENTS

# Moment 1: Mean value of the absolute returns
m1 = mean(abs(btc.ts))
m1

# Moment 2: First-order autocorrelation of the raw returns.
m2 = acf(abs(btc.ts))
m2

m2 = sd(btc.ts)
m8 = calc_hurst(btc.ts)

# Moment 3-8: ACF of the absolute returns up to a lag of 100 days.
# Six coefficients for the lags τ = 1, 5, 10, 25, 50, 100.
m3 = acf(abs(btc.ts), lwd=2, 
         type = c("correlation"),
         lag.max=100)  # Autocorrelation
m3

# Moment 9: Hill estimator
m9 = calc_hill(btc.ts)
m9

moments = matrix(c(m1, NA, NA,
                   NA, NA, NA,
                   NA, NA, m9),
                 nrow = 9, ncol = 1)
