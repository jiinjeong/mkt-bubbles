# Jiin. Aug 11, 2021.

library(geckor)  # Collector for Coingecko API
library(ggplot2)  # Plotting
library(tseries)  # Times Series Data
library(forecast)  # Autocorrelation

setwd("/Users/Jiin/Desktop/jiin-justin/mkt-bubbles")
source("common.R")  # Fns for stylized facts

options(scipen = 999)

# =================== STEP 1. Collect historic data. ===================
# ----------- a. Bitcoin Daily Data in USD
collect_btc <- function(){
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
    # write.csv(btc.df, file="btc.csv")
}

# ----------- b. Vanguard S&P 500 Index Daily Data
collect_sp500 <- function(){
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
    # write.csv(sp500.df, file="sp500.csv")
}

##### TO REUSE DATA
btc.df = data.frame(read.csv("btc.csv"))
btc.df$date = as.Date(btc.df$date)
btc.ts = crypto_to_ts(btc.df)  # Time series data
sp500.df = data.frame(read.csv("sp500.csv"))
sp500.df$date = as.Date(sp500.df$date)
sp500.ts = crypto_to_ts(sp500.df)  # Time series data

# =================== STEP 2. Basic Plots and Stats ===================
basic_plots <- function(){
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
}

basic_stats <- function(){
    sd(btc.ts)  # Std Dev
    skewness(btc.ts)
    kurtosis(btc.ts)
    summary(btc.ts)
    
    calc_q_ratio(btc.ts)
    calc_hurst(btc.ts)
    calc_hill(btc.ts)
    calc_jb(btc.ts)
    calc_adf(btc.ts)
}

# =================== STEP 3. Moments ===================
calc_moments <- function(ts){
  # Moment 1: Mean value of the absolute returns
  m1 = mean(abs(ts))

  # Moment 2: First-order autocorrelation of the raw returns.
  # Daily data = 365
  raw_acf = Acf(ts)
  lag = 1
  m2 = raw_acf$acf[1 + lag][1]

  # Moment 3-8: ACF of the absolute returns up to a lag of 100 days.
  # Six coefficients for the lags τ = 1, 5, 10, 25, 50, 100.
  abs_acf = Acf(abs(ts), lag.max=100)
  lag = c(1, 5, 10, 25, 50, 100)
  m3 = abs_acf$acf[1 + lag[1]][1]
  m4 = abs_acf$acf[1 + lag[2]][1]
  m5 = abs_acf$acf[1 + lag[3]][1]
  m6 = abs_acf$acf[1 + lag[4]][1]
  m7 = abs_acf$acf[1 + lag[5]][1]
  m8 = abs_acf$acf[1 + lag[6]][1]
  
  # Moment 9: Hill estimator
  m9 = as.numeric(calc_hill(ts))

  moments = matrix(c(m1, m2, m3,
                     m4, m5, m6,
                     m7, m8, m9),
                   nrow = 9, ncol = 1)
  return(moments)
}

# =================== STEP 4. Block bootstrap ===================
# https://rdrr.io/cran/tseries/man/tsbootstrap.html

# 0. Reduce the time series.
# 1. Subdivide the time series into X non-overlapping number of blocks.
# 2. Construct a new series, from X random draws with replacement
# 3. Compute moments of the new series from 2.
# 4. Repeat stpes 1-3 for 5k times. Get a frequency distribution for each of the moments. (Ideally, empirical in the center)

get_weighting_matrix <- function(){
    k = 100  # 5000 to match the paper
    block_size = 500  # Diff block sizes later on (250, 750)
    n_moments = 9
    
    btc.boot = tsbootstrap(btc.ts, type="block", b=block_size, nb=k)  # Increase to 5k eventually.
    btc.moments.matrix = matrix(nrow=n_moments, ncol=k)  # 9 row x 5000 col
    btc.moments.orig.matrix = calc_moments(btc.ts)  # 9 x 1
    
    # Do a for loop and get the moments from each boot
    for (i in 1:k) {
      print(i)
      btc.moments.matrix[,i] = calc_moments(btc.boot[,i])
    }
    btc.moments.matrix
    # save(btc.moments.matrix, file="btc.moments.Rdata")
    # load("btc.moments.Rdata")
    
    # Matrix of the mean of all moments (9 x 1)
    btc.moments.mean.matrix = matrix(rowMeans(btc.moments.matrix),
                                    nrow=n_moments, ncol=1)
    btc.moments.mean.matrix
    
    # First row matrix
    plot(density(btc.moments.matrix[1,]), main="smoothed density", 
         type="l",xlab="daily return",
         ylab="density estimate")
    btc.moments.orig.matrix  # 1st item.
    
    total_sum = matrix(rep(0, 81),
                       nrow=9, ncol=9)
    for (i in 1:k) {
      mb = btc.moments.matrix[,i] - btc.moments.mean.matrix
      mbar = btc.moments.mean.matrix
      each_result = (mb - mbar) %*% t(mb - mbar)
      # print(each_result)
      # (9 x 1) * (1 x 9) = (9 x 9) matrix
      total_sum = total_sum + each_result
    }
    
    total_sum
    sigma_hat = total_sum / k  # Estimate of the moments' variance-covariance matrix
    btc.weighting = solve(sigma_hat)  # Weighting matrix
    btc.weighting
    
    # save(btc.weighting, file="btc.weighting.Rdata")
}

load("btc.weighting.Rdata")
btc.weighting

# =================== STEP 6. Parameter stuff ===================
# theta = (p1, p2, p3, p4, p5, p6)
# Parameter
# How do we simulate model with params?

# Initialize parameters - Latin hypercube.

# Nelder Mead: https://www.rdocumentation.org/packages/lme4/versions/1.1-13/topics/NelderMead
# LHS: https://www.rdocumentation.org/packages/pse/versions/0.4.7/topics/LHS
library(lhs)
randomLHS(5, 4)

pop = seq.int(0, 1000, 1)
memory = seq.int(0, 500, 5)
startprice = seq.int(10, 100, 10)
params <- expand.grid(pop, memory, startprice)

set.seed(1213)
X <- randomLHS(n = 10, k = 3)

# pairs(X, labels = c("Memory","Start Price","Params"))
Y <- X
Y[,1] <- 1 + floor(X[,1] * length(pop))  # 1st col
Y[,2] <- 1 + floor(X[,2] * length(memory))  # 2nd col
Y[,3] <- 1 + floor(X[,3] * length(startprice))  # 3rd col

actual_lhs = matrix(nrow=10, ncol=3)

for (i in 1:10){
  row = Y[i,]
  print(row)
  actual_lhs[i,1] = pop[row[1]]
  actual_lhs[i,2] = memory[row[2]]
  actual_lhs[i,3] = startprice[row[3]]
}
actual_lhs  # LHS Combo params

###### Simulated model data.
btc = read.csv("btc.csv")
load("btc.moments.Rdata")
load("btc.weighting.Rdata")

btc.moments.orig.matrix

load("run601.Rdata")
run601.cc = diff(log(run601))
run601.simple = exp(run601.cc) - 1
run601.date = seq.int(1, 600, 1)

run601.df = data.frame(
  "date" = run601.date,
  "price" = as.vector(run601[2:length(run601)]),
  "simple" = run601.simple,
  "cc" = run601.cc
)
# write.csv(run601.df, file="run601.csv")
run601.df = read.csv("run601.csv")
run601.ts = run601.df$cc

run601.moments = calc_moments(run601.ts)
# (1 x 9) x (9 x 9)  x (9 x 1) --> int

eq12 <- function(model.moments, emp.moments,
                 weighting){
  eq12part1 = t(model.moments - emp.moments)
  eq12part2 = weighting
  eq12part3 = model.moments - emp.moments
  eq12part1 %*% eq12part2 %*% eq12part3
}
eq12(run601.moments, btc.moments.orig.matrix, btc.weighting)


# popsize 200  --> (0, 1000)
# memory 200  --> (0, 500)
# pupdate 0.50  --> (0, 1)
# startPrice 10 --> (10, 100)
# interest 0.05  --> (0, 1)
# dividend 0.5  --> (0, 10)
# pshock 0.8  --> (0, 1)
