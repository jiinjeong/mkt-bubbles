library(geckor)  # Collector for Coingecko API
library(ggplot2)  # Plotting
library(tseries)  # Times Series Data
library(forecast)  # Autocorrelation

source("/Users/Jiin/Desktop/market_sim_ml_bubbles/core/singleRun.r")  # Prof. Georges' Single Run Code
setwd("/Users/Jiin/Desktop/jiin-justin/mkt-bubbles")  # This Code Directory
source("common.R")  # Fns for stylized facts
options(scipen = 999)
set.seed(1213)  # Randomness

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


# =================== STEP 2. Basic Plots and Stats ===================
basic_plots <- function(df){
    # Time series plot of historical Bitcoin price and returns
    price_plot <- ggplot(df, aes(x=date, y=price)) + geom_line()
    return_plot <- ggplot(df, aes(x=date)) + 
      geom_line(aes(y=cc), color="red") +
      geom_line(aes(y=simple), color="black")

    print(price_plot)
    print(return_plot)

    # Histogram + QQPlot + More
    par(mfrow=c(2,2))
    hist(df$cc,main="Daily Returns",
         xlab="btc", probability=T, col="slateblue1")
    boxplot(df$cc,outchar=T,col="slateblue1")
    plot(density(df$cc), main="smoothed density", 
         type="l",xlab="daily return",
         ylab="density estimate")
    qqnorm(df$cc)
    qqline(df$cc)
    par(mfrow=c(1,1))
}

basic_stats <- function(ts){
    sd(ts)
    skewness(ts)
    kurtosis(ts)
    summary(ts)
    
    calc_q_ratio(ts)
    calc_hurst(ts)
    calc_hill(ts)
    calc_jb(ts)
}

# =================== STEP 3. Moments ===================
# Returns a 9 x 1 matrix of 9 moments calculated.
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


# =================== STEP 4. Block bootstrap & Weighting matrix ===================
# https://rdrr.io/cran/tseries/man/tsbootstrap.html

# 0. Reduce the time series.
# 1. Subdivide the time series into X non-overlapping number of blocks.
# 2. Construct a new series, from X random draws with replacement
# 3. Compute moments of the new series from 2.
# 4. Repeat steps 1-3 for 5k times. Get a frequency distribution for each of the moments. (Ideally, empirical in the center)

k = 10  # 5000 to match the paper
block_size = 500  # Diff block sizes later on (250, 750)
n_moments = 9

# Returns a block bootstrap time series.
get_bootstrap <- function(ts, k, block_size){
    return(tsbootstrap(ts, type="block", b=block_size, nb=k))
}

# Returns a 9 x 1 matrix of actual moments from the empirical data (BTC, S&P500).
get_moments_emp <- function(ts){
  return(calc_moments(ts))
}

# Returns a 9 x k (5000) matrix of moments from the blocks.
get_moments_block <- function(ts, k, block_size, n_moments){
  boot = get_bootstrap(ts, k, block_size)
  moments.block.matrix = matrix(nrow=n_moments, ncol=k)  # 9 row x k col

  # Do a for loop and get the moments from each boot
  for (i in 1:k) {
    print(i)
    moments.block.matrix[,i] = calc_moments(boot[,i])
  }
  # save(moments.block.matrix, file="moments.block.Rdata")
  # load("moments.block.Rdata")
  return(moments.block.matrix)
}

# Returns a 9 x 1 mean matrix of all block moments.
get_moments_block_mean <- function(moments.block.matrix){
    return(matrix(rowMeans(moments.block.matrix),
                  nrow=n_moments, ncol=1))
}

# Gets a 9 x 9 weighting matrix. (Eq 10 & 11 in Frank-Westerhoff)
get_weighting_matrix <- function(moments.block.matrix, moments.block.mean.matrix){
    total_sum = matrix(rep(0, 81), nrow=9, ncol=9)
    for (i in 1:k) {
      mb = moments.block.matrix[,i] - moments.block.mean.matrix
      mbar = moments.block.mean.matrix
      each_result = (mb - mbar) %*% t(mb - mbar)
      # print(each_result)
      # (9 x 1) * (1 x 9) = (9 x 9) matrix
      total_sum = total_sum + each_result
    }
    sigma_hat = total_sum / k  # Estimate of the moments' variance-covariance matrix
    weighting = solve(sigma_hat)  # Weighting matrix

    return(weighting)
}

# Checks whether the center of the desired moment's frequency distribution = empirical center.
check_empirical_center <- function(moment=1, moments.emp.matrix, moments.block.matrix){
  # First row matrix
  plot(density(moments.block.matrix[moment,]), main="smoothed density", 
       type="l",xlab="daily return",
       ylab="density estimate")
  moments.emp.matrix[moment]  # 1st item.
}

# =================== STEP 5. Initialize parameters (Latin Hypercube) ===================
# LHS: https://www.rdocumentation.org/packages/pse/versions/0.4.7/topics/LHS
library(lhs)

# popsize 200  --> (0, 1000) --> 1
# memory 200  --> (0, 500)  --> 5
# pupdate 0.50  --> (0, 1)
# startPrice 10 --> (10, 100) --> 10
# interest 0.05  --> (0, 1)
# dividend 0.5  --> (0, 10)
# pshock 0.8  --> (0, 1)

# Three parameters to begin with and their range, step size.
n_param = 3
pop = seq.int(0, 1000, 1)
memory = seq.int(0, 500, 5)
startprice = seq.int(10, 100, 10)
# params <- expand.grid(pop, memory, startprice)
n_lhc_set = 10  # 10 combos of Latin Hypercube param sets

# Gets initial params through Latin Hypercube
get_initial_params <- function(n_lhc_set, n_param){
  X <- randomLHS(n = n_lhc_set, k = n_param)
  Y <- X
  lengths <- c(length(pop), length(memory), length(startprice))
  # These are indices
  for (i in 1:n_param){
    Y[,i] <- 1 + floor(Y[,i] * lengths[i])  # Store in each column (Column 1: Pop, 2: Memory, 3: StartPrice)
  }
  # These are the actual params
  actual_lhs = matrix(nrow=n_lhc_set, ncol=n_param)
  for (i in 1:n_lhc_set){
    row = Y[i,]
    actual_lhs[i,1] = pop[row[1]]
    actual_lhs[i,2] = memory[row[2]]
    actual_lhs[i,3] = startprice[row[3]]
  }
  return(actual_lhs)  # LHC Combo params
}


# =================== STEP 6. Optimize parameters (Nelder-Mead) ===================
# Nelder Mead: https://www.rdocumentation.org/packages/lme4/versions/1.1-13/topics/NelderMead

n_rounds = 600

# Gets moments from simulated model of n_rounds.
get_moments_from_simulation <- function(simulation, n_rounds){
    simulation.cc = diff(log(simulation))
    simulation.simple = exp(simulation.cc) - 1
    simulation.date = seq.int(1, n_rounds, 1)
    
    simulation.df = data.frame(
      "date" = simulation.date,
      "price" = as.vector(simulation[2:length(simulation)]),
      "simple" = simulation.simple,
      "cc" = simulation.cc
    )
    simulation.ts = simulation.df$cc
    simulation.moments = calc_moments(simulation.ts[500:length(simulation) - 1])
    return(simulation.moments)
}

# Gets value to minimize from Eq12 in Frank - Westerhoff
get_eq12_minimization <- function(x){
  print("Parameters")
  print(x)
  
  # Get market simulation (using Prof. Georges code)
  MO$numAgents = round(x[1])
  MO$memory = round(x[2])
  MO$startPrice = x[3]
  market = main(MarketObject = MO)
  moments.simulation = get_moments_from_simulation(market, n_rounds)
  
  eq12part1 = t(moments.simulation - moments.emp)
  eq12part2 = weighting
  eq12part3 = moments.simulation - moments.emp
  # (1 x 9) x (9 x 9)  x (9 x 1) --> int
  result = eq12part1 %*% eq12part2 %*% eq12part3

  print("Minimization #")
  print(result)
  return(result)
}

# Nelder Mead Optimize
library(lme4)
# Runs N-M Optimize function
run_nm <- function(){
    nm_result = Nelder_Mead(par=c(487, 140, 90),
                           lower=c(0, 0, 10),
                           upper=c(1000, 500, 100),
                           fn=get_eq12_minimization, 
                           control=list(maxfun=3,
                                        xst=c(5, 5, 1)))
    return(nm_result)
}

# =================== STEP 7. Run (Reusing Data) ===================
btc.df = data.frame(read.csv("data/btc.csv"))
btc.df$date = as.Date(btc.df$date)
btc.ts = crypto_to_ts(btc.df)  # Time series data
sp500.df = data.frame(read.csv("data/sp500.csv"))
sp500.df$date = as.Date(sp500.df$date)
sp500.ts = crypto_to_ts(sp500.df)  # Time series data

load("data/btc.moments.emp.Rdata")
load("data/btc.moments.block.100.Rdata")
load("data/btc.weighting.Rdata")

check_empirical_center(5, btc.moments.emp.matrix, btc.moments.block.100.matrix)

moments.emp = btc.moments.emp.matrix
weighting = btc.weighting
run_nm()
