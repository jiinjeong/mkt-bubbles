library(tseries)  # Times Series Data
library(forecast)  # Autocorrelation
library(evir)  # Hill
library(lhs)  # Latin-hypercube
library(lme4)  # Nelder-Meade
library(xts)

setwd("/Users/Jiin/Desktop/mkt-bubbles")  # This Code Directory
options(scipen = 999)
set.seed(1213)  # Randomness

# =================== STEP 1. Data ===================
crypto_to_ts <- function(crypto_df){
  crypto_df.revised = data.frame(crypto_df$cc)
  colnames(crypto_df.revised) = "BTC"
  rownames(crypto_df.revised) = crypto_df$date
  crypto_ts = as.xts(crypto_df.revised)
  return(crypto_ts)  # Time series!!!
}

btc.df = data.frame(read.csv("data/btc.csv"))
btc.df$date = as.Date(btc.df$date)
btc.ts = crypto_to_ts(btc.df)  # Time series data

sp500.df = data.frame(read.csv("data/sp500.csv"))
sp500.df$date = as.Date(sp500.df$date)
sp500.ts = crypto_to_ts(sp500.df)  # Time series data

# =================== STEP 2. Moments ===================
# Hill
calc_hill <- function(crypto_ts){
  # Power law that best approximates the decay of left tail.
  coin_left = (-1) * crypto_ts
  # k --> More observations you use, bias. (Center - decays at faster pace, farther out - slower)
  # Constant as unbiased.
  # Speed of decay.
  coin_hill_left = hill(coin_left,
                        option = c("alpha","xi","quantile"),
                        start = 15, end = NA, reverse = FALSE, p = NA,
                        ci = 0.95, auto.scale = TRUE, labels = TRUE)
  coin_huisman.df = as.data.frame(coin_hill_left)
  # OLS Regression. (Linear model). control for k
  lm_coin = lm(coin_huisman.df$y ~ coin_huisman.df$x)
  # Constant - proxy for hill estimator corrected for bias.
  # Exponent is 1 / hill. Report constant
  huisman_coefficient = lm_coin$coefficients[1]
  return(huisman_coefficient)
}

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

# =================== STEP 3. Block bootstrap & Weighting matrix ===================
# https://rdrr.io/cran/tseries/man/tsbootstrap.html

# 0. Reduce the time series.
# 1. Subdivide the time series into X non-overlapping number of blocks.
# 2. Construct a new series, from X random draws with replacement
# 3. Compute moments of the new series from 2.
# 4. Repeat steps 1-3 for 5k times. Get a frequency distribution for each of the moments. (Ideally, empirical in the center)

# Returns a block bootstrap time series.
get_bootstrap <- function(ts, k, block_size){
  return(tsbootstrap(ts, type="block", b=block_size, nb=k))
}
#get_bootstrap(btc.ts, k, block_size)

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
# Three parameters to begin with and their range, step size.

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
# Gets moments from simulated model of n_rounds.
get_moments_from_simulation <- function(simulation, n_rounds){
  print(simulation)
  simulation.cc = diff(log(simulation))
  simulation.simple = exp(simulation.cc) - 1
  simulation.date = seq.int(1, n_rounds - 1, 1)
  
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

calculate_j <- function(moments.vector, weighting, moments.emp){
  eq9part1 = t(moments.vector - moments.emp)
  eq9part2 = weighting
  eq9part3 = moments.vector - moments.emp
  result = eq9part1 %*% eq9part2 %*% eq9part3
  return(result)
}

# Gets value to minimize from Eq12 in Frank - Westerhoff
get_eq12_minimization <- function(hi){
  print("Parameters")
  print(hi)
  
  MO$numRounds = n_rounds
  print(n_rounds)
  # Get market simulation (using Prof. Georges code)
  MO$memory = round(hi[1])
  MO$pUpDate = hi[2]
  MO$dividend = hi[3]
  MO$pShock = hi[4]
  MO$shockRangeDiv = hi[5]
  MO$lags = round(hi[6])
  MO$powers = round(hi[7])
  MO$randSeed = 3
  
  market = main(MarketObject = MO)
  moments.simulation = get_moments_from_simulation(market, n_rounds)
  
  # (1 x 9) x (9 x 9)  x (9 x 1) --> int
  result = calculate_j(moments.simulation, weighting, moments.emp)
  
  print("Minimization #")
  print(result)
  return(result)
}

# Runs N-M Optimize function
run_nm <- function(){
  nm_result = Nelder_Mead(par=c(10, 0.6, 0.8, 0.3, 0.5, 2, 2, 0.08),
                          lower=c(0, 0, 0, 0, 0, 1, 0, 0),
                          upper=c(100, 1, 1, 1, 1, 3, 3, .1),
                          fn=get_eq12_minimization, 
                          control=list(maxfun=3))
  return(nm_result)
}


# =================== STEP 7. Things we can run in HPC ===================
### Set up for HPC-1.
k = 3  # 5000 to match the paper
block_size = 500  # Diff block sizes later on (250, 750)
n_moments = 9

### ====== HPC-1. One thing we can run in HPC
btc.bootstrap.5000 = get_moments_block(btc.ts, k, block_size, n_moments)  # k = 5000
save(btc.bootstrap.5000, file="btc.bootstrap.5000.Rdata")

### Set up for HPC-2.
# Moments and weighting matrix.
load("data/btc.moments.emp.Rdata")
load("data/btc.moments.block.100.Rdata")  # Need to replace this with HPC-1 result.
load("data/btc.weighting.Rdata")  # Need to replace this with HPC-1 result.

moments.emp = btc.moments.emp.matrix
weighting = btc.weighting

# Latin hypercubes
source("/Users/Jiin/Desktop/mkt-bubbles/georges/singleRun.r")  # Prof. Georges' Single Run Code

n_param = 3
pop = seq.int(0, 1000, 1)
memory = seq.int(0, 500, 5)
startprice = seq.int(10, 100, 10)
n_lhc_set = 10  # 10 combos of Latin Hypercube param sets

# Simulation params
n_rounds = 1001  # Simulation
#MO$numAgents = 200
#MO$memory = 2
#MO$startPrice = 10
MO$numRounds = n_rounds

hi = c(100, 0.6, 0.8, 0.3, 0.5, 2, 2, 0.08)
MO$memory = round(hi[1])
MO$pUpDate = hi[2]
print(hi[2])
MO$dividend = hi[3]
MO$pShock = hi[4]
MO$shockRangeDiv = hi[5]
MO$lags = round(hi[6])
MO$powers = round(hi[7])
MO$randSeed = sample(1:2^15, 1)
print(sample(1:2^15, 1))
# Test simulation run
market = main(MarketObject = MO)
moments.simulation = get_moments_from_simulation(market, n_rounds)

### ====== HPC-2. Second thing we can run in HPC (We need output from HPC-1.)
# Need to change the Nelder Mead starting parameters? (Latin Hypercube, max_runs = 3 --> 5000 or more)
nm_result = run_nm()
