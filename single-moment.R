library(tseries)  # Times Series Data
library(forecast)  # Autocorrelation
library(evir)  # Hill
library(lhs)  # Latin-hypercube
library(lme4)  # Nelder-Meade
library(xts)
options(scipen = 999)
set.seed(1213)  # Randomness
setwd("/Users/Jiin/Desktop/jiin-justin/hpc-mkt-bubbles")
# =================== Basic Fns ===================
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

# =================== Boostrap vs. Empircal ===================

load("data/btc.moments.emp.Rdata")
load("data/btc.bootstrap.5000.Rdata")
load("data/btc.weighting.5000.Rdata")

check_empirical_center <- function(moment=1, moments.emp.matrix, moments.block.matrix){
  # First row matrix
  plot(density(moments.block.matrix[moment,]), main="smoothed density", 
       type="l",xlab="daily return",
       ylab="density estimate")
  lines(density(rnorm(4, mean=50, sd=10)))
  abline(v=moments.emp.matrix[moment], col="red")
  moments.emp.matrix[moment]  # 1st item.
}

check_empirical_center(5, btc.moments.emp.matrix, btc.bootstrap.5000)

# =================== Monte Carlo Calibration Moments ===================
source("/Users/Jiin/Desktop/mkt-bubbles/georges/singleRun.r")  # Prof. Georges' Single Run Code

n_rounds = 701
n_moments = 9
k = 5

# k = num of single runs
get_mc_moments <- function(n_moments, k){
  moments.montecarlo.matrix = matrix(nrow=n_moments, ncol=k)  # 9 row x k col
  
  # Do a for loop and get the moments from each single run
  for (i in 1:k) {
    print(i)
    MO$numRounds = n_rounds
    market = main(MarketObject = MO)
    moments.montecarlo.matrix[,i] = get_moments_from_simulation(market, n_rounds)
  }
  return(moments.montecarlo.matrix)
}

mc.moments = get_mc_moments(n_moments, k)
save(mc.moments, file="/Users/Jiin/Desktop/jiin-justin/hpc-mkt-bubbles/mc.moments.Rdata")
load("mc.moments.Rdata")

# =================== Bootstrap vs. MONTE-CARLO CALIBRATION vs. Empirical center  ===================
line_width = 1.2
moment_names = c("Mean of abs returns", "Raw ACF",
                 "Abs ACF lag1", "Abs ACF lag5",
                 "Abs ACF lag10", "Abs ACF lag25",
                 "Abs ACF lag50", "Abs ACF lag100", "Hill")
desired_moment = 1
d1 = density(mc.moments[desired_moment,])
d2 = density(btc.bootstrap.5000[desired_moment,])

par(mfrow=c(1,1))
plot(d1, type="l", col="blue", lwd=line_width,
     xlim=c(min(d1$x, d2$x), max(d1$x, d2$x)),
     ylim=c(0,max(d1$y, d2$y)),
     xlab="",
     ylab="",
     main=paste(moment_names[desired_moment], ": Single Moment Matching"))
lines(d2, col="green", lwd=line_width)
abline(v=btc.moments.emp.matrix[desired_moment], col="red", lwd=line_width)
legend( x="topleft", 
        legend=c("Bootstrap","Monte-Carlo", "Empirical"), 
        col=c("green","blue","red"), lwd=line_width, merge=FALSE )

##### FOR LOOP
par(mfrow=c(3,3))
line_width = 2

for (desired_moment in 1:9){
  print(desired_moment)
  d1 = density(mc.moments[desired_moment,])
  d2 = density(btc.bootstrap.5000[desired_moment,])
  
  plot(d1, type="l", col="blue4", lwd=line_width,
       xlim=c(min(d1$x, d2$x), max(d1$x, d2$x)),
       ylim=c(0,max(d1$y, d2$y)),
       xlab="",
       ylab="",
       main=paste(moment_names[desired_moment]))
  lines(d2, col="green4", lwd=line_width)
  abline(v=btc.moments.emp.matrix[desired_moment], col="red", lwd=line_width)
}
