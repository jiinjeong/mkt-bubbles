# Basic stylized facts.
library(PerformanceAnalytics)
library(aTSA)  # ADF

crypto_to_ts <- function(crypto_df){
  crypto_df.revised = data.frame(crypto_df$cc)
  colnames(crypto_df.revised) = "BTC"
  rownames(crypto_df.revised) = crypto_df$date
  crypto_ts = as.xts(crypto_df.revised)
  return(crypto_ts)  # Time series!!!
}

# Q ratio
calc_q_ratio <- function(crypto_ts){
  twentyfiveperc = quantile(crypto_ts, 0.25)  # 25%... Empirical quantiles
  fiveperc = quantile(crypto_ts, 0.05)  # 5%
  q_ratio = twentyfiveperc / fiveperc
  return(q_ratio)
}

# Hurst
calc_hurst <- function(crypto_ts){
  # Computing 100-lag (1...100 periods apart, absolute returns) autocorrelation coefficient.
  # There is more linear association in abs returns?
  acf_coin =acf(abs(crypto_ts), lwd=2, type = c("correlation"), lag.max=100)
  # You expect a decay.
  acf_coin_lags = acf_coin$acf
  acf_coin_zoo = as.zoo(acf_coin_lags)
  # Power law -- decay (exponent that controls speed of decay).
  # Power law that best approximates the decay.
  hurst = hurstexp(acf_coin_zoo[2:101])
  alpha = 2 - 2 * hurst$Hs  # Exponent of power law.
  return(alpha)
}

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

# JB
calc_jb <- function(crypto_ts){
  jb = jb.test(crypto_ts)  # Jarque-Bera Test for normality
  return(jb$p.value)  # Null: Skewness = 0 & Excess kurtosis = 0
}
