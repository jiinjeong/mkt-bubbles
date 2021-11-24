dependencies <- c("tseries", # Times Series Data
                  "forecast", # Autocorrelation
                  "evir", # Hill
                  "lhs", # Latin Hypercube Sampling
                  "lme4", # Nelder-Mead
                  "xts"
                )
for (d in dependencies) {
    if (d %in% rownames(installed.packages()) == FALSE) {
        install.packages(d, dependencies = TRUE)
    }
    suppressPackageStartupMessages(library(d, character.only = TRUE))
}

print("Loaded libraries...")

setwd("/usr/local/research/compsci/seminar/mkt-bubbles")
# setwd("~/Desktop/mkt-bubbles")
options(scipen = 999)
set.seed(1213) # Randomness

# =================== STEP 1. Data =============================================
crypto_to_ts <- function(crypto_df) {
    crypto_df_revised <- data.frame(crypto_df$cc)
    colnames(crypto_df_revised) <- "BTC"
    rownames(crypto_df_revised) <- crypto_df$date
    crypto_ts <- xts::as.xts(crypto_df_revised)
    return(crypto_ts)
}

btc_df <- data.frame(read.csv("data/btc.csv"))
btc_df$date <- as.Date(btc_df$date)
btc_ts <- crypto_to_ts(btc_df)    # Time series data

sp500_df <- data.frame(read.csv("data/sp500.csv"))
sp500_df$date <- as.Date(sp500_df$date)
sp500_ts <- crypto_to_ts(sp500_df)    # Time series data

# =================== STEP 2. Moments ==========================================
# Hill
calc_hill <- function(crypto_ts) {
    # Power law that best approximates the decay of left tail.
    coin_left <- (-1) * crypto_ts
    # k --> More observations you use, bias.
    # (Center - decays at faster pace, farther out - slower)
    # Constant as unbiased.
    # Speed of decay.
    coin_hill_left <- hill(coin_left,
                           option = c("alpha", "xi", "quantile"),
                           start = 15, end = NA, reverse = FALSE, p = NA,
                           ci = 0.95, auto.scale = FALSE,
                           ylim = c(0, 300), xlim = c(0, 300), labels = TRUE)
    coin_huisman_df <- as.data.frame(coin_hill_left)
    # OLS Regression. (Linear model). control for k
    lm_coin <- lm(coin_huisman_df$y ~ coin_huisman_df$x)
    # Constant - proxy for hill estimator corrected for bias.
    # Exponent is 1 / hill. Report constant
    huisman_coefficient <- lm_coin$coefficients[1]
    return(huisman_coefficient)
}

# Returns a 9 x 1 matrix of 9 moments calculated.
calc_moments <- function(ts) {
    # Moment 1: Mean value of the absolute returns
    m1 <- mean(abs(ts))

    # Moment 2: First-order autocorrelation of the raw returns.
    # Daily data = 365
    raw_acf <- forecast::Acf(ts)
    lag <- 1
    m2 <- raw_acf$acf[1 + lag][1]

    # Moment 3-8: ACF of the absolute returns up to a lag of 100 days.
    # Six coefficients for the lags τ = 1, 5, 10, 25, 50, 100.
    abs_acf <- forecast::Acf(abs(ts), lag.max = 100)
    lag <- c(1, 5, 10, 25, 50, 100)
    m3 <- abs_acf$acf[1 + lag[1]][1]
    m4 <- abs_acf$acf[1 + lag[2]][1]
    m5 <- abs_acf$acf[1 + lag[3]][1]
    m6 <- abs_acf$acf[1 + lag[4]][1]
    m7 <- abs_acf$acf[1 + lag[5]][1]
    m8 <- abs_acf$acf[1 + lag[6]][1]

    # Moment 9: Hill estimator
    m9 <- as.numeric(calc_hill(ts))

    moments <- matrix(c(m1, m2, m3,
                        m4, m5, m6,
                        m7, m8, m9),
                      nrow = 9, ncol = 1)
    return(moments)
}

# =================== STEP 3. Block bootstrap & Weighting matrix ===============
# https://rdrr.io/cran/tseries/man/tsbootstrap.html

# 0. Reduce the time series.
# 1. Subdivide the time series into X non-overlapping number of blocks.
# 2. Construct a new series, from X random draws with replacement
# 3. Compute moments of the new series from 2.
# 4. Repeat steps 1-3 for 5k times. Get a frequency distribution for each of the
#    moments. (Ideally, empirical in the center)

# Returns a block bootstrap time series.
get_bootstrap <- function(ts, k, block_size) {
    return(tseries::tsbootstrap(ts, type = "block", b = block_size, nb = k))
}

# Returns a 9x1 matrix of actual moments from the empirical data (BTC, S&P500).
get_moments_emp <- function(ts) {
    return(calc_moments(ts))
}

# Returns a 9xk (5000) matrix of moments from the blocks.
get_moments_block <- function(ts, k, block_size, n_moments) {
    boot <- get_bootstrap(ts, k, block_size)
    moments_block_matrix <- matrix(nrow = n_moments, ncol = k) # 9 row x k col

    # Do a for loop and get the moments from each boot
    for (i in 1:k) {
        print(i)
        moments_block_matrix[, i] <- calc_moments(boot[, i])
    }
    return(moments_block_matrix)
}

# Returns a 9x1 mean matrix of all block moments.
get_moments_block_mean <- function(moments_block_matrix) {
        return(matrix(rowMeans(moments_block_matrix),
                      nrow = n_moments, ncol = 1))
}

# Gets a 9x9 weighting matrix. (Eq 10 & 11 in Frank-Westerhoff)
get_weighting_matrix <- function(moments_block_matrix,
                                 moments_block_mean_matrix) {
    total_sum <- matrix(rep(0, 81), nrow = 9, ncol = 9)
    for (i in 1:k) {
        mb <- moments_block_matrix[, i] - moments_block_mean_matrix
        mbar <- moments_block_mean_matrix
        each_result <- (mb - mbar) %*% t(mb - mbar)
        # (9 x 1) * (1 x 9) = (9 x 9) matrix
        total_sum <- total_sum + each_result
    }
    # Estimate of the moments' variance-covariance matrix
    sigma_hat <- total_sum / k
    weighting <- solve(sigma_hat) # Weighting matrix

    return(weighting)
}

# Checks whether the center of the moment's distribution = empirical center.
check_empirical_center <- function(moment = 1, moments_emp_matrix,
                                   moments_block_matrix) {
    # First row matrix
    plot(density(moments_block_matrix[moment, ]), main = "smoothed density",
                 type = "l", xlab = "daily return",
                 ylab = "density estimate")
    moments_emp_matrix[moment]
}

# =================== STEP 6. Optimize parameters (Nelder-Mead) ================
# Gets moments from simulated model of n_rounds.
get_moments_from_simulation <- function(simulation) {
    simulation_cc <- diff(log(simulation))
    simulation_simple <- exp(simulation_cc) - 1
    simulation_date <- seq.int(1, length(simulation) - 1, 1)

    simulation_df <- data.frame(
        "date" = simulation_date,
        "price" = as.vector(simulation[2:length(simulation)]),
        "simple" = simulation_simple,
        "cc" = simulation_cc
    )

    simulation_ts <- simulation_df$cc
    start <- 500
    if (length(simulation) <= 600) {
        start <- 500 - (600 - length(simulation))
    }
    return(calc_moments(simulation_ts[start:length(simulation) - 1]))
}

calculate_j <- function(moments_vector, weighting, moments_emp) {
    print(moments_vector)
    eq9part1 <- t(moments_vector - moments_emp)
    eq9part2 <- weighting
    eq9part3 <- moments_vector - moments_emp
    result <- eq9part1 %*% eq9part2 %*% eq9part3
    return(result)
}

count <- 1
# Gets value to minimize from Eq12 in Frank - Westerhoff
get_eq12_minimization <- function(x) {
    print("Parameters:")
    print(paste("pUpdate", x[1]))
    print(paste("dividend", x[2]))
    print(paste("pShock", x[3]))
    print(paste("shockRangeDiv", x[4]))
    print(paste("interest", x[5]))

    # Get market simulation (using Prof. Georges code)
    MO$memory <- memory
    MO$randSeed <- sample(1:2^15, 1)
    MO$lags <- lags
    MO$powers <- powers
    MO$size <- (3 + (MO$powers * MO$lags) + (((MO$lags - 1) * (MO$lags)) / 2))
    MO$shockRangeDiv <- x[4]
    MO$interest <- x[5]
    MO$dividend <- x[2]
    MO$pUpDate <- x[1]
    MO$pShock <- x[3]
    MO$bubbleThresholdLow <- 0
    MO$bubbleThresholdHigh <- 2 * (MO$dividend / MO$interest)
    market <- main(MarketObject = MO)
    moments_simulation <- get_moments_from_simulation(market)

    # (1 x 9) x (9 x 9)    x (9 x 1) --> int
    result <- calculate_j(moments_simulation, weighting, moments_emp)

    print(paste0("Minimization #", count, ": J = ", result))
    count <<- count + 1
    return(result)
}

# Runs N-M Optimize function
run_nm <- function(p) {
    nm_result <- Nelder_Mead(par = p,
                             lower = c(0, 0, 0, 0.1, 0),
                             upper = c(1, 1, 1, 1, 0.1),
                             fn = get_eq12_minimization,
                             control = list(maxfun = 50000))
    return(nm_result)
}


# =================== STEP 7. HPC Runs ===================
### Set up for HPC-1.
k <- 5000 # 5000 to match the paper
block_size <- 500 # Different block sizes are used in F&H (250, 750)
n_moments <- 9

### ====== HPC-1 ===================
generate_bootstrap <- function() {
    print("Starting bootstrap...")
    btc_bootstrap_5000 <- get_moments_block(btc_ts, k, block_size, n_moments)
    print(btc_bootstrap_5000)
    save(btc_bootstrap_5000, file = "btc_bootstrap_5000.Rdata")
    print("Saved.")
}

### Set up for HPC-2.
# Moments and weighting matrix.
load("data/btc_moments_emp.Rdata")
load("data/btc_weighting_5000.Rdata")
print("Loaded data...")

moments_emp <- btc.moments.emp.matrix
weighting <- btc.bootstrap.5000.weighting

source("/usr/local/research/compsci/seminar/mkt-bubbles/georges/singleRun.r")
# source("./georges/singleRun.r")
print("Switched to singleRun directory...")

### ====== HPC-2 ===================
args <- commandArgs(trailingOnly = TRUE)
args <- as.numeric(args)

memory <- args[1]
lags <- args[2]
powers <- args[3]
nm_args <- args[4:length(args)]

print("Running Nelder-Mead...")
nm_result <- run_nm(nm_args)
print("Done.")
