library(lhs)  # Latin-hypercube

# Gets initial params through Latin Hypercube
get_initial_params <- function(n_lhc_set, n_param){
  X <- randomLHS(n = n_lhc_set, k = n_param)
  Y <- X
  lengths <- c(length(memories),
               length(pupdates),
               length(dividends),
               length(pshocks),
               length(shockRangeDivs),
               length(lags),
               length(powers),
               length(interests))
  # These are indices
  for (i in 1:n_param){
    Y[,i] <- 1 + floor(Y[,i] * lengths[i])  # Store in each column (Column 1: Pop, 2: Memory, 3: StartPrice)
  }
  # These are the actual params
  actual_lhs = matrix(nrow=n_lhc_set, ncol=n_param)
  for (i in 1:n_lhc_set){
    row = Y[i,]
    actual_lhs[i,1] = memories[row[1]]
    actual_lhs[i,2] = pupdates[row[2]]
    actual_lhs[i,3] = dividends[row[3]]
    actual_lhs[i,4] = pshocks[row[4]]
    actual_lhs[i,5] = shockRangeDivs[row[5]]
    actual_lhs[i,6] = lags[row[6]]
    actual_lhs[i,7] = powers[row[7]]
    actual_lhs[i,8] = interests[row[8]]
  }
  return(actual_lhs)  # LHC Combo params
}

n_param = 8
memories = seq.int(0, 100, 10)
pupdates = seq(0, 1, 0.1)
dividends = seq(0, 1, 0.1)
pshocks = seq(0, 1, 0.1)
shockRangeDivs = seq(0.1, 1, 0.1)
lags = seq(1, 3, 1)
powers = seq(0, 3, 1)
interests = seq(0, .1, 0.01)

n_lhc_set = 10  # 10 combos of Latin Hypercube param sets
print(get_initial_params(n_lhc_set, n_param))
