##############################################
# Document: Functions.r
# Purpose: Contains the functions needed for
# running linear simulations
# Functions:
#   1. predictT
#   2. updateMatrix
#   3. marketPrice
#   4. MSFE
#   5. hetero_MSFE
#   6. calculateRepAgent
#   7. estParams
#   8. predictPriceDividend
#   9. dependdencyCheck
#   10. GetMacros
#   11. printSomething
# ToDo:
##############################################

predictT = function(matrixData, params, round) {
  # Get most recent COMPLETE row of data
  pred_inputs = matrixData[which(index(matrixData) == round - 1), ]
  x_hat_t = pred_inputs  %*% (params) # changed p to x 7/17/17
  return(x_hat_t) # changed p to x 7/17/17
}

updateMatrix = function(round, MarketObject) {
  # Grab Most Recent 50 Price (and Div)
  # Chanced Price.. to PriceDiv.. 7/17/17

  if (round <= MarketObject$memory + (MarketObject$lags + 1)) {
    PriceDivVec = MarketObject$xx[1:round]
    #printSomething("PriceDivVec:") #test added temp 3/4/21
    #printSomething(PriceDivVec) #test added temp 3/4/21
    # early periods -- get all available price data
    mem = seq(1, round, 1)
  } else {
    # So early periods get lagged values ie. length(54)
    # Changed PriceVec to PriceDivVec 7/17/17
    PriceDivVec = MarketObject$xx[(round - MarketObject$memory - (MarketObject$lags + 1)):round]

    # later periods -- bounded memory
    mem = seq((round - MarketObject$memory - (MarketObject$lags + 1)), round, 1) #length(54)
  }

  # column 1 (AKA x_0):

  intercept = rep(1, length(mem))
  intercept = zoo(intercept, order.by = mem)

  # Turn data into zoo (ts) objects
  # Changed PriceVec to PriceDivVec 7/17/17
  TS <- zoo(x = PriceDivVec, order.by = mem)
  #printSomething("TS:") #test added temp 3/7/21
  #printSomething(TS) #test added temp 3/7/21

  # Create list object to store zoo objects, to merge later
  lag_list <- list(intercept=intercept, TS=TS)
  # Store the current lag
  prev_lag <- TS
  #printSomething("prev_lag:") #test added temp 3/7/21
  #printSomething(prev_lag) #test added temp 3/7/21
  # Store all previous lags, for cross products later
  prev_lag_list <- list() #
  #printSomething("lag_list:") #test added temp 3/7/21
  #printSomething(lag_list) #test added temp 3/7/21
  #printSomething("prev_lag_list:") #test added temp 3/7/21
  #printSomething(prev_lag_list) #test added temp 3/7/21

  # Main loop, add each lag to lag_list, and powers for each lag
  for (i in 1:MarketObject$lags){
    #prev_lag <- lag(prev_lag, k=1, na.pad = TRUE) #3/7/21 this is the original
    #prev_lag <- lag(prev_lag, k=-1, na.pad = TRUE) #3/7/21 try k=-1 did nothing
    prev_lag <- lag.xts(prev_lag, k=1, na.pad = TRUE) #3/7/21 try lag.xts yes!
    #needed to change for updated R and packages due to new conflict
    #between base R and xts or zoo sometime since 2019?? #3/7/21

    prev_lag_list <- list.append(prev_lag_list, i = prev_lag)
    lag_list <- list.append(lag_list, i = prev_lag)

    if (MarketObject$powers > 1) {
      for (j in 2:MarketObject$powers){
        lag_list <- list.append(lag_list, j = prev_lag ** j)
      }
    }
  }

  # Use prev_lag_list to get cross products, add to lag_list
  if (MarketObject$lags > 1){
    for (lag1 in 1:(length(prev_lag_list)-1)){
      for (lag2 in (lag1+1):length(prev_lag_list)){
        lag_list <- list.append(lag_list, inter = (prev_lag_list[lag1]$i) * (prev_lag_list[lag2]$i))
      }
    }
  }

  # Merge all zoo objects in lag_list, and return
  MatrixUpdate = do.call("merge", lag_list)
  MatrixUpdate = window(MatrixUpdate, seq((round - MarketObject$memory - 1), round, 1))
  return(MatrixUpdate)
}

marketPrice = function(matrixData, marketParams, round, MarketObject) {
  # take mean of Price + Dividend

  # Similar to forecast(i): Dot-Product of MKT-Params[2-8]=beta1 and
  # UpdateMatrix[1]

  mkt_inputs = matrix(tail(matrixData, 1), ncol = MarketObject$size - 1)
  #printSomething("mkt_inputs:")#test added temp 3/4/21
  #printSomething(mkt_inputs) #test added temp 3/4/21

  # dot prod
  X_hat_t1 = mkt_inputs %*% (marketParams) # changed P to X 7/17/17


  # R.E.E. mean

  # Initialize market price without risk aversion
  market_price = X_hat_t1 / (1 + MarketObject$interestRates[round-1])

  # If you are past linit and including risk, reset market price
  # Should start at linit + 1
  if (MarketObject$riskType > 0) {   # Risk type = 0 means no endogenous risk
    if (round - 1 > MarketObject$lInit) {
      if (MarketObject$riskType == 1) { # Risk type = 1 means homogeneous risk
        market_price = (X_hat_t1 + (MarketObject$riskConstant * MSFE(matrixData, round, MarketObject$oldRep[2:size], MarketObject))) / (1 + MarketObject$interestRates[round-1])
      } else {    # Risk type = 2 means heterogeneous risk
        market_price = (X_hat_t1 + (MarketObject$riskConstant * hetero_MSFE(matrixData, round, MarketObject))) / (1 + MarketObject$interestRates[round-1])
      }
    }
  }

  return(market_price)
}

MSFE = function(matrixData, round, agent, MarketObject) {
  sum_sq_error = 0

  # Index to start calculating MSFE at
  start_index = MarketObject$lInit

  # Don't want to look further back than memory
  # if the period is (memory) past linit or more, then only go back memory amount
  if (round - MarketObject$memory >= start_index) {
    start_index = round - MarketObject$memory + 1
  }

  for (period in start_index: round - 1) {
    period_prediction = predictT(matrixData = matrixData, params = agent, round = period)
    error_sq = (MarketObject$prices[period] - period_prediction)^2
    sum_sq_error = sum_sq_error + error_sq
  }
  MSFE = (sum_sq_error / (round - 1 - start_index))
  return(MSFE)
}

hetero_MSFE = function(matrixData, round, MarketObject) {
  # Computes a risk aversion value by calculating the mean squared
  # forecast error for [memory] forecasts back.  Instead of using the RepAgent
  # to get the MSFE, we calculate it for all of the agents in the market.
  # NOTE: This will be much slower than homogeneous risk function because it has to calculate
  # MSFE for all agents, not just one.
  total_msfe = 0

  for (i in 1:nrow(MarketObject$marketMatrix)) {
    total_msfe = total_msfe + MSFE(matrixData, round, MarketObject$marketMatrix[i, 2:MarketObject$size])
  }

  avg_msfe = total_msfe / nrow(MarketObject$marketMatrix)
  return (avg_msfe)
}

calculateRepAgent = function(MarketObject, round) {
  if (MarketObject$runType < 8) {
    # take mean over columns 2:8 of market
    mkt = data.frame(MarketObject$marketMatrix)
    mean_params = c()
    for (i in 2:MarketObject$size){
      mean_params <- c(mean_params, mean(mkt[, i], na.rm = TRUE))
    }
    return(mean_params)
  } else {
    totalPriceDiv = 0
    totalConnections = 0
    count = 1
    for (optimalAgent in MarketObject$optimalAgents) {
      if (optimalAgent$connections == 0) {
        # indicate that we stopped using this OA after last round
        optimalAgent$endUse = (round - 1)

        # if the optimalAgent was used during period of interest
        if (optimalAgent$flagged()) {
          # store OA in list
          MarketObject$storeOA(optimalAgent)
        }

        # use defined function (4/19/21)
        #MarketObject$optimalAgents = list.remove(MarketObject$optimalAgents, count)
        MarketObject$removeOptimalAgent(count)

      } else {
        # predict current price
        EX_t = optimalAgent$predict(MarketObject = MarketObject)

        if (forecast2ahead == 0) {
          # predict price of next period
          if (is.null(optimalAgent$predictors)) {
            X_t1 = 10.5
          } else {
            x1 <- EX_t
            df = data.frame(x1)
            for (predictor in 2:numberPredictors) {
              df[paste("x", toString(predictor), sep="")] = MarketObject$xx[length(MarketObject$xx) - predictor + 2]
            }
            # normalize inputs
            df = normalizeData(df)

            X_t1 = predictPriceDividend(optimalAgent$predictors, df)
          }
        } else {
          X_t1 = EX_t
        }

        # add price to sum
        totalPriceDiv = totalPriceDiv + (X_t1 * optimalAgent$connections)
        totalConnections = optimalAgent$connections + totalConnections

        # if we're between recordOA_start and recordOA_end
        if (between(round, recordOA_start, recordOA_end)) {
          # store number of connections in matrix
          MarketObject$storeOA_connections(round, optimalAgent)
        }
        count = count + 1
      }
    }
    return(totalPriceDiv / MarketObject$numAgents)
  }
}

estParams = function(new_matrix, round, MarketObject) {
  # change
  if (MarketObject$runType < 8) {
    Y = new_matrix[2:which(index(new_matrix) == round - 1), 2]

    X = matrix(new_matrix, ncol = MarketObject$size - 1)

    #convert data to matrices
    Y = matrix(Y, ncol= 1)
    colnames(Y)[1] <- "Y"

    MATRIX = X[1:(nrow(X) - 2),]
  }

  ##########
  # Note: runType is set in input-lm-v1.2.txt
  #
  # 0: LM
  # 1: glmnet with lambda = 0 (similar to LM)
  # 2: glmnet with lambda = 1
  # 3: glmnet with lambda by cross validation (lasso: alpha = 1)
  # 4: stepwise selection (forward, backward, exhaustive)
  # 5: glmnet with cv (elastic net: alpha = .5) #7/30/18
  # 6: glmnet with cv (ridge: alpha = 0) #8/15/18
  # 7: glmnet with cv (near lasso: alpha = 0.95)
  # 8: Smoothing Spline with LM
  ##########

  if (MarketObject$runType == 0) {
    # LM (standard OLS)

    # Creating dataset to run ols
    #print(length(matrix(Y, ncol= 1)))
    #print(length(matrix(X, ncol=2)))
    x_y = as.data.frame(cbind(Y, MATRIX))
    regression = lm(formula = Y ~ ., data = x_y)

    # Retrieving regression coefficients
    test_val = (as.matrix(summary(regression)$coefficients[,1],
                          nrow = MarketObject$size,
                          ncol = 1))
    return (test_val)
  }
  else if (MarketObject$runType == 1) {
    # Standard glmnet (elastic net) w/o CV, lambda = 0
    # in principle equivalent to standard lm regression
    return (coef(glmnet(x = MATRIX[,2:ncol(MATRIX)],
                        y = Y,
                        alpha = 0.9,
                        # list should contain at least 3 lambda values
                        lambda = seq(10,0,-0.1)),
                 # s controls which lambda value is chosen from the list
                 s = 0))
  }
  else if (MarketObject$runType == 2) {
    # Standard glmnet (elastic net) w/o CV - for testing
    return (coef(glmnet(x = MATRIX[,2:ncol(MATRIX)],
                        y = Y,
                        alpha = 0.9,
                        # list should contain at least 3 lambda values
                        lambda = seq(100, 0, -1)),
                 # s controls which lambda value is chosen from the list
                 s = .1))
  }
  else if (MarketObject$runType == 3) {
    # CV glmnet lasso (alpha = 1, lambda set by 10 fold CV)
    model <- cv.glmnet(x = MATRIX[,2:ncol(MATRIX)],
                       y = Y,
                       alpha = 1,
                       nfolds = 10,   # might need to be more dynamic
                       family = "gaussian")

    # store the penalty parameter with the best cv value
    bestLambda <- model[["lambda.min"]]
    MarketObject$update_histPenalty(bestLambda)

    return (coef(model,
                 # s = "lambda.min" chooses the lambda with the best cv value
                 s = "lambda.min"))
  }
  else if (MarketObject$runType == 4) {
    #stepwise selection (forward, backward, exhaustive)
    HybridAgent = rep(0,MarketObject$size)
    # Perform the hybrid model selection regression
    regfit_hybrid = regsubsets(x = MATRIX[,2:ncol(MATRIX)],
                               y = Y, nvmax = MarketObject$size,
                               method = (if(MarketObject$selectionType == 0)
                                 "forward"
                                 else if(MarketObject$selectionType == 1)
                                   "backward"
                                 else if (MarketObject$selectionType == 2)
                                   "exhaustive"))
    # Rertrive the coefficients from the regression
    # Selects the model with minimized BIC (Bayesian Information Criterion)
    hybrid_coefs_bic = coef(regfit_hybrid,
                            which.min(summary(regfit_hybrid, best = TRUE)$bic))

    # The following code is necessary due to the way the coefficients are returned by coef()
    # An exmaple of the return format of the coefficients:
    #
    # BIC Coefficients
    # (Intercept)            4            8           11           13
    # 127.51242265   0.05735865 -17.88244908  -1.69758103   1.73587485
    #
    # So some modification is needed to ensure the coefficients are
    # assigned to the correct positions in the matrix

    # assign the coefficients to their correct positions in the model
    HybridAgent[1] = hybrid_coefs_bic[1]
    for (n in 2:length(hybrid_coefs_bic)) {
      # get the positional number of the coefficient
      names(hybrid_coefs_bic)[n] = utf8ToInt(names(hybrid_coefs_bic)[n]) - 96
      # assign it to the correct position in the overall model
      HybridAgent[(as.numeric(names(hybrid_coefs_bic)[n])+1)] = hybrid_coefs_bic[names(hybrid_coefs_bic)[n]]
    }

    return (HybridAgent)
  }

  else if (MarketObject$runType == 5) {
    # CV glmnet elastic net (between ridge and lasso, alpha = 0.5)
    model <- cv.glmnet(x = MATRIX[,2:ncol(MATRIX)],
                       y = Y,
                       alpha = 0.5,
                       nfolds = 10,   # might need to be more dynamic
                       family = "gaussian")

    # store the penalty parameter with the best cv value
    bestLambda <- model[["lambda.min"]]
    MarketObject$update_histPenalty(bestLambda)

    return (coef(model,
                 # s = "lambda.min" chooses the lambda with the best cv value
                 s = "lambda.min"))
  }

  else if (MarketObject$runType == 6) {
    # CV glmnet ridge (alpha = 0)
    model <- cv.glmnet(x = MATRIX[,2:ncol(MATRIX)],
                       y = Y,
                       alpha = 0,
                       nfolds = 10,   # might need to be more dynamic
                       family = "gaussian")

    # store the penalty parameter with the best cv value
    bestLambda <- model[["lambda.min"]]
    MarketObject$update_histPenalty(bestLambda)

    return (coef(model,
                 # s = "lambda.min" chooses the lambda with the best cv value
                 s = "lambda.min"))
  }

  else if (MarketObject$runType == 7) {
    # CV glmnet near-lasso (alpha = 0.95)
    model <- cv.glmnet(x = MATRIX[,2:ncol(MATRIX)],
                       y = Y,
                       alpha = 0.95,
                       nfolds = 10,   # might need to be more dynamic
                       family = "gaussian")

    # store the penalty parameter with the best cv value
    bestLambda <- model[["lambda.min"]]
    MarketObject$update_histPenalty(bestLambda)

    return (coef(model,
                 # s = "lambda.min" chooses the lambda with the best cv value
                 s = "lambda.min"))
  }

  else if (MarketObject$runType == 8) {
    #smooth spline

    x_y = as.data.frame(cbind(Y, MATRIX))
    smoothSpline <- smooth.spline(as.vector(x_y[,3]),as.vector(x_y[,1]), df=6)
    print(x_y)
    x_y[,1] = predict(smoothSpline, x=as.vector(x_y[,3]))$y
    regression = lm(formula = Y ~ ., data = x_y)
    print(smoothSpline)

    # Retrieving regression coefficients
    test_val = (as.matrix(summary(regression)$coefficients[,1],
                          nrow = MarketObject$size,
                          ncol = 1))

    return (test_val)
  }

  else if (MarketObject$runType == 9) {
    #neural network (using nuuralnet package)
    #note that for runTypes > 8, see predict in Agents.r #3/8/21

    # if we are forecasting two ahead, shift predictors by 1
    shift <- 0
    if (forecast2ahead == 1) {
      shift <- 1
    }

    # define training data
    temp = seq(1, MarketObject$memory, 1)
    df = data.frame(temp)
    for (predictor in 1:numberPredictors) {
      df[paste("x", toString(predictor), sep="")] = MarketObject$xx[((length(MarketObject$xx) - 1) - (MarketObject$memory - 1) + (1 - predictor) - shift) : ((length(MarketObject$xx) - 1) - (predictor - 1) - shift)]
    }
    df = subset(df, select = -c(temp))
    # normalize inputs
    df = normalizeData(df)
    #print(df) #commented out #3/10/21
    #stop()  #this line was generating error #3/10/21
    formula = as.formula(paste("label ~ x", paste(seq(1,numberPredictors, 1), collapse = " + x"), sep=""))
    label = MarketObject$xx[(length(MarketObject$xx) - (MarketObject$memory - 1)) : length(MarketObject$xx)]
    df['label'] = label
    customFunction <- function(x) log(1 + exp(x))
    nn = neuralnet(formula, data=df, hidden = 3, threshold = 0.08,
                   stepmax = 1e+05, rep = 1, startweights = NULL,
                   learningrate.limit = NULL, learningrate.factor = list(minus = 0.5,plus = 1.2),
                   learningrate = NULL, lifesign = "none",
                   lifesign.step = 1000, algorithm = "rprop+", err.fct = "sse",
                   act.fct = "logistic", linear.output = TRUE, exclude = NULL,
                   constant.weights = NULL, likelihood = FALSE)
    return(nn)
  }

  else if (MarketObject$runType == 10){
    #k nearest neighbors
    trainX = MarketObject$xx[(length(MarketObject$xx) - MarketObject$memory):(length(MarketObject$xx) - 1)]
    trainY = MarketObject$xx[(length(MarketObject$xx) - MarketObject$memory + 1):length(MarketObject$xx)]
    return(list(trainX, trainY))
  }
}

# update function name to predictPriceDividend 4/19/21
predictPriceDividend = function(model, df) {
  result = tryCatch({
    if (runType == 12){
      # convert inputs to matrix
      df <- as.matrix(df)
      return(model %>% predict(df))
    }
    else if (runType == 13){
      # convert inputs to matrix
      df <- as.matrix(df)
      return(predict(model, df))
    }
    else{
      return(predict(model, df)[[1]])
    }
  }, warning = function(war) {
    print("Warning")
  }, error = function(err) {
    print("Error")
    print(err)
  })
  return(result)
}


#Package/Dependency Checks and Installation
dependencyCheck = function(onHPC) {
  if (onHPC == TRUE) {
    suppressPackageStartupMessages(library(zoo))
    suppressPackageStartupMessages(library(xts))
    suppressPackageStartupMessages(library(glmnet))
    suppressPackageStartupMessages(library(rlist))
    suppressPackageStartupMessages(library(dplyr))
    suppressPackageStartupMessages(library(leaps))
    suppressPackageStartupMessages(library(ggplot2))
  } else {
    dependencies = c("zoo", "xts", "glmnet", "rlist", "dplyr", "leaps", "ggplot2")
    for (depen in dependencies) {
      if(depen %in% rownames(installed.packages()) == FALSE){
        install.packages(depen, dependencies = TRUE)
      }
      suppressPackageStartupMessages(library(depen, character.only = TRUE))
    }
  }
}

# Get Inputs
GetMacros = function(inputfile)
{
  # Get macro inputfile
  input = read.table(inputfile, header = FALSE, sep = " ")
  input = data.frame(input)

  # get rid of scientific notation
  options(scipen = 999)
  # names
  colnames(input) <- c("Varnames", "V2")

  # macro vars
  rounds <<- subset(input, Varnames == "rounds")[[2]]
  popsize <<- subset(input, Varnames == "popsize")[[2]]
  bubbleThresholdHigh <<-
    subset(input, Varnames == "bubbleThresholdHigh")[[2]]
  bubbleThresholdLow <<-
    subset(input, Varnames == "bubbleThresholdLow")[[2]]
  pupdate <<- subset(input, Varnames == "pupdate")[[2]]

  powers <<- subset(input, Varnames == "powers")[[2]]
  lags <<- subset(input, Varnames == "lags")[[2]]
  layers <<- subset(input, Varnames == "layers")[[2]]
  numBubbles <<- subset(input, Varnames == "numBubbles")[[2]]

  # initial conditions
  startPrice <<- subset(input, Varnames == "startPrice")[[2]]
  dividend <<- subset(input, Varnames == "dividend")[[2]]
  interest <<- subset(input, Varnames == "interest")[[2]]

  # note: rename leval=memory
  memory <<- subset(input, Varnames == "memory")[[2]]
  linit <<- subset(input, Varnames == "linit")[[2]]
  # maxiter???? <<- subset(input,Varnames == "linit")[[2]]
  shockRange_div <<- subset(input, Varnames == "shockRangeDiv")[[2]]
  runType <<- subset(input, Varnames == "runType")[[2]]
  selection_type <<- subset(input, Varnames == "selection_type")[[2]]

  pshock <<- subset(input, Varnames == "pshock")[[2]]
  randSeed <<- subset(input, Varnames == "randSeed")[[2]]
  risk_constant <<- subset(input, Varnames ==  "risk_constant")[[2]]
  risk_type <<- subset(input, Varnames == "risk_type")[[2]]

  priceDifThreshold <<- subset(input, Varnames == "priceDifThreshold")[[2]]
  saveData <<- subset(input, Varnames == "saveData")[[2]]

  # get number of predictors for runTypes > 7
  numberPredictors <<- subset(input, Varnames == "numberPredictors")[[2]]

  recordOA_start <<- subset(input, Varnames == "recordOA_start")[[2]]
  recordOA_end <<- subset(input, Varnames == "recordOA_end")[[2]]

  # define switches
  normalizeInputs <<- subset(input, Varnames == "normalizeInputs")[[2]]
  testing <<- subset(input, Varnames == "testing")[[2]]
  forecast2ahead <<- subset(input, Varnames == "forecast2ahead")[[2]]

}

printSomething = function(x) {
  print(x)
}

normalizeData = function(df) {
  if (normalizeInputs == 1) {
    meanx = dividend * (1+interest)/interest
    stdevx = shockRange_div / sqrt(3)
    df = (df - meanx) / stdevx
    return(df)
  }
  if (normalizeInputs == 0) {
    return(df)
  }
}

# x = list(1,2,3,4)
# for (y in x) {
#   if (y == 2) {
#     x = list.remove(x, 2)
#   } else {
#     print(y)
#   }
# }
# print(x)
