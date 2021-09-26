##############################################
# Document: singleRun.r
# Purpose: Used for running a single simulation
# Functions:
#   1. checkPath
# ToDo:
##############################################

#clear environment
# Import functions library
setwd("/Users/Jiin/Desktop/jiin-justin/hpc-mkt-bubbles/georges")  # This Code Directory
source("Functions.r")
source("Agents.r")
source("Market.r")
source("main.r")

dependencyCheck(onHPC = FALSE)

GetMacros(inputfile = "input0001.txt")

s <- (3 + (powers * lags) + (((lags-1) * (lags)) / 2))

MO <<- new("Market",
           optimalAgents = list(),
           agents = list(),
           repAgent = new("RepresentativeAgent"),
           prices = c(1),
           dividends = c(1),
           interestRates = c(1),
           xx = c(1),
           thresholdTally = 0,
           priceDifThreshold = priceDifThreshold,
           saveData = saveData,
           memory = memory,
           pUpDate = pupdate,
           bubbles = 0,
           bubbleRound = 0,
           size = s,
           runType = runType,
           numAgents = popsize,
           numRounds = rounds,
           lInit = linit,
           #randSeed = sample(1:2000, 1),
           randSeed = randSeed,
           lags = lags,
           powers = powers,
           startPrice = startPrice,
           bubbleThresholdHigh = bubbleThresholdHigh,
           bubbleThresholdLow = bubbleThresholdLow,
           interest = interest,
           dividend = dividend,
           shockRangeDiv = shockRange_div,
           riskConstant = risk_constant,
           riskType = risk_type,
           pShock = pshock,
           selectionType = selection_type,
           oldRep = vector(),
           oldOA = vector(),
           marketMatrix = matrix(),
           alphaMatrix = matrix(),
           updateParams = matrix(),
           histOA = list(),
           histOAconnections = matrix(),
           histPenalty = vector())