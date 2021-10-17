##############################################
# Document: main.r
# Purpose: Contains the main function for running
# a simulation 
# Functions:
#   1. main
# ToDo:
##############################################

################################################################################
################################################################################
# OVERVIEW:
#
# The whole simulation happens in 3 stages:
#  	(1) Initialization of:
#	         (a) Prices of Stock and Bond
#		 (b) Market
#		 (c) Storage
#	(2) Achieve Normal Market Conditions
#	         (a) Go for linit rounds until some market history has been
#                    created
#	(3) Add Learning Dynamics
#	    	 (a) Let the traders trade
#
################################################################################
################################################################################




#################################
### Main function ###
#################################

# 0 no bubble
# 1 bubble
# -1 fail

main = function(MarketObject) {
  set.seed(MarketObject$randSeed)

  for (round in (MarketObject$lags + 1):MarketObject$numRounds) {
    if(round %% 100 == 0) {
      printSomething("round:")
      printSomething(round)
    }
    if (round == (MarketObject$lags + 1)) {
      MarketObject$init()
    } else if ( (round > (MarketObject$lags + 1))&(round <= MarketObject$lInit ) ) {
      MarketObject$createInitialData(round)
    } else if (MarketObject$prices[round - 1] > MarketObject$bubbleThresholdHigh) {
      return (c(1, round, MarketObject$memory, MarketObject$pUpDate, MarketObject$thresholdTally, MarketObject$prices[round-1], 1, MarketObject$randSeed))
    } else if (MarketObject$prices[round - 1] < MarketObject$bubbleThresholdLow) {
      return (c(1, round, MarketObject$memory, MarketObject$pUpDate, MarketObject$thresholdTally, MarketObject$prices[round-1], 0, MarketObject$randSeed))
    } else {
      MarketObject$simulation(round)
      
      if (abs(MarketObject$prices[round] - MarketObject$prices[round-1]) > MarketObject$priceDifThreshold){
        MarketObject$thresholdTally = MarketObject$thresholdTally + 1 
      }
    }
  }
  
  return(MarketObject$prices)
  # return (c(0, 0, MarketObject$memory, MarketObject$pUpDate, MarketObject$thresholdTally, MarketObject$randSeed))
}
