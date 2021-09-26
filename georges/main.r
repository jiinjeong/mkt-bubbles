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
    if(round %% 100 == 0) {   3/7/21
      printSomething("round:")
      printSomething(round)
    }
    if (round == (MarketObject$lags + 1)) {
      MarketObject$init()
    } else if ( (round > (MarketObject$lags + 1))&(round <= MarketObject$lInit ) ) {
      # Not first round and before linit round
      # changed memory + 3 to linit 7/17/17
      MarketObject$createInitialData(round)
    } else if (MarketObject$prices[round - 1] > MarketObject$bubbleThresholdHigh) { 
      # check if price has blown up
      # changed ex to prices 7/17/17
      return (c(1, round, MarketObject$memory, MarketObject$pUpDate, MarketObject$thresholdTally, MarketObject$prices[round-1], 1, MarketObject$randSeed))
    } else if (MarketObject$prices[round - 1] < MarketObject$bubbleThresholdLow) { 
      # check if price has blown up
      # changed ex to prices 7/17/17
      return (c(1, round, MarketObject$memory, MarketObject$pUpDate, MarketObject$thresholdTally, MarketObject$prices[round-1], 0, MarketObject$randSeed))
    } else {
      MarketObject$simulation(round)
      
      #Check if the price difference exceeds provided threshold and adds to tally if so
      if (abs(MarketObject$prices[round] - MarketObject$prices[round-1]) > MarketObject$priceDifThreshold){
        MarketObject$thresholdTally = MarketObject$thresholdTally + 1 
      }
    }
    #MarketObject$print(verbose=TRUE, round)
  }
  
  #print("End of Sim")
  return(MarketObject$prices)
  # return (c(0, 0, MarketObject$memory, MarketObject$pUpDate, MarketObject$thresholdTally, MarketObject$randSeed))
}
# save objects then average predictions

