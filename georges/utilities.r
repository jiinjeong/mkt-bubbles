#A. Time series plots for prices, returns, network measures, etc.

library(patchwork)
library(latex2exp)

#1. plot simulation prices and returns - from sim 
#should work with any routine

L1param <- 0.001
start_plot <- linit
#start_plot <- 1800
end_plot <- rounds
#end_plot <- 1880
#sim_round <- c(1:rounds)
sim_round <- c(1:end_plot)
sim_price <- MO$prices[1:end_plot]
sim_ts_data = data.frame(sim_round, sim_price)
sim_return <- log(MO$prices[1:end_plot]) - log(lag(MO$prices[1:end_plot]))
sim_ts_data$sim_return <- sim_return
plot1 <- ggplot(sim_ts_data[start_plot:end_plot,], aes(x = sim_round)) +
  geom_line(aes(y=sim_price, color="price")) +
  labs(title = TeX("Simulation Price History"),
       subtitle = paste("Full Model, NN: 10 predictors, 1 HL with 100 nodes, TF, L1 penalty ", L1param),
       caption = TeX("Bubble forming approx 1860, here plotted to 1880"))
plot2 <- ggplot(sim_ts_data[start_plot:end_plot,], aes(x = sim_round)) +
  geom_line(aes(y=sim_return, color = "return" )) +
  labs(title = TeX("Simulation Returns History"), 
       subtitle = paste("Full Model, NN: 10 predictors, 1 HL with 100 nodes, TF, L1 penalty ", L1param),
       caption = TeX("Bubble forming approx 1860, here plotted to 1880"))
plot1
plot2
ggsave("price-10preds-100nodes-tf-L1reg0pt001-bubble.pdf", plot=plot1, width=6, height=4)
ggsave("returns-10preds-100nodes-tf-L1reg0pt001-bubble.pdf", plot=plot2, width=6, height=4)
plot1 + plot2

MO$histPenalty[linit:rounds]

#2. plot targets and fitted values from training period 

#a. for neuralnet output from test_nn_interp_test.r 
#which analyses nn for purely artificial x series: x_t = x^* + epsilon_t
#to illustrate overfitting

training_round <- 1:memory
xx <- nn$data$label
xx_fit <- nn$net.result[[1]]
df3 <- data.frame(training_round, xx, xx_fit)
head(df3)
plot3 <- ggplot(df3, aes(x = training_round)) +
  geom_line(aes(y=xx, color="xx")) + 
  geom_line(aes(y=xx_fit, color="xx_fit")) +
  labs(title = TeX("x and fitted x for aritificial data $x_t = x^* + \\epsilon_t$"), 
       subtitle = "Neural Network: 10 predictors, 1 hidden layer with 40 nodes")
#, caption = TeX('$x = x^* + \\epsilon_t$')
plot3

#note that here the fitted neural network is nn
#whereas in the full program it's the predictors field of any optimal agent
#    so e.g., MO$histOA[[2]]$predictors
#    which is the nn associated with the 2nd optimal agent stored in histOA

#b. for tensorflow/keras output -- 
# function to generate training data used by OA
genTrainingDf <- function(optimalAgent) {
  # determine when the agent's model was trained
  trainPeriod <- optimalAgent$startUse
  # determine forecasting method (iterative or two ahead)
  shift <- 0
  if (forecast2ahead == 1) {
    shift <- 1
  }
  # define training inputs
  temp = seq(1, memory, 1)
  train_data = data.frame(temp)
  for (predictor in 1:numberPredictors) {
    train_data[paste("x", toString(predictor), sep="")] = MO$xx[((trainPeriod - 2) - (memory - 1) + (1 - predictor) - shift) : ((trainPeriod - 2) - (predictor - 1) - shift)]
  }
  train_data = subset(train_data, select = -c(temp))
  train_data = normalizeData(train_data) # normalize inputs
  train_data <- as.matrix(train_data) # convert to matrix
  # define training targets
  train_targets = MO$xx[((trainPeriod - 1) - (memory - 1)) : (trainPeriod - 1)]
  train_targets <- as.matrix(train_targets) # convert to matrix
  # return training data
  return(list("train_data" = train_data, "train_targets" = train_targets))
}

#from last round x data and for most recent optimal agent
lOA <-length(MO$optimalAgents)
lOA
source("core/Functions.r")

# reconstruct last training data
data <- genTrainingDf(MO$optimalAgents[[1]])
#data <- genTrainingDf(MO$optimalAgents[[lOA]])

train_data <- data$train_data
train_targets <- data$train_targets

### old code to reconstruct last training data - commented out 6/14/21
# shift <- 0
# if (forecast2ahead == 1) {
#   shift <- 1
# }
# temp = seq(1, memory, 1)
# train_data = data.frame(temp)
# for (predictor in 1:numberPredictors) {
#   train_data[paste("x", toString(predictor), sep="")] = MO$xx[((length(MO$xx) - 1) - (memory - 1) + (1 - predictor) - shift) : ((length(MO$xx) - 1) - (predictor - 1) - shift)]
# }
# train_data = subset(train_data, select = -c(temp))
# 
# train_data = normalizeData(train_data) # normalize inputs
# train_data <- as.matrix(train_data) # convert to matrix
# #cat('\n','dimensions of training data:',dim(train_data)) # check dimensions
# 
# # define training labels
# train_targets = MO$xx[(length(MO$xx) - (memory - 1)) : length(MO$xx)]
# train_targets <- as.matrix(train_targets) # convert to matrix

plot3 <- function() {
  training_round <- 1:memory
  xxx <- train_targets
  xxx_fit <- MO$optimalAgents[[1]]$predictors %>% predict(train_data)
  #xxx_fit <- MO$optimalAgents[[length(MO$optimalAgents)]]$predictors %>% predict(train_data)
  df3 <- data.frame(training_round, xxx, xxx_fit)
  head(df3)
  the_plot <- ggplot(df3, aes(x = training_round)) +
    geom_line(aes(y=xxx, color="xxx")) + 
    geom_line(aes(y=xxx_fit, color="xxx_fit")) +
    #labs(title = TeX("x and fitted x for aritificial data $x_t = x^* + \\epsilon_t$"), 
    #labs(title = TeX("x and fitted x for last round and last optimal agent"),
    labs(title = TeX("x and fitted x for last round and earliest active optimal agent"),
         subtitle = "NN: 10 predictors, 1 hl with 100 nodes, tf, L1 0.001",
         caption = "Bubble emerges from 1860 to 1933, OA fitted in 1925")
         #caption = TeX('$x = x^* + \\epsilon_t$'))
  
  return(the_plot)
}
plot3()

ggsave("OA1925-10preds-100nodes-tf-L1reg0pt001-bubble.pdf", plot=plot1, width=6, height=4)



# calc errors -- for #2 above
#a. neuralnet
nn$result.matrix[1,] # for error (which neuralnet records as sse/2)
nn$net.result #(for fited y values)
nn$data$label #for true y values
errors <- nn$net.result - as.data.frame(nn$data$label) # actual sample errors
sum(errors^2) #sample sse (exactly twice neuralnet's reported error)
reerrors <- as.data.frame(nn$data$label) - 10.5 # errors under RE forecast of 10.5
sum(reerrors^2) #sse under RE
sum(reerrors^2)/2 #corresponding RE error for comparison to neuralnet value

#b. keras/tensorflow
#print training loss (with penalty if any)
MO$optimalAgents[[length(MO$optimalAgents)]]$predictors %>% evaluate(train_data,train_targets)

#calc training loss manually (sse/n) (without penalty)
errors <- train_targets - MO$optimalAgents[[length(MO$optimalAgents)]]$predictors %>% predict(train_data)  # actual sample errors
sum(errors^2)/200
sum(errors^2)/2

re_errors <- train_targets - 10.5
sum(re_errors^2)/200
sum(abs(re_errors))/200


# predict out of sample  -- for #2 above -- need to have functions.r loaded

df2 = data.frame(1)
for (i in (numberPredictors - 1):0) {
  df2[paste("p", toString(i), sep = "")] = xx[length(xx) - i]
}
df2 = subset(df2, select = -c(df2$X1))
# normalize inputs: subtract mean and divide by std dev (theoretical)
meanx = dividend * (1+interest)/interest
stdevx = shockRange_div / sqrt(3)
df2 = (df2 - meanx) / stdevx
predictPriceDividend(nn, df2)





#B. ANALYSIS of neural network WEIGHTS from Keras/Tensorflow runs (runType 12)

#1. Weight matrix size measures

#Weights matrices of neural net for optimal agent #3 from last round of sim
#  note that weights[[1]] is layer 1 weights, weights[[2]] is layer 1 bias ...
#  note that we can use histOA here rather than optimalAgents if it is populated
MO$optimalAgents[[3]]$predictors$weights

#save layer 1 weights matrix for optimal agent #3
layer_1_OA3 <- as.matrix(MO$optimalAgents[[3]]$predictors$weights[[1]])

#Frobinius norm (akin to L2 or Euclidean norm of a vector) of OA3 layer 1 weights matrix
#   gives a measure of overall magnitude of the matrix 
norm(layer_1_OA3, type="F")

#Frobinius norm can also be spelled out - sum of squared elements of the matrix
sqrt(sum(layer_1_OA3^2))

#L1 norm of OA3 layer 1 weights - sum of absolute values of elements of the matrix
sum(abs(layer_1_OA3))

#there are various other norms and measures of weight matrix size that 
#   can be used.

#Can sum size measures over the various layers, leaving out biases
#So for two hidden layers plus output layer, sum sizes of [[1]], [[3]], [[5]]
#   using L2 or L1 norm equivalent
#For L2 equivalent
size_weights_OA3_L2 <- 
  norm(as.matrix(MO$optimalAgents[[3]]$predictors$weights[[1]]), type="F") +
  norm(as.matrix(MO$optimalAgents[[3]]$predictors$weights[[3]]), type="F") +
  norm(as.matrix(MO$optimalAgents[[3]]$predictors$weights[[5]]), type="F")
size_weights_OA3_L2 

#For L1 equivalent
size_weights_OA3_L1 <- 
  sum(abs(as.matrix(MO$optimalAgents[[3]]$predictors$weights[[1]]))) +
  sum(abs(as.matrix(MO$optimalAgents[[3]]$predictors$weights[[3]]))) +
  sum(abs(as.matrix(MO$optimalAgents[[3]]$predictors$weights[[5]])))
size_weights_OA3_L1

#and here are more of the individual layer kernal weights and biases for last OA
lOA <- length(MO$optimalAgents) #last OA index value
lOA <- 1
#save layer 1 weights and bias matrices
layer_1_kernel_weights <- as.matrix(MO$optimalAgents[[lOA]]$predictors$weights[[1]])
layer_1_kernel_weights
layer_1_biases <- as.matrix(MO$optimalAgents[[lOA]]$predictors$weights[[2]])
layer_1_biases
#save output layer and bias
output_layer_kernel_weights <- as.matrix(MO$optimalAgents[[lOA]]$predictors$weights[[3]])
output_layer_kernel_weights
output_layer_bias <- as.matrix(MO$optimalAgents[[lOA]]$predictors$weights[[4]])
output_layer_bias

norm(layer_1_kernel_weights, type="F")
norm(output_layer_kernel_weights, type="F")

#2. Predictor (feature) Importance

# here there are many measures available from partial derivatives to Shapley values
# some packages available for output of nuralnet package, need to find for keras output

#we have some that work with neuralnet package
#feature importance
library(NeuralNetTools)
garson(nn) #feature importance
olden(nn) #feature importance
#lekprofile(nn) ?? #haven't gotten this to work

#library(NeuralSens) #haven't gotten this to work
#SensAnalysisMLP(nn) ??
#nothing so far that does Shapley values ??

#3. Visualizing neural network

#presumably TensorBoard?

#for neuralnet package we have 
plot(nn, rep="best") #built into neuralnet
#and
library(NeuralNetTools)
plotnet(nn)









