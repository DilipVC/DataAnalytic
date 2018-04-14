# Artificial Neural Network

# Hypothetical example
FatScore = c(0.2, 0.1, 0.2, 0.2, 0.4, 0.3) ; FatScore
SaltScore = c(0.9, 0.1, 0.4, 0.5, 0.5, 0.8) ; SaltScore
Acceptance = c(1,0,0,0,1,1) ; Acceptance

# Neural Network Structure
# Input layer : Two nodes (for two predictors) - Nodes 1 & 2
# Hidden layer: One with three nodes - nodes 3, 4 & 5
# Output layer: one node - node 6

# I, j and wij initialization(random numbers in the range : -0.05 < A < 0.05)
bias = matrix(runif(4, -0.05, 0.05), ncol = 1,
              dimnames = list(c(3,4,5,6), c("I,"))); bias
weightsIH = matrix(runif(6, -0.05, 0.05), ncol = 3,
                   dimnames = list(c(1,2), c(3,4,5))); weightsIH
weightsHO = matrix(runif(3, -0.05, 0.05), ncol = 1,
                   dimnames = list(c(3,4,5),c(6))); weightsHO

# Computing output value for first record
# (FatScore = 0.2, SaltScore = 0.9)

output = NULL
k = 1 # first obsn
for(i in 1:length(bias) - 1){
  x = bias[i,1] + weightsIH[1,i]*FatScore[k] + weightsIH[2,i] * SaltScore[k]
  output[i] = 1/(1 + exp(-x))
}

i = i + 1
j = 1
x = bias[i,1] + weightsHO[1,j] * output[1] + weightsHO[2,j] * output[2] +
  weightsHO[3,j] * output[3]
output[4] = 1/(1 + exp(-x))

# Classify first record using cutoff value = 0.5
ifelse(output[4] > 0.5, 1, 0) # Predicted class
Acceptance[1] # Actual value

# Model for hypothetical data
library(neuralnet)

df = data.frame(FatScore, SaltScore, Acceptance)
df

# Neural Network model
# Strart weights vector: no. of all bias (4) and connection weight values (9)
# linear.output = T For prediction
# linear.output = F for calssification

mod = neuralnet(Acceptance ~ FatScore + SaltScore, df, hidden = c(3),
                startweights = runif(13, -0.05, 0.05), rep = 1,
                algorithm = "backprop", learningrate = 0.1, err.fct = "sse",
                act.fct = "logistic",
                linear.output = F)
mod$result.matrix[1:3,1]

# Inner layer Connection weights
# Input layer to Hidden layer connections
dimnames(mod$weights[[1]][[1]]) = list(c("bias","node1:fat", "node2:salt"),
                                       c("mode3", "node4", "node5")); mod$weights[[1]][[1]]

dimnames(mod$weights[[1]][[2]]) = list(c("bias","node3", "node4", "node5"),
                                       c("mode6:accept")); mod$weights[[1]][[2]]

#Classify training records
modtrainc = ifelse(mod$net.result[[1]][,1] > 0.5,1,0); modtrainc
modtrainc=unname(modtrainc)

data.frame("Predicted Class" = modtrainc, "Actual Class" = df$Acceptance,
           "Predicted value" = mod$net.result[[1]][,1], "fat" = df$FatScore,
           "salt" = df$SaltScore)

#Classification matrix
table("Actual Class"=df$Acceptance,
      "Predicter Class" = factor(modtrainc, levels = c("0","1")))

#Classification accuracy
mean(modtrainc == df$Acceptance)
#Misclassification error
mean(modtrainc != df$Acceptance)

#Network diagram
plot(mod)




