# Artificial Neural network

#Import dataset
library(xlsx)

#UsedCars.xlsx dataset
df1 = read.xlsx(file.choose(),1,header = T)
df1 = df1[, !apply(is.na(df1), 2, all)]
df1 = df1[!apply(is.na(df1),1,all), ]
head(df1)

Age = 2017-df1$Mfg_Year
df1 = cbind(df1, Age)

df1b = df1

df1 = df1[ -c(1,2,3,11)]
str(df1)

#Variable transformation and normalization
levels(df1$Fuel_type)
CNG = df1$Fuel_type == "CNG"
Diesel = df1$Fuel_type == "Diesel"
Petrol = df1$Fuel_type == "Petrol"

ManualT = df1$Transmission == 0
AutoT = df1$Transmission == 1

maxdf1 = unname(apply(df1[,-c(1,5)],2,max, na.rm = T))
mindf1 = unname(apply(df1[,-c(1,5)],2,min, na.rm = T))
df1[, -c(1,5)] = scale(df1[,-c(1,5)], center = mindf1,
                       scale = maxdf1 - mindf1)
df2 = cbind(df1[,-c(1,5)], Diesel, Petrol, AutoT)
str(df2)

Partitioning (90%:10%)
partidx = sample(1:nrow(df2), 0.9*nrow(df2), replace = F)
df2train = df2[partidx, ]
df2test = df2[-partidx, ]

library(neuralnet)
# Neural network structure
# Input layers : 8 nodes (for 8 predictors) - modes 1:8
# Hidden layers : one with 9 nodes - nodes 9:17
# Output layer : One node - node 18

mf = as.formula(paste("Price ~", paste(names(df2)[!names(df2) %in% "Price"],
                                       collapse = "+")))
epoch = nrow(df2train); epoch

mod1 = neuralnet(mf, algorithm = "rprop+", threshold = 0.0009, 
                 stepmax = 30 * epoch,
                 data = df2train, hidden = c(9), linear.output = T,
                 rep = 1)
mod2 = neuralnet(mf, algorithm = "rprop+", threshold = 0.04, 
                 stepmax = 1 * epoch,
                 data = df2train, hidden = c(9), linear.output = T,
                 rep = 1)
mod1$result.matrix[1:3,1]
mod2$result.matrix[1:3,1]

# Interlayer connection weights
# Input layer to hidden layer connections

dimnames(mod1$weights[[1]][[1]]) = list(c("bias", "nodel:SR_Price",
                                        "node2:KM", "node3:Owners",
                                        "node4:Airbag", "node5:Age",
                                        "node6:Diisel", "node7:Petrol",
                                        "node8:AutoT"),
                                        c("mode9","node10","node11",
                                          "node12","node13","node14",
                                          "node15","node16","node17"))

mod1$weights[[1]][[1]]

#Hidden layer to putput layer connection
dimnames(mod1$weights[[1]][[2]]) = list(c("bias","node9","node10","node11",
                                            "node12","node13","node14",
                                            "node15","node16","node17"),
                                          c("node18:Price")) 
mod1$weights[[1]][[2]]
head(data.frame("Predicted Value" = mod1$net.result[[1]][,1],
                "Acutal value" = df2train$Price, df2train[, -c(3)]))

#Performance
library(rminer)

#Training partition
M = mmetric(df2train$Price, mod1$net.result[[1]][,1], c("SSE","RMSE","ME"))
print(round(M, digits = 6), na.print = "")

M1 = mmetric(df2train$Price, mod2$net.result[[1]][,1], c("SSE","RMSE","ME"))
print(round(M1, digits = 6), na.print = "")

# Test partition
mod1test = compute(mod1, df2test[, -c(3)])
M2 = mmetric(df2test$Price, mod1test$net.result[,1], c("SSE","RMSE","ME"))
print(round(M2, digits = 6), na.print = "")

mod2test = compute(mod2, df2test[, -c(3)])
M3 = mmetric(df2test$Price, mod2test$net.result[,1], c("SSE","RMSE","ME"))
print(round(M3, digits = 6), na.print = "")

#Network diagram
plot(mod1)

#Neural Network with 18 hidden nodes
mod3 = neuralnet(mf, algorithm = "rprop+", threshold = 0.0007, 
                 stepmax = 30 * epoch,
                 data = df2train, hidden = c(18), linear.output = T,
                 rep = 1)

mod3$result.matrix[1:3,1]

# Performance
#Training Partition
M4 = mmetric(df2train$Price, mod3$net.result[[1]][,1], c("SSE","RMSE","ME"))
print(round(M3, digits = 6), na.print = "")

# Test partition
mod3test = compute(mod3, df2test[, -c(3)])
M5 = mmetric(df2test$Price, mod3test$net.result[,1], c("SSE","RMSE","ME"))
print(round(M5, digits = 6), na.print = "")

#Using rep = 20 and selecting the best model using validation partition
th = seq(0.01, 0.1, 0.005); th
Mtest = NULL
for(i in th){
  #Neural Network with 18 hidden nodes
  mod4 = neuralnet(mf, algorithm = "rprop+", threshold = i, 
                   stepmax = 30 * epoch,
                   data = df2train, hidden = c(9), linear.output = T,
                   rep = 20)
  best = as.integer(which.min(mod4$result.matrix[c("error"), ]))
  mod4test = compute(mod4, df2test[, -c(3)], rep = best)
  M6 = mmetric(df2test$Price, mod4test$net.result[,1],c("RMSE"))
  Mtest = c(Mtest, M6)
}

DF = data.frame("threshold" = th, "error" = Mtest); DF
DF[which.min(DF$error), ]
plot(th, Mtest, type = "b",
     xlab = "Threshold", ylab = "Validation Error")

#Best Model
mod5 = neuralnet(mf, algorithm = "rprop+", threshold = 0.08, 
                 stepmax = 30 * epoch,
                 data = df2train, hidden = c(9), linear.output = T,
                 rep = 20)
best = as.integer(which.min(mod5$result.matrix[c("error"), ]))
mod5$result.matrix[1:3, best]

M7 = mmetric(df2train$Price, mod5$net.result[[best]]['1'],c("RSME"))
M7

mod5test = compute(mod5, df2test[ ,-c(3)], rep = best)
M8 = mmetric(df2test$Price, mod5test$net.result[,1], c("RMSE"))
M8

# Scaling back(numeric) outcome variable to original units
b = maxdf1[3]; b
a = mindf1[3]; a

df2test.pred.org = a + (b-1) * mod5test$net.result[,1]
data.frame("Actual value" = df1b[-partidx,7],
           "Predicted value" = df2test.pred.org)






