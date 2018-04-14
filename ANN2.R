# Artificial Nueral network using nnet library

library(nnet)

#Import dataset
library(xlsx)

#UsedCars.xlsx dataset
df5 = read.xlsx(file.choose(),1,header = T)
df5 = df5[, !apply(is.na(df5), 2, all)]
df5 = df5[!apply(is.na(df5),1,all), ]
head(df5)

Age = 2017-df5$Mfg_Year
df5 = cbind(df5, Age)

df5b = df5

df5 = df5[ -c(1,2,3,7)]
str(df5)

df5$Transmission = as.factor(df5$Transmission)
df5$C_Price = as.factor(df5$C_Price)

maxdf5 = unname(apply(df5[,-c(1,4,7)],2,max,na.rm = T))
mindf5 = unname(apply(df5[,-c(1,4,7)],2,min,na.rm = T))

df5[,-c(1,4,7)]=scale(df5[,-c(1,4,7)], center = mindf5, scale = maxdf5 - mindf5)

str(df5)

#Partitioning (90%:10%)

partidx = sample(1:nrow(df5), 0.9 * nrow(df5), replace = F)
df5train = df5[partidx,]
df5test = df5[-partidx, ]

epoch2 = nrow(df5train); epoch2

mod2 = nnet(C_Price ~ ., data=df5train, size = 9, range = 0.5, decay = 0,
            maxit = 30 * epoch2, abstol = 0.09)

mod2trainc = ifelse(mod2$fitted.values > 0.5, 1, 0)

table("Actual Class" = df5train$C_Price,
      "Predicted Class" = factor(mod2trainc, levels = c("0","1")))

#Classification Accuracy
mean(mod2trainc == df5train$C_Price)
#Misclassification error
mean(mod2trainc != df5train$C_Price)

mod2test = predict(mod2, df5test[,-c(7)])
mod2testc = ifelse(mod2test > 0.5, 1, 0)

table("Actual Class" = df5test$C_Price,
      "Predicted Class" = factor(mod2test, levels = c("0","1")))
#Classification Accuracy
mean(mod2testc == df5test$C_Price)
#Misclassification error
mean(mod2testc != df5test$C_Price)

library(devtools)
#source("nnet_plot_update.r")
plot.nnet(mod2)
