## Probability, odds and logit
# odds = p/(1-p)

curve(p/(1-p), from = 0, to = 1, type = "l", xname = "p",
      xlab = "Probability of success", ylab = "Odds")
# logit = log(odds) = log(p/(1-p))

curve(log(p/(1-p)), from = 0, to = 1, type = "l", xname = "p", las = 1,
      xlab = "Porbability of success", ylab = "logit", xaxt = "n")
axis(1, pos = 0)

library(xlsx)

# Promoffers.xlsx
df = read.xlsx(file.choose(),1,header = T)
df = df[,!apply(is.na(df),2,all)]

str(df)
dfb = df
df = df[,-5]

df$Promoffer = as.factor(df$Promoffer)
df$Online = as.factor(df$Online)
str(df)

# Partitioning: Tr: Tr = 60%:40%
partidx = sample(1:nrow(df), 0.6*nrow(df), replace = F)
dftrain = df[partidx,]
dftest = df[-partidx,]

#Model with a single predictor: Income
mod = glm(Promoffer ~ Income, family = binomial(link = "logit"),
          data = dftrain)
summary(mod)

b0 = unname(mod$coefficients[1]); b0
b1 = unname(mod$coefficients[2]); b1

#Fitted model
# "p(Promoffer = Yes|Income = x)" = 1/(1+exp(-(b0 + b1*x)))

range(dftrain$Income)

plot(dftrain$Income, as.numeric(as.character(dftrain$Promoffer)),
      type = "p", xlab = "Income", ylab = "Promoffer",
     pch = "p", xlim = c(0, 250))

curve(1/(1+exp(-(mod$coefficients[[1]] + mod$coefficients[[2]]*x))),
      xlim = c(0,250), type = "l", xname = "x", add = T)

# Model with all predictors
mod1 = glm(Promoffer ~ ., family = binomial(link = "logit"),
           data = dftrain)

#Options(scipen = 999)
summary(mod1)

# p = odds/(1+odds)
curve(odds/(1+odds), from = 0, to = 100, type = "l", xname = "odds",
      xlab = "Odds", ylab = "Probability of success")

# p = exp(logit)/(1+exp(logit))
curve(exp(logit)/(1+exp(logit)), from = -100, to = 100, type = "l",
      xname = "logit", las = 1,
      xlab = "logit", ylab = "Probability of success")

# Score test partition for porbability values
modtest =  predict(mod1, dftest[, -c(3)], type = "response")
# Score test partition for logit values
modtest1 = predict(mod1, dftest[, -c(3)], type="link")
# Classify observations using a cutoff value of 0.5
modtestc = ifelse(modtest > 0.5, 1,0)

table("Actual value" = dftest$Promoffer, "Predicted value" = modtestc)

#Classification accuracy
mean(modtestc == dftest$Promoffer)

#Misclassification error
mean(modtestc != dftest$Promoffer)

head(data.frame("Predicted class" = modtestc,
                "Actual class" = dftest$Promoffer,
                "Prob for 1(success)" = modtest,
                "log odds" = modtest1,
                dftest[, -3], check.names = F))

# Cumulative Life curve
dflift = data.frame("Probability of class 1" = modtest,
                    "Actual Class" = 
                      as.numeric(as.character(dftest$Promoffer)),
                    check.names = F)
head(dflift)
dflift = dflift[order(dflift[,1], decreasing = T), ]
head(dflift)
CumActualClass = cumsum(dflift[,2])
dflift=cbind(dflift, CumActualClass)
head(dflift)

range(1:nrow(dflift))
range(dflift$CumActualClass)
plot(1:nrow(dflift), dflift$CumActualClass, type = "l",
     xlab = "# cases", ylab = "Cumulative", xlim = c(0,2100), ylim=c(0, 210))
segments(0,0,nrow(dflift), dflift$CumActualClass[nrow(dflift)], lty = 3)

legend(800, 70, inset = 0.005,
       c("Cumulative personal loan when sorted using predicted values",
         "Cumulative personal loan using average"),
       lty=c(1,2), bty = "n", cex = 0.7, x.intersp=0.3, y.intersp = 0.5)

# Decile chart
globalmean = dflift$CumActualClass[nrow(dflift)]/nrow(dflift); globalmean
decilecases = round(seq(0.1,1,0.1)*nrow(dflift)); decilecases
j = 0
decile=NULL; decilemean = NULL

for(i in decilecases) {
  j = j+1
  decilemean[j] = dflift$CumActualClass[i]/i
  decile[j] = decilemean[j]/globalmean
}

range(decile)
barplot(decile, names.arg = as.factor(seq(1,10,1)), xlab = "Deciles",
        ylab = "Decile mean/Global mean", ylim=c(0,10))

#Measures of goodness of fit
gf = c(mod1$df.residual, mod1$deviance,
       100*table(dftrain$Promoffer)[["1"]]/
         length(dftrain$Promoffer), mod$iter,
       1 - (mod$deviance/mod1$null.deviance))
gf=as.data.frame(gf, optional = T)
rownames(gf) = c("Residual df", "Std.Dev.Estimate",
                 "%Success in training data",
                 "#Iterations used",
                 "Multiple R-Squared")
gf

#Score training partition for probability values
modtrain = predict(mod1, dftrain[ , -c(3)], type = "response")
# Score training partition for logit values
modtrain1 = predict(mod1, dftrain[, -c(3)], type="link")
# Classify observations using a cutoff value of 0.5
modtrainc = ifelse(modtrain > 0.5, 1,0)

table("Actual value" = dftrain$Promoffer, "Predicted value" = modtrainc)

#Classification accuracy
mean(modtrainc == dftrain$Promoffer)

#Misclassification error
mean(modtrainc != dftrain$Promoffer)

# Cumulative Life curve
dflift2 = data.frame("Probability of class 1" = modtrain,
                    "Actual Class" = 
                      as.numeric(as.character(dftrain$Promoffer)),
                    check.names = F)
head(dflift2)
dflift2 = dflift2[order(dflift2[,1], decreasing = T), ]
head(dflift2)
CumActualClass = cumsum(dflift2[,2])
dflift2=cbind(dflift2, CumActualClass)
head(dflift2)

range(1:nrow(dflift2))
range(dflift2$CumActualClass)
plot(1:nrow(dflift2), dflift2$CumActualClass, type = "l",
     xlab = "# cases", ylab = "Cumulative", xlim = c(0,3100), ylim=c(0, 310))
segments(0,0,nrow(dflift2), dflift2$CumActualClass[nrow(dflift2)], lty = 3)

legend(800, 70, inset = 0.005,
       c("Cumulative personal loan when sorted using predicted values",
         "Cumulative personal loan using average"),
       lty=c(1,2), bty = "n", cex = 0.7, x.intersp=0.3, y.intersp = 0.5)






















