# Hypothetical dataset
#no.of observation = 1000

n = 1000

#Create predictors : x1 and x2
df = data.frame(x1 = runif(n,0,100), x2=runif(n,0,100))
head(df)

#create categorical outcome variable with three classes
df = transform(df, y=1+ifelse(100 - x1 - x2 + rnorm(n,sd = 10) < 0, 0,
                              ifelse(100 - 2 * x2 + rnorm(n,sd = 10) < 0 , 1, 2)))

head(df)
str(df)

palette()
palette(gray(0:3/3))

range(df$x1)
range(df$y)
range(df$x2)

plot(df$x1, df$x2, xlim = c(0,100), ylim = c(0,100),
     xlab = "x1", ylab = "x2", col=as.factor(df$y),
     pch=19, cex=0.8, panel.first=grid())

# Multinomial Logistics regression
library(nnet)
mod=multinom(y~., data=df)

summary(mod)

head(mod$fitted.values)

modtrain = predict(mod, df[ , -3], type = "probs")
head(modtrain)

#Classify observations
mod3trainc = ifelse(modtrain[,1] > modtrain[ ,2] & 
                      modtrain[ , 1] > modtrain[,3],1,
                    ifelse(modtrain[,2] > modtrain[ ,3], 2, 3))
table("Actual value" = df$y, "Predicted value" = mod3trainc)

#Classification accuracy
mean(mod3trainc == df$y)
# misclassification error
mean(mod3trainc != df$y)

#Ordinal Logistic regression
library(MASS)
mod1 = polr(as.factor(y) ~ ., data = df, Hess = T)

summary(mod1)

head(mod1$fitted.values)

mod1train = predict(mod1, df[,-3], type = "probs")
head(mod1train)

#Classify observations
mod1trainc = ifelse(mod1train[,1] > mod1train[ ,2] & 
                      mod1train[ , 1] > mod1train[,3],1,
                    ifelse(mod1train[,2] > mod1train[ ,3], 2, 3))

table("Actual value" = df$y, "Predicted value" = mod1trainc)

#Classification accuracy
mean(mod1trainc == df$y)
# misclassification error
mean(mod1trainc != df$y)

