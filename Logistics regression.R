# Linear Regression

library(xlsx)
#Import Cars dataset
df = read.xlsx(file.choose(),1,header = T)
df = df[ , !apply(is.na(df), 2, all)]

# Variable names
names(df)

#first 9 records
df[1:9, ]

# Sorting for outlier detection
head(data.frame("KM" = df$KM, "SR_Price" = df$SR_Price,
                "Mfg_Year" = df$Mfg_Year)[order(-df$KM), ])

# Variable transformation
Age = 2017 - df$Mfg_Year
df= cbind(df, Age)
df1 = df[, -c(1,2,3)]
df1 = df1[, -c(2)]
head(df1)
str(df1)

#Seed for randomization
set.seed(12345)

#Partitioning of dataset
partidx = sample(1:nrow(df1), 0.5*nrow(df1), replace = F)

df1train = df1[partidx,]
df1test = df1[-partidx,]

mod = lm(Price ~., df1train)
summary((mod))

Modtrain = predict(mod, df1train[, -c(4)])
Residualtrain = df1train$Price - mod$fitted.values
head(data.frame("Actual Value"=df1train$Price,
                "Predicted Values" = mod$fitted.values,
                Residualtrain))

modtest = predict(mod, df1test)
Residualtest = df1test$Price - modtest
head(data.frame("Actual Value"=df1test$Price,
                "Predicted Values" = modtest.values,
                Residualtest))

library(rminer)
mmetric(df1train$Price, mod$fitted.values, c("SSE", "RSME", "ME"))
mmetric(df1test$Price, modtest,c("SSE", "RSME", "ME"))

####################

