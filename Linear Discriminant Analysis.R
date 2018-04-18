# Linear Discriminant Analysis

library(xlsx)

#SedanCar.xlsx

df=read.xlsx(file.choose(),1,header = T)
df = df[, !apply(is.na(df),2,all)]
df = df[!apply(is.na(df),1,all), ]
head(df)
str(df)

#Clearn Seperation into classes
plot(df$Annual_Income, df$Household_Area, las = 1,
     xlab = "Annual Income (Lakhs)",
     ylab = "Household Area (Ft2)",
     xlim = c(2,12), ylim=c(13,25), pch = c(21,19)[as.numeric(df$ownership)])

legend("bottomright", inset = 0.01, c("Owner", "Nonowner"),
       pch = c(19,21), cex=0.7, x.intersp = 0.5, y.intersp = 0.5)

df[df$Annual_Income > 5 & df$Annual_Income < 6 & 
     df$Household_Area > 20 & df$Household_Area < 22, c(1,2)]

df[df$Annual_Income > 8 & df$Annual_Income < 9 & 
     df$Household_Area > 16 & df$Household_Area < 18, c(1,2)]
segments(5.3,22,8.5,15)
slope = atan2(22-15,5.3-8.5)
segments(5.3 + 2*cos(slope), 22 + 2*sin(slope), 5.3,22)
segments(8.5, 15, 8.5 - 2*cos(slope), 15 - 2*sin(slope))

# No clear seperation into classes
# Promoffers.xlsx

df1 = read.xlsx(file.choose(),1,header = T)
df1 = df1[, !apply(is.na(df1),2,1ll)]
df1 = df1[!apply(is.na(df1),1,all),]

palette(c("gray", "black"))
samp = sample(1:nrow(df1),200, replace = F)

# Plot with log scale
range(df1[samp,]$Income)
range(df1[samp,]$Spending)
plot(df1[samp, ]$Income, df1[samp, ]$Spending, log="xy",
     main = "Sample of 200 customers", 
     xlab = "Income",
     ylab = "Spending",
     xlim = c(1,1000), ylim=c(0.001,10),
     col = as.factor(df1$Promoffer),
     pch = 20, cex = 0.8)
legend("topleft", inset = 0.005, c("Acceptor","NonAcceptor"),
       pch=20, col=c("gray", "black"), bty = "n",
       cex = 0.8, x.intersp = 0.5, y.intersp = 0.5)

range(df1$Income)
range(df1$Spending)
plot(df1$Income, df1$Spending, log = "xy",
     main = "Sample of 5000 customers", 
     xlab = "Income",
     ylab = "Spending",
     xlim = c(1,1000), ylim=c(0.001,10),
     col = as.factor(df1$Promoffer),
     pch = 20, cex = 0.8)

palette("default")

#Modelling for SedanCar dataset for Discriminant Analysis
str(df)
library(MASS)

mod = lda(Ownership ~ Annual_Income + Household_Area, df); mod

#Stacked histogram of LDA values
par(mar = c(5,5,1,1), cex = 0.1)
plot(mod)

modscorec = predict(mod)$class ; modscorec
modscore = predict(mod)$x; modscore

# Open a new graphics device
dev.new()

#Scatter plot of Discriminant functions
plot(modscore[,1])
text(modscore[,1], labels = modscorec, cex = 0.7, pos= 4, col = "gray")
text(modscore[,1], labels = df$Ownership, cex = 0.7, pos = 1, col = "black")
legend("bottomright", inset = 0.01, c("Predicted", "Actual"),
       text.col = c("gray", "black"), cex = 0.7, 
       x.intersp = 0.5, y.intersp = 0.5)
data.frame("Predicted Class" = modscorec, "Actual Class" = df$Ownership,
           "Classification scores" = unname(modscore))

conf.matrix = table("Actual class" = df$Ownership,
                    "Predicted class" = modscorec); conf.matrix
mosaicplot(conf.matrix, color = T, main = "Classification Matrix")

# Classification accurary
mean(modscorec == df$Ownership)
#Misclassification error
mean(modscorec != df$Ownership)

data.frame("Predicted Class" = modscorec, "Actual Class" = df$Ownership,
          "Prob. for owner" = predict(mod)$posterior[,2], df[,c(1,2)], check.names = F)
par(mar=c(5.1,5.1,5.1,5.1))

range(df$Annual_Income)
range(df$Household_Area)
plot(df$Annual_Income, df$Household_Area, las = 1, xlim = c(4,12), 
     ylim = c(12,25), xlab = "Annual Income (Lakhs)",
     ylab = "Househodl Area(Ft2)", pch = c(21,19)[as.numeric(df$Ownership)])

# Ad-hoc line
df[df$Annual_Income > 5 & df$Annual_Income < 6 &
     df$Household_Area > 20 & df$Annual_Income < 22, c(1,2)]
df[df$Annual_Income > 8 & df$Annual_Income < 9 &
     df$Household_Area > 16 & df$Annual_Income < 18, c(1,2)]

segments(5.3,22,8.5,15)
slope = atan2(22-15, 5.3-8.5)
segments(5.3 + 2*cos(slope), 22 + 2*sin(slope),
         5.3,22)

segments(8.5,15,
         8.5-2*cos(slope), 15 -2*sin(slope))
 # DA line using contour function
slen = 500
xmm = seq(min(df$Annual_Income), max(df$Annual_Income), length.out = slen)
ymm = seq(min(df$Household_Area), max(df$Household_Area), length.out = slen)

dfxymm = expand.grid(Annual_Income = xmm, Household_Area = ymm)
modplot = predict(mod, dfxymm)$class
modplot = as.numeric(modplot)
modplotM = matrix(modplot, slen, slen) # byrow = F
contour(xmm,ymm, modplotM, levels = c(1,2), add = T, drawlabels = F,
        col = "gray")

#DA line by computing intercept and slope
A = mod$scaling; A
S = A %*% t(A); S
P = mod$prior; P
C = mod$means; C
B1 = S %*% (C[1,]-C[2,]); B1
B2 = -(1/2) * t(C[1,] + C[2, ]) %*% S %*% (C[1,]-C[2,]) + log(P[1]/P[2]); B2
slopeCL = -B1[1]/B1[2]; slopeCL
intercept = -B2/B1[2]; intercept

legend("bottomright", inset = 0.005,
       c("Ower", "Nonowner", "DA line", "Ad-hoc line", "Prediction"),
       pch = c(19,21,NA,NA,NA), lty = c(NA,NA,1,1,NA),
       col = c("black","black","black","black","gray"),
       cex = 0.7, x.intersp = 0.5, y.intersp = 0.5)

clip(4,10,14,24)
abline(intercept, slopeCL, col = "blue")

#UsedCars.xlsx
df2=read.xlsx(file.choose(),1,header = T)
df2 = df2[, !apply(is.na(df2),2,all)]
df2 = df2[!apply(is.na(df2),1,all), ]
head(df2)

Age = 2017 - df2$Mfg_Year
df2 = cbind(df2, Age)

df2b = df2
df2 = df2[,-c(1,2,3,7)]
str(df2)

df2$Transmission = as.factor(df2$Transmission)
df2$C_Price = as.factor(df2$C_Price)

str(df2)

#Partioning (90%: 10%)
partidx = sample(1:nrow(df2), 0.9 * nrow(df2), replace = F)
df2train = df2[partidx,]
df2test = df2[-partidx, ]

# Identifying near zero variance predictors
library(caret)
x=nearZeroVar(df2train, saveMetrics = T)
x[x[, "zeroVar"] + x[, "nzv"] > 0, ]

library(MASS)
mod1 = lda(C_Price ~., df2train); mod1

mod1scorec = predict(mod1)$class
mod1score = predict(mod1)$x
mod1prob = predict(mod1)$posterior

table("Actual class"= df2train$C_Price,
      "Predicted Class" = factor(mod1scorec, levels = c("0","1")))

#Classification accuracy
mean(mod1scorec == df2train$C_Price)

#Misclaasification error
mean(mod1scorec != df2train$C_Price)

head(data.frame("Predicted Class" = mod1scorec,
                "Actual Class" = df2train$C_Price,
                "ScoreLD1" = mod1score[,1],
                "Prob.class 0" = mod1prob[,1],
                "Prob.class 1" = mod1prob[,2],
                df2train[,-c(7)]))




