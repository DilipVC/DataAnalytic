# Logistics Regression

library(xlsx)

# Load fight details dataset
df1= read.xlsx(file.choose(),1,header = T)
df1 = df1[, !apply(is.na(df1),2,all)]
df1 = df1[!apply(is.na(df1),1,all),]
head(df1)
str(df1)

dfb = df1
df1$STD = strptime(format(df1$STD, "%H:%M:%S"), "%H:%M:%S")
df1$ATD = strptime(format(df1$ATD, "%H:%M:%S"), "%H:%M:%S")
df1$STA = strptime(format(df1$STA, "%H:%M:%S"), "%H:%M:%S")
df1$ATA = strptime(format(df1$ATA, "%H:%M:%S"), "%H:%M:%S")
str(df1)
head(df1)

# Break departure time into appropriate inervals
range(df1$ATD)
breaks1 = strptime("06:00:00", "%H:%M:%S")
breaks2 = strptime("18:00:00", "%H:%M:%S")
DEPT = ifelse(df1$ATD < breaks2 & df1$ATD >= breaks1, 
              "Day", "Night")

df1 = cbind(df1, DEPT)
df1$DEPT = as.factor(df1$DEPT)
df1$Day = as.factor(df1$Day)
levels(df1$Day)
levels(df1$Day) = c("Sunday","Monday")
df1$FLTIME = as.difftime(as.character(df1$FLTIME))

str(df1)
head(df1)

dfb1 = df1
df1 = df1[,-c(1,2,5:8,10, 12)]

str(df1)
head(df1)

levels(df1$Flight.Status)
levels(df1$Flight.Status) = c(1,0)
head(df1$Flight.Status)
df1$Flight.Status = relevel(df1$Flight.Status, ref = "0")

# Partioning : 90% : 10%
partidx = sample(1:nrow(df1), 0.9*nrow(df1), replace = F)
df1train = df1[partidx, ]
df1test = df1[-partidx, ]

mod3 = glm(Flight.Status ~ ., family = binomial(link = "logit"), data=df1train)

# Options(sipen = 999)
summary(mod3)

#Measure of Goodness of fit
gf = c(mod3$df.residual, mod3$deviance,
       100*table(df1train$Flight.Status)[[1]]/
         length(df1train$Flight.Status), mod3$iter,
       1 - (mod3$deviance/mod3$null.deviance))
gf = as.data.frame(gf, optional = T)
rownames(gf) = c("Residual df", "Std. Dev. Estimate",
                 "%Success in training data", "#Iterations used",
                 "Multiple R-Squared")
gf

#Classify observations using a cutoff value of 0.5
mod3trainc = ifelse(mod3$fitted.values > 0.5, 1, 0)

table("Actual value" = df1train$Flight.Status,
      "Predicted value" = mod3trainc)
# Classification accuracy
mean(mod3trainc == df1train$Flight.Status)
# Misclassification error
mean(mod3trainc != df1train$Flight.Status)

# Score test partition for probability values
mod3test = predict(mod3, df1test[ , -c(4)], type = "response")
#Score test partition for logit values
mod3testl = predict(mod3, df1test[ , -c(4)], type = "link")
# Classify observations using a cutoff value of 0.5
mod3testc = ifelse(mod3test > 0.5, 1, 0)

table("Actual value" = df1test$Flight.Status,
      "Predicted value" = mod3testc)
# Classification accuracy
mean(mod3testc == df1test$Flight.Status)
# misclassification error
mean(mod3testc != df1test$Flight.Status)

# Cumulative Lift curve
# Training partition
df1lift2 = data.frame("Probability of class 1" = mod3$fitted.values,
                      "Actual Class" = as.numeric(as.character(df1train$Flight.Status)),
                      check.names = F)
head(df1lift2)
df1lift2 = df1lift2[order(df1lift2[,1], decreasing = T), ]
head(df1lift2)
CumActualClass3 = cumsum(df1lift2[,2])
df1lift2 = cbind(df1lift2, CumActualClass3)
head(df1lift2)

range(1:nrow(df1lift2))
range(df1lift2$CumActualClass3)
plot(1:nrow(df1lift2), df1lift2$CumActualClass3, type = "l",
     xlab = "#cases", ylab = "Cumulative",
     xlim = c(0,100), ylim = c(0,45))
segments(1,1,nrow(df1lift2), df1lift2$CumActualClass3[nrow(df1lift2)], lty=3)
legend(40,10,inset=0.005,
       c("Cumulative Flight Status when sorted using predicted values",
         "Cumulative filght status using average"), 
       lty = c(1,2), bty = "n", cex = 0.7, x.intersp = 0.3, y.intersp = 0.5)

# Test partition
df1lift = data.frame("Probability of class 1" = mod3test,
                     "Actual Class" = as.numeric(as.character(df1test$Flight.Status)),
                     check.names = F)
head(df1lift)
df1lift = df1lift[order(df1lift[,1], decreasing = T), ]
head(df1lift)
CumActualClass2 = cumsum(df1lift[ ,2])
df1lift = cbind(df1lift, CumActualClass2)
head(df1lift)

range(1:nrow(df1lift))
range(df1lift$CumActualClass2)
plot(1:nrow(df1lift), df1lift$CumActualClass2, type="l",
     xlab = "#Cases", ylab = "Cumulative", xlim = c(0,12), ylim = c(0,6))
segments(1,0,nrow(df1lift), df1lift$CumActualClass2[nrow(df1lift)], lty = 3)
legend(5, 2, inset = 0.005,
       c("Cumulative Flight Status When sorted using predicted values",
         "Cumulative Flight status when sorted using average"),
       lty = c(1,2), bty = "n", cex = 0.7, x.intersp = 0.3, y.intersp = 0.5)





