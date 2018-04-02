# Logistics Regression

library(xlsx)

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

#Break departure time into appropriate intervals
range(df1$ATD)
breaks = seq(strptime("00:00:00", "%H:%M:%S"), strptime("24:00:00", "%H:%M:%S"),
             by = "6 hours")
labelsv = c("0-6","6-12","12-18","18-24")
DEPT = cut(df1$ATD, breaks=breaks, right = F, labels = labelsv)

df1 = cbind(df1, DEPT)

df1$Day = as.factor(df1$Day)
levels(df1$Day)
levels(df1$Day)=c("Sunday", "Monday")
df1$FLTIME = as.difftime(as.character(df1$FLTIME))

str(df1)
head(df1)

dfb1 = df1
df1 = df1[,-c(1,3,5:8)]
str(df1)
head(df1)

df1[sample(1:nrow(df1),20, replace=F),]

levels(df1$Flight.Status)
levels(df1$Flight.Status) = c(1,0)
head(df1$Flight.Status)
df1$Flight.Status = relevel(df1$Flight.Status, ref = "0")
str(df1$Flight.Status)
head(df1$Flight.Status)

# Partioning: 90%, 10%
partidx = sample(1:nrow(df1), 0.9*nrow(df1), replace = F)
df1train = df1[partidx,]
df1test = df1[-partidx,1]

mod2 = glm(Flight.Status ~ ., family = binomial(link = "logit"), data = df1train)

#Options(scipen = 999)
summary(mod2)

#Classify observations using a cutoff value of 0.5
mod2trainc = ifelse(mod2$fitted.values > 0.5,1,0)

table("Actual value" = df1train$Flight.Status, "Predicted value" = mod2trainc)
#Classification accuracy
mean(mod2trainc == df1train$Flight.Status)
#Misclassification error
mean(mod2trainc !=df1train$Flight.Status)









