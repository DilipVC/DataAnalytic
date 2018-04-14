# Multiple Linear regression model for a categorical response
#Data set = Promoffer.xls
df2 = read.xlsx(file.choose(),1,header = T)
df2 = df2[ ,!apply(is.na(df2),2,all)]
df2 = df2[!apply(is.na(df2),2,all), ]
str(df2)

dfb2 = df2
df2 = df2[,c(1,3,7,9)]

str(df2)

# Partitioning: Tr:Te ==> 60%:40%
partidx = sample(1:nrow(df2), 0.6*nrow(df2), replace = F)
df2train = df2[partidx, ]

mod4 = lm(Promoffer ~., df2train)

mod4summ = summary(mod4); mod4summ
mod4ava = anova(mod4);mod4ava

DF=c(mod4summ$fstatistic[["numdf"]],
     mod4summ$fstatistic[["dendf"]],
     mod4summ$fstatistic[["numdf"]]+mod4summ$fstatistic[["dendf"]])

SS = c(sum(head(mod4ava[ , "Sum Sq"], -1)),
       mod4ava["Residuals", "Sum Sq"],
       sum(mod4ava[ , "Sum Sq"]))

MS = c(mean(head(mod4ava[, "Mean Sq"], -1)),
       mod4ava["Residuals", "Mean Sq"], "")

Fstatistic = c(mod4summ$fstatistic[["value"]], "", "")

P = pf(mod4summ$fstatistic[[1]], mod4summ$fstatistic[[2]], mod4summ$fstatistic[[3]],
       lower.tail = F)
pvalue = c(P,"","")

anovadf = data.frame(DF, SS, MS, Fstatistic, pvalue)
rownames(anovadf) = c("Regression", "Error", "Total")
anovadf

# Prediction for a new observation:
# Annual income of '5 lakhs with two family members
# Who is not active online
predict(mod4, data.frame(Income = 5, Family.size = 2, Online = 0))
#Set of values for promoffer
row.names(table(df2$Promoffer))

range(mod4$residuals)
hist(mod4$residuals, main = "", xlab = "Residuals", xlim = c(-1,1))

#Cleanup
rm(list = ls())
rm()
