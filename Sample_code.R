# Simple example 

library(xlsx)

# Import data 
df = read.xlsx(file.choose(),1,header = T)
df = df[ , !apply(is.na(df),2,all)]
head(df)
summary(df)

# par(mfrow(1,2), cex=0.6, mar=c(8,4,6,0), oma=c(1,1,1,1))

plot(df$Annual_Income, df$Household_Area, las=1,
     xlab = "Annual Income (a.lakhs)",
     ylab = "Household Area (00s ft2)",
     xlim = c(2,12), ylim = c(13,25), 
     pch = c(21,19)[as.numeric(df$Ownership)])

legend("bottomright", inset=0.005, c("Owner", "Nonowner"),
       pch = c(19,21), cex = 0.7, x.intersp = 0.5, y.intersp = 0.5)

# Method 1 : set of horizontal and vertical lines
df[df$Annual_Income>5 & df$Annual_Income < 8.5 & df$Household_Area > 18
   & df$Household_Area < 20, c(1,2)]
abline(h=18.8, col=3)
segments(7, 0, 7, 18.8, col = 3)
segments(5.8, 18.8, 5.8, 26, col = 3)

df[df$Annual_Income > 6 & df$Annual_Income < 8.5 & df$Household_Area > 18
   & df$Household_Area < 21, c(1,2)]
segments(5.8, 19.5, 13, 19.5, col = 3)

df[df$Household_Area >17 & df$Household_Area < 19 & df$Annual_Income < 7,c(1,2)]
segments(0, 18.2, 7, 18.2, col=3)

######
plot(df$Annual_Income, df$Household_Area, las=1,
     xlab = "Annual Income (a.lakhs)",
     ylab = "Household Area (00s ft2)",
     xlim = c(2,12), ylim = c(13,25), 
     pch = c(21,19)[as.numeric(df$Ownership)])
legend("bottomright", inset=0.005, c("Owner", "Nonowner"),
       pch = c(19,21), cex = 0.7, x.intersp = 0.5, y.intersp = 0.5)

# Method 2: A single diagonal line
df[df$Annual_Income > 5 & df$Annual_Income < 6 & df$Household_Area > 20 &
     df$Household_Area < 23, c(1,2)]
df[df$Annual_Income > 8 & df$Annual_Income < 9 & df$Household_Area > 16 &
     df$Household_Area < 18, c(1,2)]
segments(5.4,22,8.4,16, col=3)

slope = atan2(22-16, 5.4-8.5)
segments(5.4+10*cos(slope), 22+10*sin(slope),
         5.4,22, col=3)
segments(8.4 ,16, 8.4 - 20*cos(slope), 16-20*sin(slope), col = 3)

#########




