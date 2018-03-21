#Classification tree

library(xlsx)

# Import the dataset
df = read.xlsx(file.choose(),1,header = T)
df = df[, !apply(is.na(df),2,all)]

str(df)

data.frame("Household Number" = 1:20,
           "Annual Income(a.'lakhs)" = df$Annual_Income,
           "Household Area(00s ft2" = df$Household_Area,
           "Ownership of Sedan Car" = df$Ownership,
           check.names = F)

par(mar = c(5.1,5.1,5.1,5.1))
range(df$Annual_Income)
range(df$Household_Area)

plot(df$Annual_Income, df$Household_Area, las = 1,
     xlab = "Annual Income(a.'lakhs)",
     ylab = "Household Area(00s ft2", 
     xlim = c(2,12), ylim = c(13,25),
     pch = c(21,19)[as.numeric(df$Ownership)])

legend("bottomright", inset = 0.005, c("Owner", "NonOwner"),
       pch = c(19,21), cex=0.7, x.intersp=0.5, y.intersp=0.5)

#First Split
abline(h=18.8)

#Possible set of split values
#For numerical variable
#Midpoint between pairs of consecutive values for a variable.
  #Which are ranked as per the impurity (Heterogeneity) reduction
  #in the resulting rectangular parts
sort(df$Annual_Income)
head(sort(df$Annual_Income), -1) + diff(sort(df$Annual_Income)/2)
sort(df$Household_Area)
head(sort(df$Household_Area), -1) + diff(sort(df$Household_Area)/2)

# For categorical variables
# Set of categories is divided into two subsets

#Gini impurity index and Entropy measure
# Plot og Gini vs P1(Proportion of Observations in class 1)
  # for a two-class case
P1 = seq(0, 1, 0.1)
gini = NULL
for(i in 1:length(P1)) {
  gini[i] = 1 - (P1[i]^2 + (1-P1[i])^2)
}
plot(P1, gini, ylab="Gini index", type = "l")

#Plot of entropy vs P1(Proportion of Observation in class 1)
  #for a two - class case
entropy = NULL
for(i in 1:length(P1)){
  entropy[i] = -(P1[i]*log2(P1[i]) + (1 - P1[i])*log2(1-P1[i]))
}

plot(spline(P1, entropy), type = "l", xlab = 'P1', ylab="Entropy Measures")

#First Split in SedanCar example
summary(df$Ownership)

#Impurity before spilt
giorg = 1 -(10/20)^2 - (10/20)^2
emorg = -(10/20)*log2(10/20) - (10/20)*log2(10/20)

#upper rectangle
giniurec = 1 - (7/10)^2 - (3/10)^2
emurec=-(7/10)*log2(7/10) - (3/10)*log2(3/10)
ginilrec = giniurec
emlrec = emurec

ginisplit1=(10/20)*giniurec + (10/20)*ginilrec
emsplit1 = (10/20)*emurec + (10/20)*emlrec

ginidelta = ginisplit1 - giorg
emdelta = emsplit1 - emorg

# Second split
segments(7,0,7,18.8)

#Final stage
segments(5.8,18.8,5.8,26)
segments(5.8,19.5,13,19.5)
segments(0,18.2,7,18.2)

str(df)
library(rpart)

#Method = 'class' for a classification tree
#Method = "Anove" for a regression tree

mod = rpart(Ownership ~., method="class", data= df,
            control=rpart.control(cp=0, minsplit = 2, minbucket = 1,
                                  maxcompete = 0, maxsurrogate = 0,
                                  xval = 0),
            parms = list(split="gini"))

par(mar=c(0,0,0,0), oma=c(0,0,0,0), xpd=NA)

plot(mod, uniform=T, branch=0.3, compress = T,
     margin = 0.1, nspace = 1)
text(mod, splits = T, use.n = T, all = F, minlength = 0, cex=0.8)

# Install.packages(rpart.plot)
library(rpart.plot)
prp(mod, type = 1, extra = 1, under = T, varlen = 0, cex = 0.7,
    compress = T, Margin = 0, digits = 0,
    split.cex = 0.8, under.cex = 0.8)

#Node numbering
prp(mod, type = 1, extra = 1, under = T, varlen = 0, cex = 0.7,
    compress = T, Margin = 0, digits = 0,
    split.cex = 0.8, under.cex = 0.8, nn = T, nn.cex = 0.6)

#First split
modsub = snip.rpart(mod, toss = c(6:7,12:13, 24:25))
prp(modsub, type = 1, extra = 1, under = T, varlen = 0, cex = 0.7,
    compress = T, Margin = 0, digits = 0,
    split.cex = 0.8, under.cex = 0.8)

# First three splits
modsub2 = snip.rpart(mod, toss = c(12:13, 24:25))
prp(modsub2, type = 1, extra = 1, under = T, varlen = 0, cex = 0.7,
    compress = T, Margin = 0, digits = 0,
    split.cex = 0.8, under.cex = 0.8)

attributes(mod)
summary(mod)











