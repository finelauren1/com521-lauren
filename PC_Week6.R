#PC0. Load Data
library(readxl)
owan03 <- read_excel("~/Desktop/OneDrive/Winter 2017/Statistics/com521-lauren/owan03.xlsx")
View(owan03)

#PC1. Clean Data
#I cheated and did this in excel because it seemed easier.
library(readxl)
RedDyeData <- read_excel("~/Desktop/OneDrive/Winter 2017/Statistics/com521-lauren/RedDyeData.xlsx")
table(RedDyeData$`Level of Dosage`)

colnames(RedDyeData)<-c("Dosage", "TOD")
RedDyeData$Dosage<-as.factor(RedDyeData$Dosage)

#PC2.Create summary statistics and visualizations for each group. 
#Write code that allows you to generate a useful way to both 
#(a) get a visual sense both for the shape of the data and its relationships and 
#(b) the degree to which the assumptions for t-tests and ANOVA hold. 
#What is the global mean of your dependent variable?
Group1<-RedDyeData$TOD[which(RedDyeData$Dosage=="Low")]
Group2<-RedDyeData$TOD[which(RedDyeData$Dosage=="Medium")]
Group3<-RedDyeData$TOD[which(RedDyeData$Dosage=="High")]
Group0<-RedDyeData$TOD[which(RedDyeData$Dosage=="None")]

summary(Group0)
summary(Group1)
summary(Group2)
summary(Group3)

hist(Group0)
hist(Group1)
hist(Group2)
hist(Group3)


summary(RedDyeData)
#Global mean of dependant variable is 75.55


#PC3. Do a t-test between mice with none RD40 and mice with any (i.e., at least a small amount). 
#Next, run a t-test between the group with a high dosage and control group. 
#How would you go about doing it using formula notation? 
#Be ready to report, interpret, and discuss the results in substantive terms.

AnyR40<-c(Group1, Group2, Group3)
t.test(Group0, AnyR40)

t.test(Group0, Group3)

#Not sure about the formula notation. I tried this various ways of getting R to look at subsets
#of the dosage column, but can't figure out the right notation.


#PC4. Estimate an ANOVA analysis using aov() to see if there is a difference between the groups. 
#Be ready to report, interpret, and discuss the results in substantive terms.

aov(TOD~Dosage, data=RedDyeData)
TOD.Dosage.model<-aov(TOD~Dosage, data=RedDyeData)
summary(TOD.Dosage.model)

#There is a difference between groups
