library(readr)
CSS_TRENDS_94_06 <- read_csv("~/Downloads/CSS.TRENDS.94.06.csv")
pv.SF<-data.frame(CSS_TRENDS_94_06$YEAR, 
                CSS_TRENDS_94_06$ACERECODE, 
                CSS_TRENDS_94_06$SUBJID, 
                CSS_TRENDS_94_06$STRAT, 
                CSS_TRENDS_94_06$STATE, 
                CSS_TRENDS_94_06$SEX, 
                CSS_TRENDS_94_06$SRELIGION, 
                CSS_TRENDS_94_06$RACEGROUP, 
                CSS_TRENDS_94_06$POLIVIEW, 
                CSS_TRENDS_94_06$POLIVIEW_TFS, 
                CSS_TRENDS_94_06$MAJOR1A, 
                CSS_TRENDS_94_06$GENACT11, 
                CSS_TRENDS_94_06$CLIMATE8)

library(plyr)
pv.SF<-rename(pv.SF, c("CSS_TRENDS_94_06.YEAR"="year", 
                     "CSS_TRENDS_94_06.ACERECODE"="inst", 
                     "CSS_TRENDS_94_06.SUBJID"="studentID",
                     "CSS_TRENDS_94_06.STRAT"="inst.class", 
                     "CSS_TRENDS_94_06.STATE"="state", 
                     "CSS_TRENDS_94_06.SEX"="sex",
                     "CSS_TRENDS_94_06.SRELIGION"="religion",
                     "CSS_TRENDS_94_06.RACEGROUP"="race", 
                     "CSS_TRENDS_94_06.POLIVIEW"="PVS", 
                     "CSS_TRENDS_94_06.POLIVIEW_TFS"="PVF", 
                     "CSS_TRENDS_94_06.MAJOR1A"="major", 
                     "CSS_TRENDS_94_06.GENACT11"="Pol.Discuss",
                     "CSS_TRENDS_94_06.CLIMATE8"="inst.pol"))

#I only want to look at data that has Poliview for both senior and freshman year
pv.SF<-pv.SF[complete.cases(pv.SF[,9:10]),]

pv.SF["pol.diff"]<- pv.SF$PVS-pv.SF$PVF
pv.SF$male<-pv.SF$sex==1
pv.SF$yearsince1994<-pv.SF$year-1994

table(pv.SF$pol.diff)

table(pv.SF$state)
#looks like all states except Alaska and Utah are represented, though not equally.


hist(pv.SF$pol.diff)
#most don't change. When the do, it seems to move in both directions

table(pv.SF$PVS)
hist(pv.SF$PVS)
#most people are in the middle. Slightly more on left than on right at the end of college.

table(pv.SF$PVF)
hist(pv.SF$PVF)
#freshman have fewer liberals, more conservatives, and more moderates--maybe there is a shift left.

summary(pv.SF$pol.diff)
summary(pv.SF$PVS)
summary(pv.SF$PVF)
sd(pv.SF$PVS)

t.test(pv.SF$PVS, pv.SF$PVF, paired=TRUE)
SvF.t<-t.test(pv.SF$PVS, pv.SF$PVF, paired=TRUE)

summary(lm(PVS~PVF, data=pv.SF))

#let's see how year affects it.
summary(lm(PVS~PVF+yearsince1994, data=pv.SF))
summary(lm(abs(pol.diff)~year, data=pv.SF))
summary(lm(pol.diff~yearsince1994, data=pv.SF))

hist(pv.SF$yearsince1994)


table(pv.SF$pol.diff)
#4 and 3 on either side are evidence of swithcing


summary(lm(PVS~yearsince1994, data=pv.SF))
#there is a statistically significant change over time, but it is tiny


#Use faculty data to get a politics score for the institution
library(haven)
FAC_TRENDS_89_98 <- read_sav("~/Downloads/FAC.TRENDS.89.98.SAV")
fac<-data.frame(FAC_TRENDS_89_98$YEAR, 
                FAC_TRENDS_89_98$ACERECODE, 
                FAC_TRENDS_89_98$SUBJID, 
                FAC_TRENDS_89_98$POLIVIEW)

fac<-rename(fac, c("FAC_TRENDS_89_98.YEAR"="year", 
                   "FAC_TRENDS_89_98.ACERECODE"="inst", 
                   "FAC_TRENDS_89_98.SUBJID"="facultyID", 
                   "FAC_TRENDS_89_98.POLIVIEW"="Poliview"))

table(fac$Poliview)
hist(fac$Poliview)

fac98<-subset(fac, year==1998)
fac98<-fac98[complete.cases(fac98[,4]),]


means.inst<-aggregate(fac98[,4], list(fac98$inst), mean)
colnames(means.inst)<-c("inst", "inst.PV")


#Recode state by politics
state<-levels(pv.SF$state)
state.pol<-c("R","S","S","D","R","D","D","S","R","D","S","R","D","R","R",
             "S","S","D","D","D","D","D","S","R","R","R","R","R","S","D",
             "S","S","D","S","R","D","D","NA","D","R","R","S","R","R","NA",
             "D","D","D","S","R")
s.sp<-data.frame(state, state.pol)
df.statepol<-merge(pv.SF, s.sp, by.x="state", by.y="state")

pv.recoded<-merge(df.statepol, means.inst, by.x="inst", by.y="inst")

table(pv.recoded$state.pol)
#This illustrates that we have a lot more representation from D states than R states. 

#recode major by category
major.categories<-c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
major.types<-c("SS", "STEM", "SS", "SS", "STEM", "Hum", "Other", "SS", "Hum", "Hum", 
               "STEM", "STEM", "SS", "STEM", "Other", "Other")
majors<-data.frame(major.categories, major.types)
pv.recoded<-merge(pv.recoded, majors, by.x="major", by.y="major.categories")

pv.recoded<-merge(pv.recoded, means.inst, by.x="inst", by.y="inst")
#pv.recoded now has major categories, state politics, inst politics, gender, race
#in order to categorize all of these things, we had to exclude quite a bit--now we only have 103,090

#fix major types order
pv.recoded$major.types<-as.factor(pv.recoded$major.types)
levels(pv.recoded$major.types)
pv.recoded$major.types<- factor(pv.recoded$major.types,levels(pv.recoded$major.types)[c(3,4,1:2)])

#add a variable that separates race according to white or not white
pv.recoded$non.white<-pv.recoded$race!=5

pv.recoded<-pv.recoded[complete.cases(pv.recoded[,15,19]),]
pv.recoded<-pv.recoded[complete.cases(pv.recoded[,21]),]

aggregate(pv.recoded$PVS, list(pv.recoded$yearsince1994), mean)
aggregate(pv.SF$PVS, list(pv.SF$yearsince1994), mean)
#This shows the average for each year. Students don't seem to be moving further left 
#(when you run the test, it is statistically significant but not practically significant)

fac.complete<-fac[complete.cases(fac[,4]),]
aggregate(fac.complete$Poliview, list(fac.complete$year), mean)
#The average for professors is noticably more liberal than for students
#however, prior to 98, the shift wasn't substatial, as the graph will show you. 
#The shift happens the most from 1998 to 2008 in professors, as the heterodoxy research shows. 

mean(fac.complete$Poliview)
mean(pv.SF$PVS)
#Those are pretty substantially different

t.test(fac.complete$Poliview, pv.SF$PVS)
st98<-subset(pv.SF, year==1998)
t.test(fac98$Poliview, st98$PVS)
#Here's proof that faculty and students are substantially different in their political views

#ok, linear regression time
#Linear regressions witn PVS as dependent variable
summary(lm(PVS~PVF, data=pv.recoded))
summary(lm(PVS~PVF+male, data=pv.recoded))
summary(lm(PVS~PVF+major.types, data=pv.recoded))
summary(lm(PVS~PVF+state.pol, data=pv.recoded))
summary(lm(PVS~PVF+inst.PV.y, data=pv.recoded))
summary(lm(PVS~PVF+yearsince1994, data=pv.recoded))
summary(lm(PVS~PVF+non.white, data=pv.recoded))

summary(lm(PVS~male, data=pv.recoded))
summary(lm(PVS~major.types, data=pv.recoded))
summary(lm(PVS~state.pol, data=pv.recoded))
summary(lm(PVS~inst.PV.y, data=pv.recoded))
summary(lm(PVS~yearsince1994, data=pv.recoded))
summary(lm(PVS~non.white, data=pv.recoded))
#Year has a statistically significant impact on PVS, but it's so small, I'm going to exclude it from now on
#intitution's politics was the second biggest predictor of PVS (after PVF). Does that hold true for PVF as well?

summary(lm(PVF~inst.PV.y, data=pv.recoded))
#it is also a large predictor for PVF, suggesting students may choose their university based on its politics

#Race, then gender, then major seem to have large effects as well
summary(lm(PVS~PVF+inst.PV.y+major.types, data=pv.recoded))
summary(lm(PVS~PVF+inst.PV.y+major.types+state.pol, data=pv.recoded))
#Adding state politics didn't increase the R-squared much

summary(lm(PVS~PVF+inst.PV.y+major.types+non.white, data=pv.recoded))
#race did increase the R-squred

summary(lm(PVS~PVF+inst.PV.y+major.types+non.white+male, data=pv.recoded))
#adding male increased the R-squared

summary(lm(PVS~PVF+inst.PV.y+major.types+male+non.white+state.pol, data=pv.recoded))
#Once again, the state's politics seems to have a minimal effect.

#Overall, this seems to be the most meaningful model
summary(lm(PVS~PVF+inst.PV.y+major.types+non.white+male, data=pv.recoded))

summary(lm(PVS~PVF, data=pv.recoded))
#explains 31%
summary(lm(PVS~PVF+inst.PV.y, data=pv.recoded))
#explains 33.2%
summary(lm(PVS~PVF+inst.PV.y+major.types, data=pv.recoded))
#explains 33.7%
summary(lm(PVS~PVF+inst.PV.y+major.types+non.white, data=pv.recoded))
#explains 33.9%
summary(lm(PVS~PVF+inst.PV.y+major.types+non.white+male, data=pv.recoded))
#explains 34.2%

full.PVS<-lm(PVS~PVF+inst.PV.y+major.types+non.white+male, data=pv.recoded)

#Just for fun, I try it with pol.discuss (how often they talked about politics).
#This only includes half the data because we only have this answer for 2001 to 2006
summary(lm(PVS~PVF+inst.PV.y+major.types+non.white+male+Pol.Discuss, data=pv.recoded))

#Interesting--political discussion doesn't seem to have a statistically significant effect.


#OK, now I need to do the same thing with with political difference
summary(lm(pol.diff~male, data=pv.recoded))
summary(lm(pol.diff~major.types, data=pv.recoded))
#his shows the basic differences between majors. SS and STEM are about equal, at around .07diff
#Other is only about .05, and Hum is about .18
summary(lm(pol.diff~state.pol, data=pv.recoded))
summary(lm(pol.diff~inst.PV.y, data=pv.recoded))
#this shows that for every 1 pt increase in the institution politics, the students will have a change of .05
summary(lm(pol.diff~non.white, data=pv.recoded))
#Interesting. Major has the biggest impact on political change, while inst.pol has the biggest impact on PVS

summary(lm(pol.diff~inst.PV.y+state.pol, data=pv.recoded))
#it seems that when you account for the school's politics, the state's politics is no longer statistically significant
#I will exclude stat's politics from this model as well

summary(lm(pol.diff~major.types, data=pv.recoded))
#explains .295%
#Also, this shows the basic differences between majors. SS and STEM are about equal, at around .07diff
#Other is only about .05, and Hum is about .18
summary(lm(pol.diff~major.types+inst.PV.y, data=pv.recoded))
#explains .37%
summary(lm(pol.diff~major.types+inst.PV.y+male, data=pv.recoded))
#explains .385%
summary(lm(pol.diff~major.types+inst.PV.y+male+non.white, data=pv.recoded))
#explains .389% of variability

full.pol.diff<-lm(pol.diff~major.types+inst.PV.y+male+non.white, data=pv.recoded)

summary(lm(pol.diff~major.types+inst.PV.y+male+non.white+Pol.Discuss, data=pv.recoded))
#It does seem to have an affect on the political difference, but it makes the race variable disappear
#now the R2 is .0070, so it explains .7% of the variability. What do I make of that
#it also cuts my dataset in half, so I don't think I'll include it in the final model.

#Time to make tables
table(pv.recoded$PVF, pv.recoded$PVS)
prop.table(table(pv.recoded$PVF, pv.recoded$PVS))

corr.data<-data.frame(pv.recoded$PVF, 
                      pv.recoded$PVS, 
                      pv.recoded$pol.diff, 
                      pv.recoded$inst.PV.y, 
                      pv.recoded$major.types, 
                      pv.recoded$male, 
                      pv.recoded$non.white)
library(plyr)
corr.data<-rename(corr.data, c("pv.recoded.PVF"="PVF", 
                  "pv.recoded.PVS"="PVS", 
                  "pv.recoded.pol.diff"="pol.diff", 
                  "pv.recoded.inst.PV.y"="inst.PV.y", 
                  "pv.recoded.major.types"="major.types", 
                  "pv.recoded.male"="male", 
                  "pv.recoded.non.white"="non.white"))
corr.data$male<-as.numeric(corr.data$male)
corr.data$non.white<-as.numeric(corr.data$non.white)
corr.data$SS<-corr.data$major.types=="SS"
corr.data$STEM<-corr.data$major.types=="STEM"
corr.data$Hum<-corr.data$major.types=="Hum"
corr.data$Other<-corr.data$major.types=="Other"
corr.data$major.types=NULL
corr.data$SS<-as.numeric(corr.data$SS)
corr.data$STEM<-as.numeric(corr.data$STEM)
corr.data$Hum<-as.numeric(corr.data$Hum)
corr.data$Other<-as.numeric(corr.data$Other)

cor(corr.data, use="complete")

#output of tables
screenreg(full.pol.diff)
htmlreg(full.pol.diff)

screenreg(full.PVS)
htmlreg(full.PVS)

stargazer(cor(corr.data, use="complete"), type="html")

#look at risiduals and distributions of variables.
summary(pv.recoded$inst.PV.y)
hist(pv.recoded$inst.PV.y)
sd(pv.recoded$inst.PV.y)
#that's somewhat irregular, a little left skewed.

summary(pv.recoded$major.types)
summary(pv.recoded$male)
summary(pv.recoded$non.white)

summary(pv.recoded$state.pol)
#This ended up not being relevant, so I probably don't need to include it. 

#for the original data
summary(pv.SF$PVF)
sd(pv.SF$PVF)
summary(pv.SF$PVS)
sd(pv.SF$PVS)
#for the limited data
summary(pv.recoded$PVF)
sd(pv.recoded$PVF)
summary(pv.recoded$PVS)
sd(pv.recoded$PVS)

#for the 1998 students and faculty
summary(st98$PVS)
sd(st98$PVS)
summary(fac98$Poliview)
sd(fac98$Poliview)

summary(pv.recoded$pol.diff)
sd(pv.recoded$pol.diff)


#Residuals
hist(residuals(full.pol.diff))
hist(residuals(full.PVS))

#come back and plot residuals later.

#do a few examples for each model
#PVS model
#a white man studying social science who is moderate at a perfectly moderate university
#now same, but a liberal at a conservative university
#now with him studying humanities
#now make him a minority woman

#a minority woman studying Humanities at a liberal university who is liberal
#a white man studying "other" at a conservative university who is conservative

#pol.diff model
#a white man studying social science at a perfectly moderate university
#now at a conservative university
#now at a liberal university
#now with him studying humanities
#now make him a minority woman

#a minority woman studying Humanities at a liberal university
#a white man studying "other" at a conservative university
