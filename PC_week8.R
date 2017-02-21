#PC0 Load
library(readr)
week3_dataset_lauren <- read_csv("~/Desktop/OneDrive/Winter 2017/Statistics/com521-lauren/week3_dataset-lauren.csv")
w3d<-week3_dataset_lauren

#PC1. Run t.test
t.test(w3d$x, w3d$y)
#That just tells us there is a difference between x and y
summary(w3d)
plot(w3d$x, w3d$y)
#Plot is showing a correlation. Looks like slope is about 6. Let's run the stats.

#PC2.Estimate XY correlation.
cor.test(w3d$x, w3d$y)
lm(w3d)


#PC3. Recode data
w3d$j<-as.logical(w3d$j)
w3d$i<-as.logical(w3d$i)
w3d$k<-as.factor(w3d$k)

w3d$k<-factor(w3d$k, levels=c(0,1,2,3), labels=c("none", "some", "lots", "all"))

#PC4.Generate 3 linear models and interpret.
#a) 
xy.lm<-lm(x~y, data=w3d)
xy.lm

#b)
xyij.lm<-lm(x~y+i+j, data=w3d)
xyij.lm

#c)
xyijk.lm<-lm(x~y+i+j+k, data=w3d)
xyijk.lm

#interpret
summary(xy.lm)
summary(xyij.lm)
summary(xyijk.lm)

#PC5. Generate Residual Plots for C (above).

#a) Interpret Histograms.
hist(residuals(xyijk.lm))

#b)Plot the residuals by your values of x, i, j, and k (four different plots).
plot(w3d$x, residuals(xyijk.lm))
plot(w3d$i, residuals(xyijk.lm))
plot(w3d$j, residuals(xyijk.lm))
plot(w3d$k, residuals(xyijk.lm))
# last one has characters, so it gives a box plot. I could change them to numbers and it would do scatter

#c) A QQ plot to evaluate the normality of residuals assumption.
qqnorm(residuals(xyijk.lm))

#PC6. Generate Publication ready table
library(stargazer)
stargazer(xyijk.lm, type="text")

#PC7. Load michelle Obama data.
library(haven)
Halloween2012_2014_2015_PLOS <- read_dta("~/Downloads/Halloween Dataverse Files/Halloween2012-2014-2015_PLOS.dta")
Halloween<-Halloween2012_2014_2015_PLOS

#a) compute linear model
fruitobama.lm<-lm(obama~fruit, data=Halloween)
fruitobama.lm

#b) add a control for age and year
fr.obam.age.year.lm<-lm(obama~fruit+age+treat_year, data=Halloween)
fr.obam.age.year.lm

#PC7. Residuals for Model A
residuals(fruitobama.lm)
hist(residuals(fruitobama.lm))
plot(Halloween$obama, residuals(fruitobama.lm))
#this is returning an error for some reason

#PC8. Run 7a) 3 times with just 2012, just 2014, and just 2015 data.
year2012<-subset(Halloween, year==2012)
year2014<-subset(Halloween, year==2014)
year2015<-subset(Halloween, year==2015)

obam.fr.2012.lm<-lm(obama~fruit, data=year2012)
obam.fr.2012.lm

obam.fr.2014.lm<-lm(obama~fruit, data=year2014)
obam.fr.2014.lm

obam.fr.2015.lm<-lm(obama~fruit, data=year2015)
obam.fr.2015.lm