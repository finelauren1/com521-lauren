#PC0 Load Data
library(foreign)
> lilypad_anonymized<-read.dta("~/Downloads/lilypad_anonymized.dta")
View(lilypad_anonymized)
lp<-lilypad_anonymized

#PC1
#a. Reproduce both Table 1 and Table 2
table(lp$gender, lp$order_type)
gen.table<-table(lp$gender, lp$order_type)

US.gen.table<-table(lp$gender, lp$order_type, lp$country=="United States")
#The second of the two tables in produced is the one we want here. Not sure how to get rid of the first



#b.Run a X^2 test on both tables. Compare to the paper.
chisq.test(gen.table)
#got same result

chisq.test(US.gen.table)
#Once I figure out how to get rid of second table, this should work.


#c.Install the package "gmodels" and try to display the table using the function CrossTable()
install.packages("gmodels")
library(gmodels)
CrossTable(gen.table)
CrossTable(US.gen.table)

#d.export the output of your table
install.packages("xtable")
library(xtable)
gen.table.export<-xtable(gen.table)
print.xtable(gen.table.export, type="html", file="gentable.html")
#well, I following the instructions I found online and nothing happened.

#PC2 42 and 19 participants becomes 31 and 14. Compare attrition in groups use prop.test and X^2
# is there evidence of dependence between instructor and attrition
day.1<-c(42,19)
day.2<-c(31,14)
attrition<-data.frame(day.1,day.2)
rownames(attrition)<-c("mako","tommy")
attrition<-as.matrix(attrition)

chisq.test(attrition)
prop.test(attrition)
#this is giving me the wrong numbers

flipped.attrition<-t(attrition)
prop.test(flipped.attrition[2,], flipped.attrition[1,])

#PC3
#a. Download and imput data
library(foreign)
Halloween2012_2014_2015_PLOS <- read_dta("~/Downloads/Halloween Dataverse Files/Halloween2012-2014-2015_PLOS.dta")
eating<-Halloween2012_2014_2015_PLOS


#b. recode some data--Michelle's face and whether they picked up fruit
table(eating$obama, eating$fruit)
fruit.obama<-table(eating$obama, eating$fruit)
colnames(fruit.obama)<-c("no fruit", "fruit")
rownames(fruit.obama)<-c("didn't see Michelle", "saw Michelle")

#c. Are two groups dependent? Would another test have different results? 
chisq.test(fruit.obama)
#this doesn't suggest strong enough evidence for dependence. If we controlled for more variables
#by using more of the data, perhaps we would find a link. 


