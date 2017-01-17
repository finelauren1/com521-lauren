setwd("/users/laurenfine/desktop/onedrive/winter 2017/statistics/com521-lauren")
library(readr)
week3_dataset_lauren <- read_csv("/users/laurenfine/desktop/onedrive/winter 2017/statistics/com521-lauren/week3_dataset-lauren.csv")

load("~/Desktop/OneDrive/Winter 2017/Statistics/com521-lauren/week2_dataset-lauren copy.RData")

summary(week3_dataset_lauren)
apply(week3_dataset_lauren, 2, sd)
nrow(week3_dataset_lauren)
ncol(week3_dataset_lauren)
names(week3_dataset_lauren)
head(week3_dataset_lauren)
week3_dataset_lauren$x
table(week3_dataset_lauren$k)

hist(week3_dataset_lauren$x)
hist(week3_dataset_lauren$y)
hist(week3_dataset_lauren$j)
hist(week3_dataset_lauren$k)
hist(week3_dataset_lauren$i)

summary(week2.dataset)
summary(week3_dataset_lauren$x)
boxplot(week3_dataset_lauren$x, week2.dataset)
hist(week3_dataset_lauren$x)
hist(week2.dataset)
#They appear to be identical

table(sort(week2.dataset)==sort(week3_dataset_lauren$x))
#not identical

table(sort(round(week2.dataset))==sort(round(week3_dataset_lauren$x)))
#now identical

install.packages("ggplot2")
library(ggplot2)
ggplot(data=week3_dataset_lauren)+geom_point()+aes(x=x, y=y)
#I'm getting an error. I need to clean some data

week3_dataset_lauren$j<-as.logical(week3_dataset_lauren$j)
week3_dataset_lauren$i<-as.logical(week3_dataset_lauren$i)
week3_dataset_lauren$k<-as.factor(week3_dataset_lauren$k)

ggplot(data=week3_dataset_lauren)+geom_point()+aes(x=x, y=y, color=i, shape=j, size=k)

week3_dataset_lauren$k.tmp<-week3_dataset_lauren$k
week3_dataset_lauren$k.tmp<-factor(week3_dataset_lauren$k, levels=c(0,1,2,3), labels=c("none", "some", "lots", "all"))

ggplot(data=week3_dataset_lauren)+geom_point()+aes(x=x, y=y, color=i, shape=j, size=k.tmp)

week3_dataset_lauren$i[!week3_dataset_lauren$i]<-NA
week3_dataset_lauren$i[is.na(week3_dataset_lauren$i)]<-FALSE

table(week3_dataset_lauren$j)
table(week3_dataset_lauren$i)
table(week3_dataset_lauren$k)
