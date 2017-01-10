load("~/Desktop/OneDrive/Winter 2017/Statistics/com521-lauren/week2_dataset-lauren copy.RData")

length(week2.dataset)

summary(week2.dataset)
IQR(week2.dataset)
var(week2.dataset)
sd(week2.dataset)

my.mean<- function(week2.dataset) {sum(week2.dataset)/length(week2.dataset)}
my.mean(week2.dataset)

my.median<-function(week2.dataset) {sort(week2.dataset)[length(week2.dataset)/2]}
my.median(week2.dataset)
sort(round(week2.dataset))
rounded mode: 21

boxplot(week2.dataset)
hist(week2.dataset)

week2.dataset[week2.dataset<0]<-NA
summary(week2.dataset, na.rm=TRUE)
mean(week2.dataset, na.rm=TRUE)
sd(week2.dataset, na.rm=TRUE)

log.w2<-log(week2.dataset)
summary(log.w2, na.rm=TRUE)
sd(log.w2, na.rm=TRUE)
boxplot(log.w2, na.rm=TRUE)
hist(log.w2, na.rm=TRUE)

