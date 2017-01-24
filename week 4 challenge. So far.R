#PC1
library(readr)
COS_Statistics_Mobile_Sessions <- read_csv("~/Desktop/OneDrive/Winter 2017/Statistics/com521-lauren/COS-Statistics-Mobile_Sessions.csv")
View(COS_Statistics_Mobile_Sessions)

library(readr)
COS_Statistics_Top5000_Pages <- read_csv("~/Desktop/OneDrive/Winter 2017/Statistics/com521-lauren/COS-Statistics-Top5000-Pages.csv")
View(COS_Statistics_Top5000_Pages)

Top5000<-data.frame(COS_Statistics_Top5000_Pages)
> Mobile<-data.frame(COS_Statistics_Mobile_Sessions)

#PC2
head(Mobile)
dim(Mobile)
summary(Mobile)
#It seems the only data classified as integers are sessions, new_users, and PagesPerSession.
#They also seem quite skewed. (maybe not Pages...). Let's see.

class(Mobile$New_Sessions)
class(Mobile$New_Users)
class(Mobile$AvgSessionDuration)
class(Mobile$PagesPerSession)
class(Mobile$Month)

hist(Mobile$Sessions)
hist(Mobile$New_Users)
hist(Mobile$PagesPerSession)

Mobile[sample(seq(1, nrow(Mobile)), 5),]
Mobile[sample(seq(1, nrow(Mobile)), 5),]
Mobile[sample(seq(1, nrow(Mobile)), 5),]

#Summary tells me how many NAs in the variables. How do I look at those rows?

head(Top5000)
summary(Top5000)
dim(Top5000)
#looks like Pageviews, UniquePageviews, and Entrances are numeric or integers
#they are all quite skewed, looking at mean and median differences.

Top5000[sample(seq(1, nrow(Top5000)), 5),]
Top5000[sample(seq(1, nrow(Top5000)), 5),]
Top5000[sample(seq(1, nrow(Top5000)), 5),]


#PC3
MonthViews5000<-data.frame(Top5000$Month, Top5000$Pageviews)
head(MonthViews5000)
monthlyviews<-tapply(Top5000$Pageviews, Top5000$Month, sum)
MonthViews5000<-data.frame(month=names(monthlyviews), monthlyviews)

as.POSIXct(as.character(Top5000$Month), format="%m/%d/%y %H:%M:%S %p", tz="UTC")
#Returns NA for everything. Can't figure out why.

#PC4
library(plyr)
count(Mobile$Operating_System)
byOS<-tapply(Mobile$Sessions, Mobile$Operating_System, sum)
OSTotals<-data.frame(Platform=names(byOS), byOS)
#Oops, that's a dataframe for platforms and totals by platform

mobile.os.monthly.views<-Mobile$Sessions*Mobile$PagesPerSession
totalsbymonth.mobile<-tapply(mobile.os.monthly.views, Mobile$Month, sum)
MonthSessionsMobile<-data.frame(month=names(totalsbymonth.mobile), totalsbymonth.mobile)


#PC5
merge(x = Mobile, y = Top5000, by = "Month", all.x=TRUE)
#That merged everything, I think. I want to just merge a few columns

merge(x = MonthSessionsMobile, y = MonthViews5000, by = "month", all.x=TRUE, all.y=TRUE)
views<-merge(x = MonthSessionsMobile, y = MonthViews5000, by = "month", all.x=TRUE, all.y=TRUE)


#PC6
views$pct<-views$totalsbymonth.mobile/views$monthlyviews

views$month<-as.Date(views$month, format="%m/%d/%Y %H:%M:%S %p", tz="UTC")
class(views$month)

#PC7
ggplot(data=views)+aes(x=month, y=pct)+geom_point()+scale_y_continuous(limits=c(0,1))
