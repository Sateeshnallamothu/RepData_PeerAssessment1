library(lattice)
library(ggplot2)
library(data.table)
library(dplyr)

# read input data
setwd('RepData_PeerAssessment1')
activity <- read.csv(file='activity.csv',header = TRUE)
str(activity)
head(activity)
# check for NA in date and interval
mean(is.na(activity$date))
mean(is.na(activity$interval))
# convert data to ccyy-mm-dd
activity$date <- as.Date(activity$date,"%Y-%m-%d")

##What is mean total number of steps taken per day?  (ignore 'NA')
## 1. Make a histogram of the total number of steps taken each day
ttl.steps.per.day<-na.omit(activity)%>% 
                   group_by(date) %>%
                   summarize(ttl.steps = sum(steps,na.rm=TRUE))
head(ttl.steps.per.day)
step.range<-range(ttl.steps.per.day$ttl.steps)
hist(ttl.steps.per.day$ttl.steps,main="Histogram - Total number of steps per day", 
     xlab="Total number of steps in a day",col="red",ylim=c(0,30),
     xlim=step.range)
dev.copy(png,file="figure/histstepsplot.png")
dev.off()

dt.range<-range(ttl.steps.per.day$date)
ggplot(ttl.steps.per.day,aes(x=date, y=(ttl.steps)))+geom_histogram(stat="identity",binwidth = 0.1) +
  xlab("Date of measurement")+ylab("Number of steps")+
  scale_x_date( date_labels = "%Y-%m-%d",date_breaks = "1 day", 
                breaks=seq(min(ttl.steps.per.day$date), max(ttl.steps.per.day$date), 60),
                limits = dt.range)+
  theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.copy(png,file="figure/stepsbyintplot2.png")
dev.off()

# 2. Calculate and report the mean and median total number of steps taken per day
mu.steps<-mean(ttl.steps.per.day$ttl.steps)
md.steps<-median(ttl.steps.per.day$ttl.steps)

# 1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
avg.steps.per.interval<-na.omit(activity)%>% 
  group_by(interval) %>%
  summarize(avg.steps = mean(steps,na.rm=TRUE))  
#%>%
# mutate(interval=format(strptime(sprintf("%04d", interval), format="%H%M"), format = "%H:%M"))
avg.steps.per.interval
 
plot(avg.steps.per.interval, type="l",main="Avg. number of steps in 5 minute interval",
     xlab="Inteval in 5 mintues(hhmm)",ylab="Avg. steps",col="red",xaxt = "n")
axis(1, avg.steps.per.interval$interval, cex.axis = .7)
dev.copy(png,file="figure/stepsbyintplot.png")
dev.off()

# 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max.steps<-avg.steps.per.interval[which.max(avg.steps.per.interval$avg.steps),]
max.steps$interval
max.steps$avg.steps
#Imputing missing values

#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
na.ttl<-sum(!complete.cases(activity))
na.ttl
#2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
ttl.activities <- nrow(activity)
activity2 <- activity
for (i in 1:ttl.activities) {
  if (is.na(activity2[i,1])) {
    activity2[i,1] <- subset(avg.steps.per.interval,interval==activity2[i,3])$avg.steps
  }
}
 
#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
head(activity2)

#4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
#   Do these values differ from the estimates from the first part of the assignment? 
#   What is the impact of imputing missing data on the estimates of the total daily number of steps?
ttl.steps.per.day2<-activity2%>% 
  group_by(date) %>%
  summarize(ttl.steps = sum(steps))
head(ttl.steps.per.day2)
step.range2<-range(ttl.steps.per.day2$ttl.steps)
hist(ttl.steps.per.day2$ttl.steps,main="Histogram - Total number of steps per day with imputed data", 
     xlab="Total number of steps in a day",col="red",ylim=c(0,30),
     xlim=step.range2)
dev.copy(png,file="figure/histstepsbyint2.png")
dev.off()

mu.steps2<-mean(ttl.steps.per.day2$ttl.steps)
md.steps2<-median(ttl.steps.per.day2$ttl.steps)
mu.steps2
md.steps2

##Are there differences in activity patterns between weekdays and weekends?
#     For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.
# 1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
activity2$weekday <- ifelse(weekdays(activity2$date) %in% c('Saturday','Sunday'),'Weekday','weekend')
activity2$weekday <- as.factor(activity2$weekday)
avg.steps.per.interval.by.weekday <- activity2 %>% group_by(weekday,interval) %>%
                                     summarize(avg.steps=mean(steps))
head(avg.steps.per.interval.by.weekday)
# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
#    The plot should look something like the following, which was created using simulated data:
g<-ggplot(avg.steps.per.interval.by.weekday,aes(x=interval,y=avg.steps))+geom_line(color="red")
g<-g+facet_grid(weekday~.)
g<-g+xlab("Interval") + ylab("Avg. steps taken")+ggtitle("Avg. steps taken by weekday") 
g
dev.copy(png,file="figure/weekdayplot.png")
dev.off()

## remove datasets 
rm(avg.steps.per.interval.by.weekday,avg.steps.per.interval,activity2,activity,ttl.steps.per.day2,ttl.steps.per.day)

