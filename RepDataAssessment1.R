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


ggplot(ttl.steps.per.day,aes(x=date, y=(ttl.steps)))+geom_bar(stat="identity") +
  xlab("Date of measurement")+ylab("Number of steps")+
  scale_x_date( date_labels = "%Y-%m-%d",date_breaks = "1 day", 
            breaks=seq(min(ttl.steps.per.day$date), max(ttl.steps.per.day$date), 60),
               limits = dt.range)+
  theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggplot(ttl.steps.per.day,aes(x=date, y=(ttl.steps)))+geom_histogram(stat="identity",binwidth = 0.1) +
  xlab("Date of measurement")+ylab("Number of steps")+
  scale_x_date( date_labels = "%Y-%m-%d",date_breaks = "1 day", 
                breaks=seq(min(ttl.steps.per.day$date), max(ttl.steps.per.day$date), 60),
                limits = dt.range)+
  theme_bw()+theme(axis.text.x = element_text(angle = 90, hjust = 1))

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

# 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max.steps<-avg.steps.per.interval[which.max(avg.steps.per.interval$avg.steps),]
max.steps$interval
max.steps$avg.steps
