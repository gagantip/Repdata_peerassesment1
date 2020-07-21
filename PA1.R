activity <- read.csv("activity.csv")
activity$date <- as.Date(as.character(activity$date))
head(activity)
summary(activity)
pairs(activity)

stepsperday<- aggregate(steps~date, activity,sum, na.rm=T)
hist(stepsperday$steps, xlab="No. of steps taken per day", main="Histogram of steps taken per day")
meansteps <- mean(stepsperday$steps)
meansteps

act2 <- aggregate(steps~interval, activity, mean, na.rm=T)
plot(steps~interval, act2, type="l")
maxinterval <- act2[which.max(act2$steps),]$interval
maxinterval

totalmissing <- sum(is.na(activity))
totalmissing

impute_val <- function(interval) {
  act2[act2$interval==interval,]$steps
}

act_noNA <- activity
for(i in 1:nrow(act_noNA)) {
  if(is.na(act_noNA[i,]$steps)) {
    act_noNA[i,]$steps <- impute_val(act_noNA[i,]$interval)
  }
}

totalsteps_noNA <- aggregate(steps~date, act_noNA, sum)
hist(totalsteps_noNA$steps)

act_noNA$date <- as.Date(strptime(act_noNA$date,forma="%Y-%m-%d"))
act_noNA$day <- weekdays(act_noNA$date)
for(i in 1:nrow(act_noNA)) {
    if(act_noNA[i,]$day=="Saturday"|act_noNA[i,]$day=="Sunday") {
      act_noNA[i,]$day <- "Weekend"
    }
    else {
      act_noNA[i,]$day <- "Weekday"
    }
}

act3 <- split(act_noNA, as.factor(act_noNA$day))
library(lattice)
xyplot(steps~interval|day, act_noNA, layout=c(1,2),type="l",xlab="Interval", ylab="No. of steps")
