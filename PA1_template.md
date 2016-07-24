---
title: "ReproducibleResearchProject1"
author: "BN"
date: "22 July 2016"
output: html_document
---

#**Loading and preprocessing the data**

###Loaded the data 

```r
temp<-tempfile()
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
data<-read.csv(unz(temp,"activity.csv"))
```

###Process/transform the data:

```r
data$date<-as.Date(data$date)
data1<-na.omit(data)
```

#**What is mean total number of steps taken per day?**

###Calculating mean total number of steps taken per day

```r
stepsperday <- aggregate(steps ~ date, data1, sum)
```

###Making a histogram of the total number of steps taken each day

```r
hist(stepsperday$steps, main = paste("Histogram of the total number of steps taken each day"), col=3, xlab="Number of Steps", ylab="Count")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

###Calculating the mean and median of the total number of steps taken per day

```r
stepsmean <- mean(stepsperday$steps)
stepsmedian <- median(stepsperday$steps)
```
##*The mean of the total number of steps taken per day is 1.0766189 &times; 10<sup>4</sup>*
##*The median of the total number of steps taken per day is 10765*

#**What is the average daily activity pattern?**

###Making a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsperinterval <- aggregate(steps ~ interval, data1, mean)
plot(stepsperinterval$interval,stepsperinterval$steps, type="l", xlab="Interval", ylab="Average Number of Steps Taken",main="Average Number of Steps per Day by Interval")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)
###Calculating the 5-minute interval, on average across all the days in the dataset, which contains the maximum number of steps

```r
maxstepsinterval <- stepsperinterval[which.max(stepsperinterval$steps),1]
```

##*The maximum number of steps is 835 

#**Imputing missing values**

###Calculating the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
NoNA<-sum(is.na(data$steps))
```
##*The total number of missing values in the dataset is 2304*

###Devising a strategy for filling in all of the missing values in the dataset- we substitute them by mean. Created a new dataset that is equal to the original dataset but with the missing data filled in

```r
dataimputed<-data
for(i in 1:nrow(dataimputed)){if(is.na(dataimputed$steps[i])){dataimputed$steps[i]<-0}} 
```

###Making a histogram of the total number of steps taken each day

```r
stepsperday1 <- aggregate(steps ~ date, dataimputed, sum)
hist(stepsperday1$steps, main = paste("Histogram of the total number of steps taken each day"), col=3, xlab="Number of Steps", ylab="Count")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

###Calculating the mean and median total number of steps taken per day

```r
stepsmean1 <- mean(stepsperday1$steps)
stepsmedian1 <- median(stepsperday1$steps)
diffmean<-stepsmean-stepsmean1
diffmedian<-stepsmedian-stepsmedian1
```

##*The mean of the total number of steps taken per day is 9354.2295082*
##*The difference of means is 1411.959171*
##*The median of the total number of steps taken per day is 1.0395 &times; 10<sup>4</sup>*
##*The difference of medians is 370*

#**Are there differences in activity patterns between weekdays and weekends?**

###Creating a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
dataimputed$dow = as.factor(ifelse(is.element(weekdays(as.Date(dataimputed$date)),weekdays), "Weekday", "Weekend"))
```

###Making a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
steps_by_interval_i <- aggregate(steps ~ interval + dow, dataimputed, mean)
library(lattice)
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)
