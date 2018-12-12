---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---



## Loading and preprocessing the data

The following libraries are included for the assignment

```r
library(dplyr)
library(timeDate)
library(ggplot2)
```

The following piece of code loads the data from the csv file and does some preprocessing

```r
activityMonitoringDS <- read.csv("activity.csv")
activityMonitoringDS$date <- as.Date(activityMonitoringDS$date, format = "%Y-%m-%d")
```


## What is mean total number of steps taken per day?

The following piece of code displays the total number of steps taken per day

```r
totalStepsbyDate <- activityMonitoringDS %>% group_by(date) %>% summarise(totalsteps = sum(steps, na.rm=TRUE))
totalStepsbyDate
```

```
## # A tibble: 61 x 2
##    date       totalsteps
##    <date>          <int>
##  1 2012-10-01          0
##  2 2012-10-02        126
##  3 2012-10-03      11352
##  4 2012-10-04      12116
##  5 2012-10-05      13294
##  6 2012-10-06      15420
##  7 2012-10-07      11015
##  8 2012-10-08          0
##  9 2012-10-09      12811
## 10 2012-10-10       9900
## # ... with 51 more rows
```

The histogram of the total number of steps taken each day is as follows

```r
hist(totalStepsbyDate$totalsteps, main = "Histogram of total steps per day", breaks = 25, xlab="Total steps per day", ylab="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The following code chunk calculates the mean of the total number of steps taken per day 

```r
mean(totalStepsbyDate$totalsteps, na.rm = TRUE)
```

```
## [1] 9354.23
```

The following code chunk calculates the median of the total number of steps taken per day

```r
median(totalStepsbyDate$totalsteps, na.rm = TRUE)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

The following code chunk creates a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
averageStepsbyInterval <- activityMonitoringDS %>% group_by(interval) %>% summarise(meanofsteps = mean(steps, na.rm = TRUE))
with(averageStepsbyInterval, plot(interval, meanofsteps, type = "l", xlab="5-minute interval", ylab="average number of steps taken", main="average daily activity pattern"))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

The following code chunk displays Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps

```r
averageStepsbyInterval[which.max(averageStepsbyInterval$meanofsteps),]
```

```
## # A tibble: 1 x 2
##   interval meanofsteps
##      <int>       <dbl>
## 1      835        206.
```


## Imputing missing values

The following code chunk displays the total number of missing values in the dataset

```r
sum(is.na(activityMonitoringDS$steps))
```

```
## [1] 2304
```

The following code chunk imputes the missing data and then plots a histogram of the total number of steps taken each day

```r
activityMonitoring_No_NA <- activityMonitoringDS[which(!is.na(activityMonitoringDS$steps)),]
activityMonitoring_With_NA <- activityMonitoringDS[which(is.na(activityMonitoringDS$steps)),]
meanStepsbyInterval_No_NA <- activityMonitoring_No_NA %>% group_by(interval) %>% summarise(averagesteps = mean(steps))

activityMonitoring_With_NA$steps <- ifelse(meanStepsbyInterval_No_NA$interval==activityMonitoring_With_NA$interval, meanStepsbyInterval_No_NA$averagesteps)

activityMonitoringImpute <- rbind(activityMonitoring_No_NA, activityMonitoring_With_NA)

totalStepsbyDate_AfterImpute <- activityMonitoringImpute %>% group_by(date) %>% summarise(totalsteps = sum(steps))

hist(totalStepsbyDate_AfterImpute$totalsteps, main = "Histogram of total steps per day", breaks = 25, xlab="Total steps per day", ylab="Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

The following code chunk calculates the mean of the total number of steps taken per day after imputing the dataset

```r
mean(totalStepsbyDate_AfterImpute$totalsteps)
```

```
## [1] 10766.19
```

The following code chunk calculates the median of the total number of steps taken per day after imputing the dataset

```r
median(totalStepsbyDate_AfterImpute$totalsteps)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

The following code creates a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

```r
activityMonitoringImpute <- activityMonitoringImpute %>% mutate(weekdayflag = ifelse(isWeekday(date)==TRUE, "Weekday", "weekend"))
```

The following code chunk creates a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
activityMonitoringImpute_Mean <- activityMonitoringImpute %>% group_by(interval,weekdayflag) %>% summarise(averagesteps = mean(steps))
qplot(interval, averagesteps, data=activityMonitoringImpute_Mean, geom="line", facets = weekdayflag~., xlab="Interval", ylab = "average steps", main="Average steps pattern by weekdays and weekends")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
