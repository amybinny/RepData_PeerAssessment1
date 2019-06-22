---
title: "Reproducible Research: Peer Assessment 1"
author: "Amy"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

1. Total numer of steps taken each day.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activity_na <- activity[complete.cases(activity$steps),]
activity_nagroupdate<- group_by(activity_na, date)
activity_nasum <- summarise(activity_nagroupdate, steps_sum=sum(steps))
activity_nasum
```

```
## # A tibble: 53 x 2
##    date       steps_sum
##    <fct>          <int>
##  1 2012-10-02       126
##  2 2012-10-03     11352
##  3 2012-10-04     12116
##  4 2012-10-05     13294
##  5 2012-10-06     15420
##  6 2012-10-07     11015
##  7 2012-10-09     12811
##  8 2012-10-10      9900
##  9 2012-10-11     10304
## 10 2012-10-12     17382
## # ... with 43 more rows
```

2. Histogram of total number of steps taken each day

```r
hist(activity_nasum$steps_sum, main="Histogram of steps taken each day", xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

3. Mean of total number of steps taken per day is given by result of the following code.


```r
mean(activity_nasum$steps_sum)
```

```
## [1] 10766.19
```

Median of total number of steps taken per day is given by result of the following code.


```r
median(activity_nasum$steps_sum)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
library(dplyr)
activity_na <- activity[complete.cases(activity$steps),]
activity_nagroupint<- group_by(activity_na, interval)
activity_naavg <- summarise(activity_nagroupint, steps_avg=mean(steps))
activity_naavg
```

```
## # A tibble: 288 x 2
##    interval steps_avg
##       <int>     <dbl>
##  1        0    1.72  
##  2        5    0.340 
##  3       10    0.132 
##  4       15    0.151 
##  5       20    0.0755
##  6       25    2.09  
##  7       30    0.528 
##  8       35    0.868 
##  9       40    0     
## 10       45    1.47  
## # ... with 278 more rows
```

1. Time series plot of average number of steps taken

```r
plot(activity_naavg$interval, activity_naavg$steps_avg, main="Average total steps across 5-minute interval ", type="l", xlab = "Interval", ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. The time series, that has the maximum average number of steps

```r
max <- max(activity_naavg$steps_avg)
subset(activity_naavg, steps_avg==max)[["interval"]]
```

```
## [1] 835
```
## Imputing missing values

1. The total number of missing values in the file:

```r
sum(!complete.cases(activity$steps))
```

```
## [1] 2304
```

2&3. A new dataset is created with missing values filled in with mean of 5-minute interval

```r
activity_im <- activity
activity_im$steps[is.na(activity$steps)] <- mean(activity_na$steps, na.rm=TRUE)
```

4. Total number of steps taken each day after missing values are imputed:


```r
activity_groupdate<- group_by(activity_im, date)
activity_sum <- summarise(activity_groupdate, steps_sum=sum(steps))
activity_sum
```

```
## # A tibble: 61 x 2
##    date       steps_sum
##    <fct>          <dbl>
##  1 2012-10-01    10766.
##  2 2012-10-02      126 
##  3 2012-10-03    11352 
##  4 2012-10-04    12116 
##  5 2012-10-05    13294 
##  6 2012-10-06    15420 
##  7 2012-10-07    11015 
##  8 2012-10-08    10766.
##  9 2012-10-09    12811 
## 10 2012-10-10     9900 
## # ... with 51 more rows
```

Histogram of total number of steps taken each day after missing values are imputed:


```r
hist(activity_sum$steps_sum, main="Histogram of steps taken each day after missing values are imputed", xlab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

Mean of total number of steps taken per day after missing values are imputed:


```r
mean(activity_sum$steps_sum)
```

```
## [1] 10766.19
```

Median of total number of steps taken per day after missing values are imputed:


```r
median(activity_sum$steps_sum)
```

```
## [1] 10766.19
```

The median of total steps taken per day has changed after imputing the missing values. If we notice the histogram plot we can also notice that the y limit has increased when missing values were imputed.

## Are there differences in activity patterns between weekdays and weekends?


```r
activity_im$date <- as.Date(activity_im$date)
activity_imweek <- mutate(activity_im, day=weekdays(activity_im$date))
sub_wday<- subset(activity_imweek, day=="Monday" | day=="Tuesday" | day=="Wednesday" |day == "Thursday" | day == "Friday" ) 
sub_day <- mutate(sub_wday, weekday_weekend= "weekday")
sub_wend<- subset(activity_imweek, day=="Saturday" | day=="Sunday" )
sub_end <- mutate(sub_wend, weekday_weekend= "weekend")
activity_day <- rbind(sub_day,sub_end)
sub_end_int <- group_by(sub_end, interval)
sub_day_int <- group_by(sub_day, interval)
sub_day_avg <- summarise(sub_day_int, avg=mean(steps))
sub_end_avg <- summarise(sub_end_int, avg=mean(steps))
par(mfrow=c(1,2))
plot(sub_day_avg$interval, sub_day_avg$avg, main="Weekday steps average", type="l", xlab = "Interval", ylab = "Steps")
plot(sub_end_avg$interval, sub_end_avg$avg, main="Weekend steps average", type="l", xlab = "Interval", ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
