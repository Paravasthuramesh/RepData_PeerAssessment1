# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

Show any code that is needed to

  1. Load the data (i.e. read.csv())
 
  2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
setwd("./")

library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.3
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
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.2.3
```

```r
library(tidyr)
```

```
## Warning: package 'tidyr' was built under R version 3.2.3
```

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.2.3
```

```r
activity_data<-read.csv("activity.csv" )
activity_data_noNAs<-read.csv("activity.csv" )
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

  1. Make a histogram of the total number of steps taken each day
  
  2. Calculate and report the mean and median total number of steps taken
     per day


```r
activity_data <- transform(activity_data, date = factor(date))
activity_data<-group_by(activity_data,date)
sum_steps_day <- summarise(activity_data, steps = sum(steps))
sum_steps_day <-na.omit(sum_steps_day)

hist(sum_steps_day$steps, main = "Total number of steps per day", xlab = "Steps per day", ylab = "Frequency" ,col="blue")    
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

#### Mean and median number of steps taken each day


```r
options(scipen=999)
mean_steps<-mean(sum_steps_day$steps, na.rm = TRUE)
print(paste("Mean:", mean_steps), sep = " ")
```

```
## [1] "Mean: 10766.1886792453"
```

```r
median_steps<-median(sum_steps_day$steps, na.rm = TRUE)
print(paste("Median:", median_steps), sep = " ")
```

```
## [1] "Median: 10765"
```


## What is the average daily activity pattern?

  1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
     and the average number of steps taken, averaged across all days (y-axis)

  2. Which 5-minute interval, on average across all the days in the dataset,
     contains the maximum number of steps?
  



```r
activity_data <- transform(activity_data, interval = factor(interval))
activity_data<-group_by(activity_data,interval)
mean_steps_interval <- summarise(activity_data, steps = mean(steps,na.rm=TRUE))

plot(levels(as.factor(mean_steps_interval$interval)), mean_steps_interval$steps, 
     type="l", col="blue", lwd=3, 
     main="Daily activity pattern", 
     xlab="Interval (hhmm)", ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
```

#### Which 5-minute interval, on average across all the days in the dataset contains the maximum number of steps?



```r
max_steps<-mean_steps_interval[match(max(mean_steps_interval$steps),mean_steps_interval$steps),]
```

Interval 835 contains the maximum steps. On average across all the days - 206 steps are taken. 



## Imputing missing values
Note that there are a number of days/intervals where there are missing values
(coded as NA). The presence of missing days may introduce bias into some
calculations or summaries of the data.

  1. Calculate and report the total number of missing values in the dataset
     (i.e. the total number of rows with NAs)
  
  2. Devise a strategy for filling in all of the missing values in the dataset. The
     strategy does not need to be sophisticated. For example, you could use
     the mean/median for that day, or the mean for that 5-minute interval, etc.
  
  3. Create a new dataset that is equal to the original dataset but with the
     missing data filled in.

  4. Make a histogram of the total number of steps taken each day and Calculate
     and report the mean and median total number of steps taken per day. Do
     these values differ from the estimates from the first part of the assignment?

    What is the impact of imputing missing data on the estimates of the total
    daily number of steps?



```r
number_na<-sum(is.na(activity_data))
```

###Total number of missing values in the dataset((i.e. the total number of rows with NAs) 2304.

###Above calculated averages (steps per 5-min interval) are used to fill NAs.


```r
class(activity_data_noNAs$interval)<-"numeric"
i<-1
for (i in 1:dim(activity_data_noNAs)[1]){
        if (is.na(activity_data_noNAs[i,1])){
                r<-match(activity_data_noNAs[i,3],mean_steps_interval$interval)
                activity_data_noNAs[i,1]<-mean_steps_interval[r,2]
                }
        i=i+1
}       
sum_steps_day_noNAs <- tapply(activity_data_noNAs$steps, activity_data_noNAs$date, sum, na.rm = TRUE)
hist(sum_steps_day_noNAs, main = "Total number of steps per day", 
    xlab = "Steps per day", ylab = "Frequency"
    ,col="red")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

#### Mean and median number of steps taken each day (no NAs) 


```r
mean_noNAs<-mean(sum_steps_day_noNAs)
print(paste("Mean:", mean_noNAs), sep = " ")
```

```
## [1] "Mean: 10766.1886792453"
```

```r
median_noNAs<-median(sum_steps_day_noNAs)
print(paste("Median:", median_noNAs), sep = " ")
```

```
## [1] "Median: 10766.1886792453"
```
The mean of steps taken per day is 10766 the median of taken steps is 10766. There is not much change to Mean and Median value with the previous one. This may be due to substituing mean values to NAs which appearing for full days.Since these values are substituted by mean values nothing changes during mean and median calculation.


## Are there differences in activity patterns between weekdays and weekends?

  1. Create a new factor variable in the dataset with two levels - "weekday"
     and "weekend" indicating whether a given date is a weekday or weekend
     day.

  2. Make a panel plot containing a time series plot (i.e. type = "l") of the
     5-minute interval (x-axis) and the average number of steps taken, averaged
     across all weekday days or weekend days (y-axis). The plot should look
     something like the following, which was creating using simulated data:


```r
activity_data_noNAs<-mutate(activity_data_noNAs, date_day=wday(date))
activity_data_weekday<-subset(activity_data_noNAs,date_day>1 & date_day<7)
activity_data_weekday <- transform(activity_data_weekday, interval = factor(interval))
activity_data_weekday<-group_by(activity_data_weekday,interval)
mean_steps_interval_weekday <- summarise(activity_data_weekday, steps = mean(steps,na.rm=TRUE))

activity_data_weekend<-subset(activity_data_noNAs,date_day==1 | date_day==7)
activity_data_weekend <- transform(activity_data_weekend, interval = factor(interval))
activity_data_weekend<-group_by(activity_data_weekend,interval)
mean_steps_interval_weekend <- summarise(activity_data_weekend, steps = mean(steps,na.rm=TRUE))


par(mfrow = c(2, 1))
plot(levels(as.factor(mean_steps_interval_weekday$interval)), mean_steps_interval_weekday$steps, 
     type="l", col="blue", lwd=3, ylim=c(0,250),
     main="Daily activity pattern on weekdays", 
     xlab="Interval (hhmm)", ylab="Average number of steps")

plot(levels(as.factor(mean_steps_interval_weekend$interval)), mean_steps_interval_weekend$steps, 
     type="l", col="red", lwd=3, ylim=c(0,250),
     main="Daily activity pattern at weekend",
     xlab="Interval (hhmm)", ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Yes, there are differences between weekdays and weekends. During weekdays the activities are high and peaked in morning time where during the weekends the morning activities are high but not to the level of weekdays and also it is distributed more throught out the daytime.
