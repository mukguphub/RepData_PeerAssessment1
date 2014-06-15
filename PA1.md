# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
1. Load the data (i.e. read.csv())
2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day? 
For this part of the assignment, you can ignore the missing values in the dataset.

- Make a histogram of the total number of steps taken each day

```r
  totalSteps  <- aggregate(steps ~ date, data = data, sum, na.rm = TRUE)
  hist(totalSteps$steps)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

- Calculate and report the mean and median total number of steps taken per day

```r
  stepsMean <- mean(totalSteps$steps)
  stepsMedian <- median(totalSteps$steps) 
```
- The **mean** total number of steps taken per day is 1.0766 &times; 10<sup>4</sup> steps.
- The **median** total number of steps taken per day is 10765 steps.

## What is the average daily activity pattern?
- Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
stepsPerInterval <- aggregate( steps ~ interval,data=data, mean,na.rm = TRUE)
plot(x=stepsPerInterval[,1],y=stepsPerInterval[,2],type="l",main ="Average Number of Steps",
       xlab = "Interval",ylab="Average")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 
- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
  MaxNumberofSteps <- stepsPerInterval[which.max(stepsPerInterval$steps), ]$interval
```
It is the **835th** interval.


## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?