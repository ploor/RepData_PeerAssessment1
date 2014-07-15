# Reproducible Research: Peer Assessment 1

## Preliminary R scripting

Before starting the analysis, run the following:


```r
Sys.setlocale("LC_TIME", "English") # to use English dates
library(ggplot2)
library(lattice)
```

The following function will be used in a few places to convert the interval variable into an [interval scale](http://en.wikipedia.org/wiki/Level_of_measurement#Interval_scale).


```r
intervalToNum <- function(interval){
  floor(interval/100)*12 + (interval %% 100)/5 + 1
}
```

Some example values:

```r
intervalToNum(c(0,5,10,55,100,105,2350,2355))
```

```
## [1]   1   2   3  12  13  14 287 288
```


## Loading and preprocessing the data

### 1. Load the data (i.e. read.csv())


```r
unzip("activity.zip")
data <- read.csv("activity.csv")
```

### 2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
data$date <- as.Date(data$date, format="%Y-%m-%d")
data$intervalNum<-intervalToNum(data$interval)
```


## What is mean total number of steps taken per day?

For this part of the assignment, we can ignore the missing values in the dataset.

### 1. Make a histogram of the total number of steps taken each day


```r
data.noMiss <- data[!is.na(data$steps),]
data.miss <- data[is.na(data$steps),] # handy later

steps.date <- aggregate(steps ~ date
                       ,data=data.noMiss
                       ,FUN=sum)

ggplot(steps.date, aes(x=steps) ) +
  geom_histogram(binwidth=500) +
  xlab("steps in a day")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

### 2. Calculate and report the mean and median total number of steps taken per day


```r
 mean.steps <-  mean(steps.date$steps)
median.steps<-median(steps.date$steps)
```

The **mean** steps per day is 1.0766 &times; 10<sup>4</sup>.  The **median** steps per day is 10765.


## What is the average daily activity pattern?

### 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps.intervalNum <- aggregate(steps ~ intervalNum
                              ,data=data
                              ,FUN=mean)

xyplot(steps~intervalNum
      ,data=steps.intervalNum
      ,type="l"
      ,xlab="5-minute interval number"
      ,ylab="number of steps"
      )
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8.png) 

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
# just so we can output in the original format
steps.interval <- aggregate(steps ~ interval
                           ,data=data
                           ,FUN=mean)

steps.interval.max <- steps.interval$interval[
  which(steps.interval$steps==max(steps.interval$steps))
  ]

steps.intervalNum.max <- steps.intervalNum$interval[
  which(steps.intervalNum$steps==max(steps.intervalNum$steps))
  ]
```
On average, the **max steps occur at interval 835**, or, in other words, 5-min. interval number 104 of the day.

## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
numMissing <- nrow(data) - nrow(data.noMiss)
```

There are **2304 missing values** in the dataset.


### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

The strategy is to fill the NAs with the mean for an interval, which was already computed above.  Here is the dataset we'll merge with to accomplish this: 


```r
mergeData <- data.frame(interval = steps.interval$interval
                       ,meanSteps = steps.interval$steps )
```


### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data.imputed <- merge(data,mergeData)
data.imputed<- transform(data.imputed
                        ,steps=ifelse(is.na(steps)
                                     ,meanSteps
                                     ,steps)
                        )
```

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
imputed_steps.date <- aggregate(steps ~ date
                               ,data=data.imputed
                               ,FUN=sum)

imputed_mean.steps<-mean(imputed_steps.date$steps)
imputed_median.steps<-median(imputed_steps.date$steps)
```

For the imputed data, the mean steps per day is 1.0766 &times; 10<sup>4</sup>.  The median steps per day is 1.0766 &times; 10<sup>4</sup>.

This mean steps per day, after imputation, is the exact same.  Why?  Observe the following:

```r
          numDays <-length(unique(data$date))
  numDaysNoMissing<-length(unique(data.noMiss$date))
numDaysWithMissing<-length(unique(data.miss$date))
c(numDays,numDaysNoMissing,numDaysWithMissing)
```

```
## [1] 61 53  8
```
Which shows that for any day containing missing values for steps, all the steps for that day are missing.

The median has shifted higher, due to the mean being higher than the median.  And now it also happens to equal the mean.

And now for the histogram:

```r
ggplot(imputed_steps.date, aes(x=steps) ) +
  geom_histogram(binwidth=500) +
  xlab("steps in a day")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15.png) 

## Are there differences in activity patterns between weekdays and weekends?

### 1. Create a new factor variable in the dataset with two levels ¡V ¡§weekday¡¨ and ¡§weekend¡¨ indicating whether a given date is a weekday or weekend day.


```r
data.imputed<- transform(data.imputed
                        ,weekday=ifelse(weekdays(date,abbreviate=T) 
                                           %in% c("Sat","Sun")
                                       ,"weekend"
                                       ,"weekday")
                        )
data.imputed$weekday<-as.factor(data.imputed$weekday)
```

### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
steps.intervalNum.day <- aggregate(steps ~ intervalNum+weekday
                                  ,data=data.imputed
                                  ,FUN=mean)
xyplot(steps~intervalNum|weekday
      ,data=steps.intervalNum.day
      ,type="l"
      ,xlab="5-minute interval number"
      ,ylab="number of steps"
      ,layout=c(1,2))
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17.png) 
