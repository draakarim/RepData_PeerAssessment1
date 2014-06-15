# Reproducible Research: Peer Assessment 1
========================================================

## Loading and preprocessing the data

```r
activity <- read.csv("~/Desktop/Working Directory/activity.csv")
```

## What is mean total number of steps taken per day?

```r
Steps.taken <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(Steps.taken$steps, names.arg = Steps.taken$date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean(Steps.taken$steps)
```

```
## [1] 10766
```

```r
median(Steps.taken$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
average.activity <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(average.activity, type = "l")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
average.activity$interval[which.max(average.activity$steps)]
```

```
## [1] 835
```

## Imputing missing values

```r
sum(is.na(activity))
```

```
## [1] 2304
```

```r
activity <- merge(activity, average.activity, by = "interval", suffixes = c("", ".y"))
                              
act <- is.na(activity$steps)
activity$steps[act] <- activity$steps.y[act]
activity <- activity[, c(1:3)]

step.date <- aggregate(steps ~ date, data = activity, FUN = sum)
barplot(step.date$steps, names.arg = step.date$date, xlab = "date", ylab = "steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
mean(step.date$steps)
```

```
## [1] 10766
```

```r
median(step.date$steps)
```

```
## [1] 10766
```


## Are there differences in activity patterns between weekdays and weekends?

```r
days.analysis <- function(date) {
  if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
    "weekend"
  } else {
    "weekday"
  }
}
activity$days.analysis <- as.factor(sapply(activity$date, days.analysis))

par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
  steps.type <- aggregate(steps ~ interval, data = activity, subset = activity$days.analysis == 
                            type, FUN = mean)
  plot(steps.type, type = "l", main = type)
}
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

