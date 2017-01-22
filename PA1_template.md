# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

We load the data from the current folder and add a new column representing the day of the week.

```r
activity <- read.csv("activity.csv", header=TRUE)
activity$date <- as.Date(activity$date)
activity$day <- weekdays(activity$date)
str(activity)
```

```
## 'data.frame':	17568 obs. of  4 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ day     : chr  "Monday" "Monday" "Monday" "Monday" ...
```

## What is mean total number of steps taken per day?

First we sum the steps across each day and plot a histogram of the number of steps per day.

```r
#Do not use na.rm for aggregate function, or will get histogram that include days with missing steps
stepsday <- aggregate(activity$steps, list(activity$date), sum)
colnames(stepsday) <- c("date", "steps")
hist(stepsday$steps, main = "Total steps taken per day", xlab = "Steps", ylim = c(0,30))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

We then calculate the mean and median for each day.

```r
meansteps <- round(mean(stepsday$steps, na.rm = TRUE), 0)
meansteps
```

```
## [1] 10766
```

```r
mediansteps <- round(median(stepsday$steps, na.rm = TRUE), 0)
mediansteps
```

```
## [1] 10765
```

The mean of the total number of steps taken per day is 10766. The median of the total number of steps taken per day is 10765.

## What is the average daily activity pattern?

First, we make a time series plot of the average number of steps taken through the day, as averaged across all days.

```r
stepsblock <- aggregate(activity$steps, list(activity$interval), mean, na.rm = TRUE)
colnames(stepsblock) <- c("interval", "steps")
plot(stepsblock, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

We then calculate which interval contains the maximum number of steps.

```r
intervalMax <- stepsblock[which.max(stepsblock$steps),]
intervalMax
```

```
##     interval    steps
## 104      835 206.1698
```
The interval in which the maximum number of steps occurs is 835, when 206 steps are taken.

## Imputing missing values

First we calculate the number of rows with missing values.

```r
missing <- sum(is.na(activity$steps))
missing
```

```
## [1] 2304
```
There are 2304 rows with missing values. 

To determine our strategy for imputing missing values, we will first check the pattern of missing values across days.

```r
table(activity$day, is.na(activity$steps))
```

```
##            
##             FALSE TRUE
##   Friday     2016  576
##   Monday     2016  576
##   Saturday   2016  288
##   Sunday     2016  288
##   Thursday   2304  288
##   Tuesday    2592    0
##   Wednesday  2304  288
```
Where values are missing, they are missing for a full day, and the mix of days is uneven.

As a result (and anticipating later analysis comparing weekdays and weekends), we will impute values by determining the mean steps for each interval and for each day of the week, and use that to fill the missing values for those days with no entries.

We create our imputed dataset by finding the mean across days and intervals. We then fill the missing values.

```r
# Create dataset of mean values that will be used to fill gaps
imputed <- aggregate(steps ~ day + interval, data = activity, FUN = mean, na.rm = TRUE)

# Create a new data.frame for complete dataset and a loop to fill missing values
complete <- activity

for (i in 1:dim(complete)[1]) {
    if (is.na(complete$steps[i])) {
        day <- complete$day[i]
        interval <- complete$interval[i]
        complete$steps[i] <- imputed$steps[imputed$day == day & imputed$interval == interval]
    }
}
```

For this new complete dataset, we produce a histogram of the steps each day, and the new mean and median steps taken.

```r
#Calculate the average steps per day
stepsdayImputed <- aggregate(complete$steps, list(complete$date), sum)
colnames(stepsdayImputed) <- c("date", "steps")

#Produce the histogram
hist(stepsdayImputed$steps, main = "Total steps taken per day", xlab = "Steps", ylim = c(0,40))
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
#Calculate the mean and median
meanstepsImputed <- round(mean(stepsdayImputed$steps, na.rm = TRUE), 0)
meanstepsImputed
```

```
## [1] 10821
```

```r
medianstepsImputed <- round(median(stepsdayImputed$steps, na.rm = TRUE), 0)
medianstepsImputed
```

```
## [1] 11015
```

For the complete dataset with missing values imputed, the mean of the total number of steps taken per day is 10821. The median of the total number of steps taken per day is 11015. (Note: if we had taken the mean across all days, this mean would not have changed from the earlier answer - and the median would have been equal to the mean - coming from one of the imputed days.)

## Are there differences in activity patterns between weekdays and weekends?

First we develop a function to identify whether the day of the week is a weekday or weekend, and append it in a new column to our complete dataset.

```r
#Develop a function to identify whether the day of the week is a weekend or weekday, and create a new column to store the result
we <- function(x) {
    if (x == "Monday" | x == "Tuesday" | x == "Wednesday" | x == "Thursday" | x == "Friday") {
        y <- "weekday"
        }
    if (x == "Saturday" | x == "Sunday") {
        y <- "weekend"
    }
    y
}

complete$we <- complete$day
for (i in 1:dim(complete)[1]) {
    complete$we[i] <- we(complete$we[i])
}
```

We then average across weekdays and weekends to create plots of the average steps in each interval for weekdays and weekends.

```r
#Average across weedays and weekends
stepsWeekAverage <- aggregate(steps ~ interval + we, data = complete, FUN = mean)

#Plot 
library(lattice)
xyplot(steps ~ interval | we, data=stepsWeekAverage, type="l", layout = c(1, 2))
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
#Determine the mean number of steps
aggregate(steps ~ we, data = stepsWeekAverage, FUN = sum)
```

```
##        we    steps
## 1 weekday 10257.53
## 2 weekend 12406.57
```

There are differences in the patterns between weekdays and weekends. Weekdays have more steps early in the day and less through the body of the day, while weekends have a more even distribution of steps. On average, more steps are taken on a weekend.
