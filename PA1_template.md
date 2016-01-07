# Reproducible Research: Peer Assessment 1
##Load neccesary libraries

```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.2.3
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lubridate)
```

```
## Warning: package 'lubridate' was built under R version 3.2.3
```

```r
library(chron)
```

```
## Warning: package 'chron' was built under R version 3.2.3
```

```
## 
## Attaching package: 'chron'
## 
## The following objects are masked from 'package:lubridate':
## 
##     days, hours, minutes, seconds, years
```

```r
library(lattice)
```

## Loading and preprocessing the data
Show any code that is needed to

Load the data (i.e. read.csv())
Process/transform the data (if necessary) into a format suitable for your analysis




```r
activity <- read.csv("activity.csv", header = TRUE, sep = ",")
activity$date <- ymd(activity$date)
actByDay <- group_by(activity, date)
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

Calculate the total number of steps taken per day

```r
stepsByDay <- summarise(actByDay, totSteps = sum(steps))
head(stepsByDay)
```

```
## Source: local data frame [6 x 2]
## 
##         date totSteps
##       (time)    (int)
## 1 2012-10-01       NA
## 2 2012-10-02      126
## 3 2012-10-03    11352
## 4 2012-10-04    12116
## 5 2012-10-05    13294
## 6 2012-10-06    15420
```

If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```r
hist(stepsByDay$totSteps, breaks = seq(0, 22000, by = 1000))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

Calculate and report the mean and median of the total number of steps taken per day


```r
mean(stepsByDay$totSteps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(stepsByDay$totSteps, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

There are 288 five minute intervals each day.


```r
actByInterval <- group_by(actByDay, interval)
stepsByInterval <- summarise(actByInterval, avgSteps = mean(steps, na.rm = TRUE))
xrange <- range(stepsByInterval$interval)
yrange <- range(stepsByInterval$avgSteps)
plot(xrange, yrange, type = "n", xlab = "5-minute interval", ylab = "Number of steps")
lines(stepsByInterval$interval,stepsByInterval$avgSteps, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

The 5 minute interval with the most steps on average across all days is

```r
most <- filter(stepsByInterval, avgSteps == max(stepsByInterval$avgSteps))
most[1,1]
```

```
## Source: local data frame [1 x 1]
## 
##   interval
##      (int)
## 1      835
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
ok <- complete.cases(activity)
nrow(activity) - sum(ok)
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
activity$avgSteps <- stepsByInterval$avgSteps
filledInMissing <- mutate(activity, steps = ifelse(is.na(steps), avgSteps, steps)) %>%
    select(steps, date, interval)
head(filledInMissing)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?



```r
filledByDay <- group_by(filledInMissing, date)

filledStepsByDay <- summarise(filledByDay, totSteps = sum(steps))
head(filledStepsByDay)
```

```
## Source: local data frame [6 x 2]
## 
##         date totSteps
##       (time)    (dbl)
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
hist(filledStepsByDay$totSteps, breaks = seq(0, 22000, by = 1000))
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
mean(filledStepsByDay$totSteps)
```

```
## [1] 10766.19
```

```r
median(filledStepsByDay$totSteps)                       
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
filledInMissing2 <- mutate(filledInMissing, weekday = wday(date, label = FALSE), weekday_end = ifelse(is.weekend(date), "weekend", "weekday"))
stepsByWeekday_end <- group_by(filledInMissing2, weekday_end, interval) %>% summarise(avgSteps = mean(steps))
xyplot(avgSteps ~ interval | weekday_end, data = stepsByWeekday_end, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
