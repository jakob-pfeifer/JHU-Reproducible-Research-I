------------------------------------------------------------------------

Loading and preprocessing the data
----------------------------------

Unzip data to obtain the csv file

``` r
library("data.table")
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.6.2

``` r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl, destfile = paste0(getwd(), '/repdata%2Fdata%2Factivity.zip'), method = "curl")
unzip("repdata%2Fdata%2Factivity.zip",exdir = "data")
```

Reading .csv data into data.table

``` r
activityDT <- data.table::fread(input = "data/activity.csv")
```

What is mean total number of steps taken per day?
-------------------------------------------------

Calculating the total number of steps taken per day

``` r
Total_Steps <- activityDT[, c(lapply(.SD, sum, na.rm = FALSE)), .SDcols = c("steps"), by = .(date)] 
head(Total_Steps, 10)
```

    ##           date steps
    ##  1: 2012-10-01    NA
    ##  2: 2012-10-02   126
    ##  3: 2012-10-03 11352
    ##  4: 2012-10-04 12116
    ##  5: 2012-10-05 13294
    ##  6: 2012-10-06 15420
    ##  7: 2012-10-07 11015
    ##  8: 2012-10-08    NA
    ##  9: 2012-10-09 12811
    ## 10: 2012-10-10  9900

Making a histogram of the total steps taken per day

``` r
ggplot(Total_Steps, aes(x = steps)) +
    geom_histogram(fill = "red", binwidth = 1000) +
    labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

    ## Warning: Removed 8 rows containing non-finite values (stat_bin).

![](PA1_template_files/figure-markdown_github/unnamed-chunk-4-1.png)
Calculating the mean and median of the total number of steps taken per
day

``` r
Total_Steps[, .(Mean_Steps = mean(steps, na.rm = TRUE), Median_Steps = median(steps, na.rm = TRUE))]
```

    ##    Mean_Steps Median_Steps
    ## 1:   10766.19        10765

What is the average daily activity pattern?
-------------------------------------------

Making a time series plot (i.e. 𝚝𝚢𝚙𝚎 = “𝚕”) of the 5-minute interval
(x-axis) and the average number of steps taken, averaged across all days
(y-axis)

``` r
IntervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)] 

ggplot(IntervalDT, aes(x = interval , y = steps)) + geom_line(color="red", size=1) + labs(title = "Average Daily Steps", x = "Interval", y = "Avgerage Steps per day")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-6-1.png)

Selecting the interval across all the days in the dataset, containing
the maximum number of steps

``` r
IntervalDT[steps == max(steps), .(max_interval = interval)]
```

    ##    max_interval
    ## 1:          835

Imputing missing values
-----------------------

Calculating the total number of missing values in the dataset

``` r
activityDT[is.na(steps),]
```

    ##       steps       date interval
    ##    1:    NA 2012-10-01        0
    ##    2:    NA 2012-10-01        5
    ##    3:    NA 2012-10-01       10
    ##    4:    NA 2012-10-01       15
    ##    5:    NA 2012-10-01       20
    ##   ---                          
    ## 2300:    NA 2012-11-30     2335
    ## 2301:    NA 2012-11-30     2340
    ## 2302:    NA 2012-11-30     2345
    ## 2303:    NA 2012-11-30     2350
    ## 2304:    NA 2012-11-30     2355

Filling in the missing values with the median of the dataset

``` r
activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
```

Creating a new dataset that is equal to the original dataset but with
the missing data filled in

``` r
data.table::fwrite(x = activityDT, file = "data/tidyData.csv", quote = FALSE)
```

Making a histogram of the total number of steps taken each day and
calculating the mean and median total number of steps taken per day.

``` r
# total number of steps taken per day
Total_Steps <- activityDT[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)] 

# mean and median total number of steps taken per day
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]
```

    ##    Mean_Steps Median_Steps
    ## 1:    9354.23        10395

``` r
ggplot(Total_Steps, aes(x = steps)) + geom_histogram(fill = "red", binwidth = 1000) + labs(title = "Daily Steps", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-12-1.png)

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

Creating a new factor variable in the dataset with two levels –
“weekday” and “weekend” indicating whether a given date is a weekday or
weekend day

``` r
activityDT <- data.table::fread(input = "data/activity.csv")
activityDT[, date := as.POSIXct(date, format = "%Y-%m-%d")]
activityDT[, `Day of Week`:= weekdays(x = date)]
activityDT[grepl(pattern = "Montag|Dienstag|Mittwoch|Donnerstag|Freitag", x = `Day of Week`), "weekday or weekend"] <- "weekday"
activityDT[grepl(pattern = "Samstag|Sonntag", x = `Day of Week`), "weekday or weekend"] <- "weekend"
activityDT[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(activityDT, 10)
```

    ##     steps       date interval Day of Week weekday or weekend
    ##  1:    NA 2012-10-01        0      Montag            weekday
    ##  2:    NA 2012-10-01        5      Montag            weekday
    ##  3:    NA 2012-10-01       10      Montag            weekday
    ##  4:    NA 2012-10-01       15      Montag            weekday
    ##  5:    NA 2012-10-01       20      Montag            weekday
    ##  6:    NA 2012-10-01       25      Montag            weekday
    ##  7:    NA 2012-10-01       30      Montag            weekday
    ##  8:    NA 2012-10-01       35      Montag            weekday
    ##  9:    NA 2012-10-01       40      Montag            weekday
    ## 10:    NA 2012-10-01       45      Montag            weekday

Making a panel plot containing a time series plot of the 5-minute
interval (x-axis) and the average number of steps taken, averaged across
all weekday days or weekend days (y-axis).

``` r
activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
IntervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 

ggplot(IntervalDT , aes(x = interval , y = steps, color = `weekday or weekend`)) + geom_line() + labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-14-1.png)
