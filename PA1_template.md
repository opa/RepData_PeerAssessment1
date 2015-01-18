---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

* URL and name of data as provided in assignment


```r
    # constants
    zip_file_url <- 'https://d396qusza40orc.cloudfront.net/repdata/data/activity.zip'
    zip_file_name <- 'activity.zip'
    data_file_name <- 'activity.csv'

    # install and/or load Hadley's ggplot2
    package <- 'ggplot2'

    if(package %in% rownames(installed.packages()) == FALSE) {install.packages(package)}
    do.call(library, as.list(eval(package)))    
```


## Loading and preprocessing the data

### Show any code that is needed to

1. Load the data (i.e. read.csv())


```r
    # Download the data from url
    if ( !file.exists( zip_file_name) ) {
      download.file( url= zip_file_url, 
                    destfile= zip_file_name, 
                    method= "curl")
      unzip( zip_file_name)
    }

    # unzip data
    if ( !file.exists( data_file_name) ) {
      unzip(zipfile = data_file_name )
    }

    # load data
    activity <- read.csv(data_file_name, 
                         colClasses = c("integer", 
                                        "Date", 
                                        "factor"))
```


2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
    # summarize data
    summary(activity)
```

```
##      steps             date               interval    
##  Min.   :  0.00   Min.   :2012-10-01   0      :   61  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   10     :   61  
##  Median :  0.00   Median :2012-10-31   100    :   61  
##  Mean   : 37.38   Mean   :2012-10-31   1000   :   61  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   1005   :   61  
##  Max.   :806.00   Max.   :2012-11-30   1010   :   61  
##  NA's   :2304                          (Other):17202
```

```r
    # ignore NA steps as per instructions
    ignoreNA <- na.omit(activity)

    summary(ignoreNA)
```

```
##      steps             date               interval    
##  Min.   :  0.00   Min.   :2012-10-02   0      :   53  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   10     :   53  
##  Median :  0.00   Median :2012-10-29   100    :   53  
##  Mean   : 37.38   Mean   :2012-10-30   1000   :   53  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-16   1005   :   53  
##  Max.   :806.00   Max.   :2012-11-29   1010   :   53  
##                                        (Other):14946
```

## What is mean total number of steps taken per day?


##### For this part of the assignment, you can ignore the missing values in the dataset.

```r
  # aggregate steps per day throughout all days
  steps.per.day <- aggregate(list(steps = ignoreNA$steps),
                             list(date = ignoreNA$date), 
                             sum, 
                             na.rm=TRUE)
```

### Make a histogram of the total number of steps taken each day

```r
  # draw histogram of total steps per day 
  hist(steps.per.day$steps,
       breaks=30, 
       xlab="total number of steps taken each day",
       main="histogram total steps per day")
```

![plot of chunk stepsHist](figure/stepsHist-1.png) 


### Calculate and report the mean and median total number of steps taken per day


```r
  # calculate mean and median steps per day
  stepsMean <- mean(steps.per.day$steps)
  stepsMedian <- median(steps.per.day$steps)
```

* There were 10,766 mean steps per day, and 10,765 median steps per day.

## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
# calculate the average steps for each interval
interval.mean <- aggregate(list(steps= ignoreNA$steps),
                           list(interval= ignoreNA$interval),
                           mean, 
                           na.rm=TRUE)

#convert the interval from a factor back into an integer
interval.mean$interval <-  as.integer(as.character(interval.mean$interval))

#order data by interval
interval.mean <- interval.mean[order(interval.mean$interval),]

#create plot
plot( interval.mean$interval, 
     interval.mean$steps, 
     type="l", 
     main="average daily activity pattern",
     xlab="5-minute interval", 
     ylab="average number of steps taken")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
  #find the interval row with the max average steps
  intervalMean.max <- interval.mean[interval.mean$steps == max(interval.mean$steps), ]
```

* The 835 interval contains on average the max number of steps (at 206 steps).

## Imputing missing values

##### Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

##### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
    numNArows <- nrow(activity) - nrow(ignoreNA)
```

* There are 2,304 missing values in the dataset.

##### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

##### Create a new dataset that is equal to the original dataset but with the missing data filled in.

* Method chosen -- using calculated cross-day mean values for each 5-minute interval and substituting this into any specific intervals that currently have NA values


```r
  # set the activity interval into an integer from a factor to get ready for join
  activity$interval <-  as.integer(as.character(activity$interval))

  # join activity to interval.mean based upon interval, and create a new dataframe
  replaceNA <- merge(activity, interval.mean, by="interval")

  # replace NA values from original (x) dataset
  replaceNA[is.na(replaceNA$steps.x),]$steps.x <- replaceNA[is.na(replaceNA$steps.x),]$steps.y

  # drop the y data columns
  replaceNA <- replaceNA[,c('interval','date','steps.x')]

  # rename steps.x back to steps
  names(replaceNA)[names(replaceNA) == 'steps.x'] <- 'steps'
```

##### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
# aggregate steps per day throughout all days
  new.steps.per.day <- aggregate(list(steps = replaceNA$steps),
                             list(date = replaceNA$date), 
                             sum, 
                             na.rm=TRUE)

  # draw histogram of total steps per day 
  hist(new.steps.per.day$steps,
       breaks=30, 
       xlab="total number of steps taken each day",
       main="histogram total steps per day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

```r
  # calculate mean and median steps per day
  new.stepsMean <- mean(new.steps.per.day$steps)
  new.stepsMedian <- median(new.steps.per.day$steps)
```

##### Do these values differ from the estimates from the first part of the assignment? 
* With all NA's replaced with a average value for the same interval but across all days in the dataset, there are 10,766 mean steps per day, and 10,766 median steps per day.  

##### What is the impact of imputing missing data on the estimates of the total daily number of steps?
The mean and median remain very close to the original values ... no significant change to the mean and median resulted when the NA's where replaced by associated mean values within the dataset.  However, notice that the histogram indicates that the data is more concentrated toward the center around the mean and the magnitude of the mean in the histogram is much more pronounced.

## Are there differences in activity patterns between weekdays and weekends?

##### For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

##### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
    replaceNA$weekday <- weekdays(replaceNA$date)
    replaceNA$week.class <- "weekday"
    replaceNA[replaceNA$weekday %in% c('Saturday','Sunday') ,]$week.class = 'weekend'
```

##### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
# calculate the average steps for each interval
new.interval.mean <- aggregate(list(steps= replaceNA$steps),
                           list(interval= replaceNA$interval, 
                                week.class= replaceNA$week.class),
                           mean, 
                           na.rm=TRUE)

#order data by interval
new.interval.mean <- new.interval.mean[order(c(new.interval.mean$week.class,new.interval.mean$interval)),]

#create plot
library(lattice)
xyplot(new.interval.mean$steps ~ new.interval.mean$interval | new.interval.mean$week.class, 
       layout = c(1, 2), 
       type = "l", 
       xlab = "Interval", 
       ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 


Notice that the graph indicates that on the weekend the subject is more active througout the day as compared to the activity dispersion during the week. 


