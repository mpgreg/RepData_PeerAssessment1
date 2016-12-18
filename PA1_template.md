# Reproducible Research: Peer Assessment 1




##Introduction

This project analyses personal movement using activity monitoring data from an anonymous individual. The data consists of two months of data collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.  The data, downloaded from https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip includes 17,568 observations (5 minutes intervals) of the following variables in csv format:

-steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)  
-date: The date on which the measurement was taken in YYYY-MM-DD format  
-interval: Identifier for the 5-minute interval in which measurement was taken  
  
  
  
##Loading and preprocessing the data
1. Load the data


```r
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
activityFile <- "activity.csv"
tempFile <- tempfile()
if(capabilities("libcurl")) {
        download.file(fileURL, tempFile, method = "libcurl")        
} else {
        download.file(fileURL, tempFile)
}
activityDF <- read.csv(unz(tempFile, activityFile))
unlink(tempFile)
```

2. Process/transform the data into a format suitable for your analysis

```r
#set the date data type
activityDF$date <- as.Date(activityDF$date)
```


##What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day (ignoring missing values)

```r
sumStepsByDay <- aggregate(steps ~ date, FUN=sum, data=activityDF, na.action = "na.omit")
```

2. Make a histogram of the total number of steps taken each day


```r
hist(sumStepsByDay$steps, main = "Histogram of Total Steps Taken per Day (ignoring missing values)", xlab = "steps")
```

![](PA1_template_files/figure-html/stepshist-1.png) 

3. Calculate and report the mean and median total number of steps taken per day


```r
library(xtable)
meanSteps <- round(mean(sumStepsByDay$steps))
medianSteps <- round(median(sumStepsByDay$steps))
xt <- xtable(matrix(c(meanSteps, medianSteps), dimnames = list(c("Mean","Median"),c("Steps"))), digits = 0)
print(xt, type="html")
```

<!-- html table generated in R 3.2.0 by xtable 1.7-4 package -->
<!-- Sun Dec 18 20:18:24 2016 -->
<table border=1>
<tr> <th>  </th> <th> Steps </th>  </tr>
  <tr> <td align="right"> Mean </td> <td align="right"> 10766 </td> </tr>
  <tr> <td align="right"> Median </td> <td align="right"> 10765 </td> </tr>
   </table>



##What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
meanStepsByInterval <- aggregate(steps ~ interval, FUN=mean, data=activityDF, na.action = "na.omit")
plot(meanStepsByInterval, type="l",
     main = "Average Steps Taken per 5-min Interval", 
     ylab = "Averages steps across all days",
     xlab = "5-min Interval")
```

![](PA1_template_files/figure-html/showplot-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxMeanSteps <- meanStepsByInterval[which.max(meanStepsByInterval$steps),]
```

On average, the 5-minute interval 835 contains the most (206) steps.  



##Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
totalNAs <- sum(is.na(activityDF))
```

There are 2304 rows in the activity dataset containing missing (NA) values.  

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
#Create a new column in the dataset with the mean for that 5-minute interval across all days.
activityDF <- merge(activityDF, meanStepsByInterval, by = "interval")
colnames(activityDF) <- c("interval", "steps", "date", "dailyMeanSteps")
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
activityDF[is.na(activityDF$steps),]$steps <- activityDF[is.na(activityDF$steps),]$dailyMeanSteps
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
imputedSumStepsByDay <- aggregate(steps ~ date, FUN=sum, data=activityDF)
hist(imputedSumStepsByDay$steps, main = "Histogram of Total Steps Taken per Day (with imputed missing values)", xlab = "steps")
```

![](PA1_template_files/figure-html/showimputed-1.png) 

```r
imputedMeanSteps <- round(mean(imputedSumStepsByDay$steps))
imputedMedianSteps <- round(median(imputedSumStepsByDay$steps))
imputedXT <- xtable(matrix(c(imputedMeanSteps, imputedMedianSteps), dimnames = list(c("Imputed Mean","Imputed Median"),c("Steps"))), digits = 0)
print(imputedXT, type="html")
```

<!-- html table generated in R 3.2.0 by xtable 1.7-4 package -->
<!-- Sun Dec 18 20:18:24 2016 -->
<table border=1>
<tr> <th>  </th> <th> Steps </th>  </tr>
  <tr> <td align="right"> Imputed Mean </td> <td align="right"> 10766 </td> </tr>
  <tr> <td align="right"> Imputed Median </td> <td align="right"> 10766 </td> </tr>
   </table>

```
## The average steps per day did not change as a result of the imputation.
```

```
## As a result of the imputation the median steps per day changed from 10765 to 10766.
```

For this dataset imputaion using daily average did not have a significant impact on analysis.



##Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
weekendNames <- c("Saturday", "Sunday")
activityDF$Type.of.Day <- as.factor(ifelse(weekdays(activityDF$date) %in% weekendNames,"weekend", "weekday"))
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
library(lattice)
meanStepsByDayType <- aggregate(steps ~ interval + Type.of.Day, FUN=mean, data=activityDF, na.action = "na.omit")
xyplot(steps ~ interval | Type.of.Day, meanStepsByDayType, 
       type = "l", 
       layout = c(1,2), 
       main = "Mean Steps Taken per Interval by Day Type", 
       ylab = "Number of Steps (mean)")
```

![](PA1_template_files/figure-html/plotbydaytype-1.png) 

