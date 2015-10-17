## CDSS Reproducible Research
## Project 1 code


## Written by: Michael Gregory
## Date: 15-Oct-2015

## Original data file at https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip
##This assignment makes use of data from a personal activity monitoring device. This device collects 
##data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous 
##individual collected during the months of October and November, 2012 and include the number of steps 
##taken in 5 minute intervals each day.

##The data for this assignment can be downloaded from the course web site:
##        Dataset: Activity monitoring data [52K]
##The variables included in this dataset are:      
##steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
##date: The date on which the measurement was taken in YYYY-MM-DD format
##interval: Identifier for the 5-minute interval in which measurement was taken
##The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.


##setwd("~/Documents/School/coursera/data science/reproducible research/project1/")


##fileURL <- "https://github.com/mpgreg/RepData_PeerAssessment1/blob/master/activity.zip"
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

##set the date data type
activityDF$date <- as.Date(activityDF$date)

##set the interval as a factor
##activityDF$interval <- as.factor(activityDF$interval)

##change zero values in steps to NA
##is.na(activityDF$steps) <- !activityDF$steps

##What is mean total number of steps taken per day?
##1. Calculate the total number of steps taken per day
##sumStepsByDay <- tapply(activityDF$steps, activityDF$date, FUN=sum, na.rm=TRUE)

sumStepsByDay <- aggregate(steps ~ date, FUN=sum, data=activityDF, na.action = "na.omit")

##2. Make a histogram of the total number of steps taken each day (ignoring missing values)
hist(sumStepsByDay$steps, main = "Histogram of Total Steps Taken per Day (ignoring missing values)", xlab = "steps")

##3. Calculate and report the mean and median total number of steps taken per day
library(xtable)
meanSteps <- round(mean(sumStepsByDay$steps))
medianSteps <- round(median(sumStepsByDay$steps))
xt <- xtable(matrix(c(meanSteps, medianSteps), dimnames = list(c("Mean","Median"),c("Steps"))), digits = 0)
print(xt, type="html")

##What is the average daily activity pattern?
#1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
##meanStepsByInterval <- round(tapply(activityDF$steps, activityDF$interval, FUN=mean, na.rm=TRUE), digits = 2)

meanStepsByInterval <- aggregate(steps ~ interval, FUN=mean, data=activityDF, na.action = "na.omit")
plot(meanStepsByInterval, type="l",
     main = "Average Steps Taken per 5-min Interval", 
     ylab = "Averages steps across all days",
     xlab = "5-min Interval")

#2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
##maxMeanSteps <- meanStepsByInterval[which.max(meanStepsByInterval)]

maxMeanSteps <- meanStepsByInterval[which.max(meanStepsByInterval$steps),]
cat(sprintf("On average, the 5-minute interval %s contains the most (%s) steps.\n", maxMeanSteps$interval, round(maxMeanSteps$steps)))

##Imputing missing values

#Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
#1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
totalNAs <- sum(is.na(activityDF))
cat(sprintf("There are %s rows in the activity dataset containing missing (NA) values.\n", totalNAs))

#2. Create a new column in the dataset with the mean for that 5-minute interval across all days.

activityDF <- merge(activityDF, meanStepsByInterval, by = "interval")
colnames(activityDF) <- c("interval", "steps", "date", "Mean.Daily.Steps")

#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

activityDF[is.na(activityDF$steps),]$steps <- activityDF[is.na(activityDF$steps),]$Mean.Daily.Steps


#4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. 
#Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

imputedSumStepsByDay <- aggregate(steps ~ date, FUN=sum, data=activityDF)
hist(imputedSumStepsByDay$steps, main = "Histogram of Total Steps Taken per Day (with imputed missing values)", xlab = "steps")
imputedMeanSteps <- round(mean(imputedSumStepsByDay$steps))
imputedMedianSteps <- round(median(imputedSumStepsByDay$steps))
imputedXT <- xtable(matrix(c(imputedMeanSteps, imputedMedianSteps), dimnames = list(c("Imputed Mean","Imputed Median"),c("Steps"))), digits = 0)
print(imputedXT, type="html")

if(!meanSteps==imputedMeanSteps) {
        cat(sprintf("As a result of the imputation the average steps per day changed from %s to %s.\n", meanSteps, imputedMeanSteps))
} else {
        cat(sprintf("The average steps per day did not change as a result of the imputation."))
}

if(!medianSteps==imputedMedianSteps) {
        cat(sprintf("As a result of the imputation the median steps per day changed from %s to %s.\n", medianSteps, imputedMedianSteps))
} else {
        cat(sprintf("The median steps per day did not change as a result of the imputation."))
}
#For this dataset imputing using daily average did not have a significant impact on analysis.


##Are there differences in activity patterns between weekdays and weekends?

##For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

#1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

##weekdayNames <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekendNames <- c("Saturday", "Sunday")
activityDF$Type.of.Day <- as.factor(ifelse(weekdays(activityDF$date) %in% weekendNames,"weekend", "weekday"))


#2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
        
library(lattice)
meanStepsByDayType <- aggregate(steps ~ interval + Type.of.Day, FUN=mean, data=activityDF, na.action = "na.omit")
xyplot(steps ~ interval | Type.of.Day, meanStepsByDayType, 
       type = "l", 
       layout = c(1,2), 
       main = "Mean Steps Taken per Interval by Day Type", 
       ylab = "Number of Steps (mean)")



