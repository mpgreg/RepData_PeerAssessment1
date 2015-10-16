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


fileURL <- "https://github.com/mpgreg/RepData_PeerAssessment1/blob/master/activity.zip"
##fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
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

##change zero values in steps to NA
is.na(activityDF$steps) <- !activityDF$steps

##1. Make a histogram of the total number of steps taken each day (ignoring missing values)

##calculate cross-tabulation of total steps by day
stepsXtab <- xtabs(steps ~ date, activityDF)
hist(stepsXtab, main = "Histogram of Total Steps Taken per Day", xlab = "steps")

##2. Calculate and report the mean and median total number of steps taken per day
library(xtable)
meanSteps <- round(mean(as.matrix(stepsXtab)))
medianSteps <- median(as.matrix(stepsXtab))
print(xtable(matrix(c("Mean", "Median", meanSteps, medianSteps)), type = "html"))









