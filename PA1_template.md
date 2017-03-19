---
title: "Reproducible Research Project 1"
author: Cameron Johnston
output: html_document
---

## About
This is the first peer-reviewed project for the Reproducible Research Coursera class.

## Data
The data for this was downloaded from the assignment on the course website.

* Dataset: [Activity Monitoring Data] (https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)

The variables included in this dataset are:

* Steps: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰)

* Date: The date on which the measurement was taken in YYYY-MM-DD format

* Interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data
Download, unzip and load data into data frame `data`. 
```{r}
activity <- read.csv("activity.csv")
activity_day <- weekdays(as.Date(activity$date))
activity_DateTime <- as.POSIXct(activity$date, format="%Y-%m-%d")
activity_clean <- activity[!is.na(activity$steps),]

```

## What is the mean total number of steps taken per day?
Sums the steps per day, creates a Histogram, and calculates the mean and median.
```{r}
steps_per_day <- aggregate(steps ~ date, activity_clean, sum)
hist(steps_per_day$steps, main = paste("Total Steps Per Day"), col = "blue", xlab = "Number of Steps")
rmean <- mean(steps_per_day$steps)
rmedian <- median(steps_per_day$steps)
```

The 'mean' is 'rmean' and the median is 'rmedian'

## What is the average daily activity pattern?
* Calculate average steps for each interval for all days. 
* Plot the Average Number Steps per Day by Interval. 
* Find interval with most average steps. 

```{r}
steps_by_interval <- aggregate(steps ~ interval, activity_clean, mean)
plot(steps_by_interval$interval, steps_by_interval$steps, type = "l", xlab="Interval", ylab="Number of Steps", main = "Average Number of Steps per Day by Interval")
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]

```

The 5-minute interval, on average across all the days in the data set, containing the maximum number of steps is `r max_interval`.

## Impute missing values.  Compare imputed to non-imputed data.

Missing data is to be imputed.  Missing values imputed by inserting the average for each interval.

```{r}
incomplete <- sum(!complete.cases(activity))
imputed_data <- transform(activity, steps = ifelse(is.na(activity$steps), steps_by_interval$steps[match(activity$interval, steps_by_interval$interval)], activity$steps))


```

Zeroes were imputed for 10-01-2012 because it was the first day and would have been over 9,000 steps higher than the following day, which had only 126 steps. NAs then were assumed to be zeros to fit the rising trend of the data. 
```{r}
imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0
```

Recount total steps by day and create Histogram. 
```{r}
steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
hist(steps_per_day$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("imputed", "Non-imputed"), col = c("blue", "red"), lwd = 10)
```

Calculate new mean and median for imputed data. 
```{r}
rmean.i <- mean(steps_by_day_i$steps)
rmedian.i <- median(steps_by_day_i$steps)
```

Calculate difference between imputed and non-imputed data.
```{r}
mean_diff <- rmean.i - rmean
med_diff <- rmedian.i - rmedian
```

Calculate total difference.
```{r}
total_diff <- sum(steps_by_day_i$steps) - sum(steps_per_day$steps)

```

* The imputed data mean is `r rmean.i`
* The imputed data median is `r rmedian.i`
* The difference between the non-imputed mean and imputed mean is `r mean_diff`
* The difference between the non-imputed mean and imputed mean is `r med_diff`
* The difference between total number of steps between imputed and non-imputed data is `r total_diff`. Thus, there were `r total_diff` more steps in the imputed data.

## Are there activity differences between weekends and weekdays?

Created a plot to compare and contrast number of steps between the week and weekend. There is a higher peak earlier on weekdays, and more overall activity on weekends.  
```{r}
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))
steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)
library(lattice)
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")

```

