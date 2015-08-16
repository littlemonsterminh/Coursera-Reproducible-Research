### This is Coursera Project. Author: Minh Pham
##  Download URL: https://github.com/littlemonsterminh/Coursera-Reproducible-Research
##  Data file included in the repository link
##  To run the R file: download the file to computer. Change R directory
##  Type source("project1.r") to load the R code from this file to R and run the scripts.
read

# MAKING THE CODE VISIBLE

echo = TRUE  # Always make code visible
options(scipen = 1)  # Turn off scientific notations for numbers

# LOADING AND PROCESSING THE DATA:

data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
data$month <- as.numeric(format(data$date, "%m"))
noNA <- na.omit(data)
rownames(noNA) <- 1:nrow(noNA)
head(noNA)
dim(noNA)
library(ggplot2)

## What is mean total number of steps taken per day?
# PLOT THE HISTOGRAM:

ggplot(noNA, aes(date, steps)) + geom_bar(stat = "identity", 
	colour = "steelblue", fill = "steelblue", width = 0.7) 
	+ facet_grid(. ~ month, scales = "free") 
	+ labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")

# MEAN:

totalSteps <- aggregate(noNA$steps, list(Date = noNA$date), FUN = "sum")$x
mean(totalSteps)

# MEDIAN:
median(totalSteps)

## What is the average daily activity pattern?
# MAKE A TIME SERIES PLOT (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

avgSteps <- aggregate(noNA$steps, list(interval = as.numeric(as.character(noNA$interval))), FUN = "mean")
names(avgSteps)[2] <- "meanOfSteps"

ggplot(avgSteps, aes(interval, meanOfSteps)) 
	+ geom_line(color = "steelblue", size = 0.8) 
	+ labs(title = "Time Series Plot of the 5-minute Interval", x = "5-minute intervals", y = "Average Number of Steps Taken")

# MAXIMUM NUMBEROF STEPS: Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

avgSteps[avgSteps$meanOfSteps == max(avgSteps$meanOfSteps), ]

## Imputing missing values
# I HAVE NO IDEA !

## Are there differences in activity patterns between weekdays and weekends?
# I HAVE NO IDEA !

