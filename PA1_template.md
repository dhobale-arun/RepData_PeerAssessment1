###Load the data (i.e. read.csv())
```{r,echo=TRUE}
activity <- read.csv("activity.csv",colClasses = c("numeric", "character","integer"))
```
###Process/transform the data (if necessary) into a format suitable for your analysis
```{r,echo=TRUE}
dim(activity)
```
###Load different libraries 
```{r,echo=TRUE}
library(plyr)
library(dplyr)
library(lubridate)
library(ggplot2)
total.steps <- tapply(activity$steps, activity$date, FUN = sum, na.rm = TRUE)
activity$date <- ymd(activity$date)
```
#Part One
##What is mean total number of steps taken per day? Calculate and report the mean and median of the total number of steps taken per day.
```{r,echo=TRUE}
mean(total.steps)
```
```{r,echo=TRUE}
median(total.steps)
```
###For this part of the assignment, you can ignore the missing values in the dataset.
##Calculate the total number of steps taken per day
```{r,echo=TRUE}
steps <- activity %>%
  filter(!is.na(steps)) %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print      
```
##Make a histogram of the total number of steps taken each day
```{r,echo=TRUE}
ggplot(steps, aes(x=date, y=steps))+geom_histogram(stat="identity")+ xlab("Dates")+ ylab("Steps")+ labs(title= "Total numbers of Steps per day")
```
#Part Two
##What is the average daily activity pattern?
```{r,echo=TRUE}
daily <- activity %>%
        filter(!is.na(steps)) %>%
        group_by(interval) %>%
        summarize(steps=mean(steps)) %>%
        print
```
##Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
```{r,echo=TRUE}
plot(daily, type = "l")
```
##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
```{r,echo=TRUE}
daily[which.max(daily$steps), ]$interval
```
##Imputing missing values
###Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r,echo=TRUE}
missing <- sum(is.na(activity))
```
##Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r,echo=TRUE}
new <- activity %>%
        group_by(interval) %>%
        mutate(steps = ifelse(is.na(steps), mean(steps, na.rm=TRUE), steps))
summary(new)
```
##Make a histogram of the total number of steps taken each day
```{r,echo=TRUE}
new.steps <- new %>%
  group_by(date) %>%
  summarize(steps = sum(steps)) %>%
  print      
```
#Histogram between date and steps
```{r,echo=TRUE}
ggplot(new.steps, aes(x=date, y=steps))+geom_histogram(stat="identity")+ xlab("Dates")+ ylab("Imputed Steps")+ labs(title= "Total numbers of Steps per day (missing data imputed)")
```
##Calculate and report the mean and median total number of steps taken per day.
```{r,echo=TRUE}
imputed.steps <- tapply(new$steps, new$date, FUN = sum, na.rm = TRUE)
new$date <- ymd(new$date)
mean(imputed.steps)
```
#Calculate the median
```{r,echo=TRUE}
median(imputed.steps)
```
##Do these values differ from the estimates from the first part of the assignment?
```{r,echo=TRUE}
mean(total.steps)==mean(imputed.steps)
```
##Do these values differ from the estimates from the first part of the assignment?
```{r,echo=TRUE}
median(total.steps)==median(imputed.steps)
```
###What is the impact of imputing missing data on the estimates of the total daily number of steps? The estimates of the number of steps increased by 41, 3041, 370, 1416, 0, 0.
```{r,echo=TRUE}
summary(imputed.steps) - summary(total.steps)
```
#Histogram
```{r,echo=TRUE}
par(mfrow=c(2,1))
hist(imputed.steps,col="red")
hist(total.steps,col="blue")
```
###Part 3
##Are there differences in activity patterns between weekdays and weekends?
###For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
```{r,echo=TRUE}
dayofweek <- function(date) {
    if (weekdays(as.Date(date)) %in% c("Saturday", "Sunday")) {
        "weekend"
    } else {
        "weekday"
    }
}
new$daytype <- as.factor(sapply(new$date, dayofweek))
```
###Make a panel plot containing a time series plot
```{r,echo=TRUE}
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    steps.type <- aggregate(steps ~ interval, data = new, subset = new$daytype == 
        type, FUN = mean)
    plot(steps.type, type = "l", main = type)
}
```
###Session info
```{r,echo=TRUE}
sessionInfo()
```


