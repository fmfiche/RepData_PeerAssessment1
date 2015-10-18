#Reproducible Research - Peer assessement 1

##Written by Ferdinando Fiche


This is a report about data collect from a personal activity monitoring device.

##Loading and processing data


```r
#loading data
if(!file.exists("repdata_data_activity.zip")) {
        temp <- tempfile()
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        file <- unzip(temp)
        unlink(temp)
}
activity <- read.table(file, header=T, sep=",")

#processing data
activity$date <- as.Date(activity$date)
```

##What is mean total number of steps taken per day?

```r
#aggregating steps by day
steps_by_day <- aggregate(x = activity$steps, by = list(activity$date), FUN = sum, na.rm = TRUE)

#giving name to columns
names(steps_by_day) <- c("date", "steps")

#plotting the histogram
hist(x = steps_by_day$steps, col = "grey", breaks = 10, xlab = "Total number of steps", main = "Histogram of steps by day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
#calculating mean and median
mean_dailysteps <- mean(steps_by_day$steps)
median_dailysteps <- median(steps_by_day$steps)
```

The mean steps taken by day is 9354.2295082 and the median is 10395.

##What is the average daily activity pattern?


```r
#aggregating steps by interval
steps_by_interval <- aggregate(x = activity$steps, by = list(activity$interval), FUN = mean, na.rm = TRUE)

#giving name to columns
names(steps_by_interval) <- c("interval", "mean_steps")

#plotting graph
plot(steps_by_interval$interval, steps_by_interval$mean_steps, type = "l", main = "Mean Steps by Interval", xlab = "Interval", ylab = "steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
#finding with 5 minute interval has more number of steps
max_interval_steps <- steps_by_interval[which.max(steps_by_interval$mean_steps),c("interval")]
```

The 5-min interval with more steps is 835.

##Imputing missing values


```r
#calculing the total missing values of steps
sum(is.na(activity$steps))
```

```
## [1] 2304
```

```r
#merging data
merged_data <- merge(x = activity, y = steps_by_interval, by = "interval", all.x = TRUE)

#creating a function to replace NA values
replace.na <- function(x,y) {
    if(is.na(x)) {
        return(y)
    }
    return(x)
}

#creating a new dataset that is equal to the original dataset but with the missing data filled in
modificated_activity <- data.frame(merged_data$date, merged_data$interval, new_steps = mapply(replace.na, merged_data$steps, merged_data$mean_steps))

#changing columns names
colnames(modificated_activity) <- c("date", "interval", "steps")

#aggregating steps by day (new data)
steps_by_day_new <- aggregate(x = modificated_activity$steps, by = list(modificated_activity$date), FUN = sum, na.rm = TRUE)

#giving name to columns
names(steps_by_day_new) <- c("date", "steps")

#plotting the histogram
hist(x = steps_by_day_new$steps, col = "grey", breaks = 10, xlab = "Total number of steps", main = "Histogram of steps by day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
#calculating mean and median
mean(steps_by_day_new$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(steps_by_day_new$steps, na.rm = TRUE)
```

```
## [1] 10766.19
```

After replacing NA values the mean and median is so close to previously results as waited.

##Are there differences in activity patterns between weekdays and weekends?


```r
#adding day name
modificated_activity$day = weekdays(as.Date(modificated_activity$date))

#adding a fucntion to classify type of day
daytype <- function(x){
  if(x %in% c("sábado","domingo")){
    return('Weekend') #weekday written in portuguese
  }
  return('weekday')
}

#adding a factor daytype
modificated_activity$daytype = as.factor(apply(as.matrix(modificated_activity$day), 1, daytype))

#plotting graph
library("lattice")
xyplot(steps~interval | daytype, data = modificated_activity, type = "l", main = "Mean Steps by Interval", xlab = "Interval", ylab = "steps", layout = c(1,2))
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
