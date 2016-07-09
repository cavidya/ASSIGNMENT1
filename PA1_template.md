Loading and preprocessing the data
-----------------------------------
```{r}
library(plyr)
library(ggplot2)
activity <- read.csv("activity.csv") # read in the data
activity$date <- as.POSIXct(activity$date) # set the dates to POSIXct
```
What is mean total number of steps taken per day?
--------------------------------------------------

Calculate the total number of steps taken per day
--------------------------------------------------
```{r}
dailysteps <- aggregate(activity$steps, by = list(activity$date), sum, na.rm=TRUE)
names(dailysteps) <- c("Date", "steps")
```

Make a histogram of the total number of steps taken each day
-------------------------------------------------------------
```{r}
qplot(steps, data = dailysteps, geom="histogram", xlab = "Daily Number of Steps", binwidth = 300)
```

```{r}
mean.steps <- mean(dailysteps$steps) 
median.steps <- median(dailysteps$steps)
```

The mean number of steps each day is 9354
The median number of steps each day is 10395

What is the average daily activity pattern?
--------------------------------------------

##df of the mean and median number of steps taken, averaged across all days (y-axis)
```{r}
intsteps <- aggregate(activity$steps, by = list(activity$interval), mean, na.rm=TRUE)
intstepsmed <- aggregate(activity$steps, by = list(activity$interval), median, na.rm=TRUE)
intsteps <- cbind(intsteps[], intstepsmed$x)
```

##Tidy the df names and round the numbers
```{r}
names(intsteps) = c("interval","mean.steps", "median.steps")
intsteps$mean.steps <- round(intsteps$mean.steps)
intsteps$median.steps <- round(intsteps$median.steps)
ggplot(intsteps, aes(x = interval, y = mean.steps)) + geom_line()
```

##The 5-minute interval that on average across all the days in the dataset containing the maximum number of steps

```{r}
most.steps <- intsteps$interval[intsteps$mean.steps == max(intsteps$mean.steps)]
```

The interval with the most steps each day (on average is) : 835

Imputing missing values
-------------------------

##Find the NAs
```{r}
na.steps <- subset(activity, is.na(steps))
num.NAs <-length(na.steps$steps)
```

##Replace the NAs with the median number of steps for that period

```{r}
nstps <- data.frame(date=activity$date[is.na(activity$steps)], interval = activity$interval[is.na(activity$steps)], steps=intsteps[match(intsteps$interval, activity$interval[is.na(activity$steps)]),3])
```

##A new dataset that is equal to the original dataset but with the missing data filled in

###Remove the NA's from the period
```{r}
activity <- subset(activity, !is.na(steps))
```

###Append the median steps to the Activity DF
```{r}
activity <- rbind(activity, nstps)
```

###sum the number of steps each day into the dailysteps2 DF and get the mean and median 
```{r}
dailysteps2 <- aggregate(activity$steps, by = list(activity$date), sum, na.rm=TRUE)
names(dailysteps2) <- c("Date", "steps")
```

##Histogram of the total number of steps taken each day and the mean and median total number of steps taken per day.
```{r}
qplot(steps, data = dailysteps2, geom="histogram", xlab = "Daily Number of Steps", binwidth = 300)
```
```{r}
mean.steps2 <- mean(dailysteps2$steps) # 
median.steps2 <- median(dailysteps2$steps)
```
There are 2304 intervals with NA

THe new mean number of steps is 9504, this is close to the mean from the data with NAs of 9354. THe new median number of steps is 10395 this is the same as the median from the data with NAs of 10395. There is little impact to the estimated number of steps a day from using the median for the time interval to replace the missing data. I had previously used the mean but this introduced a sizeable difference.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------
## Add the Weekday/weekend identifier

```{r}
activity$week <- ifelse(weekdays(activity$date) == "Saturday" | weekdays(activity$date) == "Sunday" ,"weekend","weekday")
```

##df of the mean and median number of steps taken, averaged across all days (y-axis)
```{r}
intsteps2 <- aggregate(activity$steps, by = list(activity$week, activity$interval), mean, na.rm=TRUE)
intstepsmed2 <- aggregate(activity$steps, by = list(activity$week, activity$interval), median, na.rm=TRUE)
intsteps2 <- cbind(intsteps2[], intstepsmed2$x)
```

##Tidy the df names and round the numbers
```{r}
names(intsteps2) = c("weekday", "interval","mean.steps", "median.steps")
intsteps2$mean.steps <- round(intsteps2$mean.steps)
intsteps2$median.steps <- round(intsteps2$median.steps)
```

```{r}
ggplot(intsteps2, aes(x = interval, y = mean.steps)) + ylab("Number of Steps") + geom_line() + facet_grid(weekday~.)
```

As can be seen from the plots above the user appears to start walking around later at weekends (maybe they sleep later). Once they are awake they tend to take more steps and to be walking around (awake?) later in the day.
