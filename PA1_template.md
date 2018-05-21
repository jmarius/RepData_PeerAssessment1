Activity Monitoring
===================

1. Code for reading in the dataset and/or processing the data


```r
activity <- read.csv(file = "activity.csv", header = TRUE, sep = ",",
                     na.strings = "NA")
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

2. Histogram of the total number of steps taken each day

```r
totalsteps <- with(activity, tapply(steps, date, sum, na.rm = TRUE))
hist(totalsteps, col = "blue",
     main = "Histogram of the Total Number of Steps taken each day",
     ylab = "Frequency",
     xlab = "Steps per Day")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

3. Mean and median number of steps taken each day

```r
mean(totalsteps)
```

```
## [1] 9354.23
```

```r
median(totalsteps)
```

```
## [1] 10395
```

4. Time series plot of the average number of steps taken

```r
avgstps_intrvl <- with(activity, tapply(steps, interval, mean, na.rm = TRUE))
plot(rownames(avgstps_intrvl), avgstps_intrvl, type = "l", col = "red",
     main = "Average Daily Activity Pattern",
     ylab = "Average across all Days",
     xlab = "Average number of Steps Taken")
```

![plot of chunk Time Series Plot](figure/Time Series Plot-1.png)

5. The 5-minute interval that, on average, contains the maximum number of steps

```r
avgstps_intrvl[which.max(avgstps_intrvl)]
```

```
##      835 
## 206.1698
```

6. Code to describe and show a strategy for imputing missing data
   Strategy - Fill in missing values by inserting the average steps for each 
   5-minute interval

Step 1: Create two groups: with NAs and without NAs

```r
missing <- activity[is.na(activity$steps),]
complete <- activity[!is.na(activity$steps),]
```

Step 2: Create a dataframe with the average steps for each interval

```r
avgstps_intrvl2 <- data.frame(steps = avgstps_intrvl, interval = rownames(avgstps_intrvl))
```

Step 3: Left join the missing data and avgsteps per interval

```r
missing_cmplt <- merge(missing, avgstps_intrvl2, by = "interval", all.x = TRUE)
missing_cmplt2 <- data.frame(steps = as.integer(missing_cmplt$steps.y), 
                             date = missing_cmplt$date, 
                             interval = missing_cmplt$interval)
```

Step 4: combine datasets

```r
complete_set <- rbind(complete, missing_cmplt2)
complete_set <- complete_set[order(complete_set$date, complete_set$interval),]
complete_set <- complete_set[order(complete_set$date, complete_set$interval),]
```

7. Histogram of the total number of steps taken each day after missing values are imputed

```r
complete_sum <- with(complete_set, tapply(steps, date, sum))

hist(complete_sum, col = "orange",
     main = "Histogram of the Total Number of Steps taken each day",
     ylab = "Frequency",
     xlab = "Steps per Day")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png)

```r
mean(complete_sum)
```

```
## [1] 10749.77
```

```r
median(complete_sum)
```

```
## [1] 10641
```

8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
complete_set$week <- ifelse(weekdays(complete_set$date) %in% c("Saturday","Sunday"), "weekend", "weekday")

panel <- as.data.frame(with(complete_set, tapply(steps, list(interval, week), mean)))
panel$interval <- rownames(panel)
panel$interval <- as.integer(panel$interval)

library(tidyr)
panel_long <- gather(data = panel, value = steps, week, weekday:weekend, factor_key = TRUE)

library(ggplot2)
qplot(x = interval, y = steps, data = panel_long, geom = "line",facets = week ~.,
      xlab = "Interval", ylab = "Avg Number of Steps",
      main = "Avg Steps per Interval Based on Type of Day")
```

![plot of chunk Panel Plot](figure/Panel Plot-1.png)
9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report

