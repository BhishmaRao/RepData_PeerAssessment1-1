---
output: html_document
---
##Reproducible Research: PeerAssessment_1

##Loading and preprocessing the data
```{r}
act <- read.csv("activity.csv", colClasses = c("numeric", "character", "numeric"))
head(act)
tail(act)
names(act)

library(lattice)
act$date <- as.Date(act$date, "%Y-%m-%d")
```
# What is mean total number of steps taken per day?
```{r}
TotalSteps <- aggregate(steps ~ date, data = act, sum, na.rm = TRUE)
hist(TotalSteps$steps, main = "TotalSteps/day", xlab = "Day", col = "red")
mean(TotalSteps$steps)
median(TotalSteps$steps)

```
```{r}
steps <- rep(NA, 61)
day <- rep("NA", 61)
steps_day <- tapply(act$steps, act$date, sum, na.rm = T)
length(steps_day)
```
```{r}
for (i in 1:length(steps_day)) {
  steps[i] <- steps_day[[i]]
  day[i] <- names(steps_day)[i]
}

day_n_steps<- data.frame(day, steps)
head(day_n_steps)
```
```{r}
hist(day_n_steps$steps, main = "TotalSteps/day", xlab = "Day", col = "green")
```
## What is the average daily activity pattern?
```{r}
TimeSeries <- tapply(act$steps, act$interval, mean, na.rm = TRUE)
plot(row.names(TimeSeries), TimeSeries, type = "l", xlab = "5 minute interval", 
     ylab = "Average across all Days", main = "Average number of steps taken", 
     col = "red")
```
```{r}
MaxInterval <- which.max(TimeSeries)
names(MaxInterval)
```
## Imputing missing values
```{r, echo=FALSE}
act_NA <- sum(is.na(act))
act_NA
```
```{r}
AverageSteps <- aggregate(steps ~ interval, data = act, FUN = mean)
fill_NA <- numeric()
for (i in 1:nrow(act)) {
  obsn <- act[i, ]
  if (is.na(obsn$steps)) {
    steps <- subset(AverageSteps, interval == obsn$interval)$steps
  } else {
    steps <- obsn$steps
  }
  fill_NA <- c(fill_NA, steps)
}

act_new <- act
act_new$steps <- fill_NA
```
```{r}
TotalSteps2 <- aggregate(steps ~ date, data = act_new, sum, na.rm = TRUE)

hist(TotalSteps2$steps, main = "Total steps/day", xlab = "Day", col = "red")
mean(TotalSteps2$steps)
median(TotalSteps2$steps)
```
## Are there differences in activity patterns between weekdays and weekends?
```{r}
day <- weekdays(act$date)
daylevel <- vector()
for (i in 1:nrow(act)) {
  if (day[i] == "Saturday") {
    daylevel[i] <- "Weekend"
  } else if (day[i] == "Sunday") {
    daylevel[i] <- "Weekend"
  } else {
    daylevel[i] <- "Weekday"
  }
}
act$daylevel <- daylevel
act$daylevel <- factor(act$daylevel)
```
```{r}
stepsByDay <- aggregate(steps ~ interval + daylevel, data = act, mean)
names(stepsByDay) <- c("interval", "daylevel", "steps")
```
```{r}
xyplot(steps ~ interval | daylevel, stepsByDay, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number_of_steps")
```
