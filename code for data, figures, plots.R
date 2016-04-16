## Loading and preprocessing the data
data <- read.table(unz("activity.zip", "activity.csv"), nrows = 17568, header = TRUE, sep = ",", na.strings = "NA")

data$date <- as.Date(data$date)


## What is the mean total number of steps taken per day?
library(dplyr)

formatC(sum(is.na(data$steps)), format = "g", big.mark = ",")
sum(is.na(data$steps))
head(table(data$date, is.na(data$steps)), 10)

data      <- group_by(data, date)
dataDaily <- summarize(data, steps = sum(steps))

hist(dataDaily$steps,
     breaks = 25,
     ylim = c(0, 20),
     main = "Histogram of Daily Steps\n(null observations excluded at intveral level)",
     xlab = "Number of Daily Steps",
     ylab = "Daily Frequency")
abline(v = mean(dataDaily$steps, na.rm = TRUE),   col = "blue")
abline(v = median(dataDaily$steps, na.rm = TRUE), col = "red")

options(scipen = 999)
meanSteps   <- mean(dataDaily$steps, na.rm = TRUE)
medianSteps <- median(dataDaily$steps, na.rm = TRUE)

formatC(meanSteps, format = "f", big.mark = ",", digits = 2)
formatC(medianSteps, format = "f", big.mark = ",", digits = 0)


## What is the average daily activity pattern?
data         <- group_by(data, interval)
dataInterval <- summarize(data, meanSteps = mean(steps, na.rm = TRUE))

with(dataInterval,
     plot(x = interval,
          y = meanSteps,
          type = "l",
          main = "Time Series of Mean Steps by Time Interval\n(null observations excluded)",
          xlab = "Time", ylab = "Mean Steps",
          ylim = c(0, 250)))
abline(h = max(dataInterval$meanSteps, na.rm = TRUE), col = "purple")

maxInt <- dataInterval[which(dataInterval$meanSteps == max(dataInterval$meanSteps, na.rm = TRUE)), ]

as.numeric(maxInt[1])
round(maxInt[2], 2)

dataInterval[which(dataInterval$meanSteps == max(dataInterval$meanSteps, na.rm = TRUE)), ]


## Imputing missing values
formatC(sum(is.na(data$steps)), format = "g", big.mark = ",")

head(dataInterval)

data <- merge(data, dataInterval)
data <- mutate(data, stepsImputed = ifelse(is.na(steps), meanSteps, steps), meanSteps = NULL)
data <- arrange(data, date, interval)

data             <- group_by(data, date)
dataDailyImputed <- summarize(data, stepsImputed = sum(stepsImputed))

hist(dataDailyImputed$stepsImputed,
     breaks = 25,
     ylim = c(0, 20),
     main = "Histogram of Daily Steps\n(null observations imputed)",
     xlab = "Number of Daily Steps",
     ylab = "Daily Frequency")
abline(v = mean(dataDailyImputed$steps),   col = "blue")
abline(v = median(dataDailyImputed$steps), col = "red")

meanStepsImputed   <- mean(dataDailyImputed$steps)
medianStepsImputed <- median(dataDailyImputed$steps)

formatC(meanStepsImputed, format = "f", big.mark = ",", digits = 2)
formatC(medianStepsImputed, format = "f", big.mark = ",", digits = 2)


## Are there differences in activity patterns between weekdays and weekends?
data                <- mutate(data, dayGroup = ifelse(weekdays.Date(date) %in% c("Saturday", "Sunday"), "Weekend", "Weekday"))
data                <- group_by(data, interval)
dataIntervalWeekday <- summarize(filter(data, dayGroup == "Weekday"), meanStepsImputed = mean(stepsImputed))
dataIntervalWeekend <- summarize(filter(data, dayGroup == "Weekend"), meanStepsImputed = mean(stepsImputed))

par(mfcol = c(1, 2))

with(dataIntervalWeekday,
     plot(x = interval,
          y = meanStepsImputed,
          type = "l",
          main = "Mean Weekday Steps by Time\n(null observations imputed)",
          xlab = "Time", ylab = "Mean Steps",
          ylim = c(0, 250),
          xaxt = "n",
          yaxt = "n"))
axis(c(1), cex.axis = 0.80)
axis(c(2), cex.axis = 0.80)
abline(h = max(dataIntervalWeekday$meanStepsImputed, na.rm = TRUE), col = "purple")

with(dataIntervalWeekend,
     plot(x = interval,
          y = meanStepsImputed,
          type = "l",
          main = "Mean Weekend Steps by Time\n(null observations imputed)",
          xlab = "Time", ylab = "Mean Steps",
          ylim = c(0, 250),
          xaxt = "n",
          yaxt = "n"))
axis(c(1), cex.axis = 0.80)
axis(c(2), cex.axis = 0.80)
abline(h = max(dataIntervalWeekend$meanStepsImputed, na.rm = TRUE), col = "hotpink")

maxIntWeekday <- max(dataIntervalWeekday$meanStepsImputed)
maxIntWeekend <- max(dataIntervalWeekend$meanStepsImputed)

round(maxIntWeekday, 2)
round(maxIntWeekend, 2)

dataIntervalWeekday[which(dataIntervalWeekday$meanStepsImputed == maxIntWeekday), 1]
dataIntervalWeekend[which(dataIntervalWeekend$meanStepsImputed == maxIntWeekend), 1]
