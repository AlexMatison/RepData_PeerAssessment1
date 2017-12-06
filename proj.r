library(dplyr)
# Loading and preprocessing the data
# need to download data, unzip
fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
localFileName <- "activity.zip"
download.file(fileURL, localFileName)
unzip(zipfile = localFileName)
activityData <- read.csv("activity.csv", nrows=3000)

activityData$date <- as.Date(activityData$date)

# What is mean total number of steps taken per day?

dailySummary <- activityData %>%
    group_by(date) %>%
    summarise(dailyTotalSteps=sum(steps))


# clean up labels
hist(dailySummary$dailyTotalSteps, breaks = 10)

meanStepsPerDay <- mean(dailySummary$dailyTotalSteps, na.rm = TRUE)

medianStepsPerDay <- median(dailySummary$dailyTotalSteps, na.rm = TRUE)

# What is the average daily activity pattern?
fiveMinuteSummary <- activityData %>%
    group_by(interval) %>%
    summarise(intervalMeanSteps=mean(steps, na.rm = TRUE))

# fix labels
plot(x = fiveMinuteSummary$interval, y = fiveMinuteSummary$intervalMeanSteps, type = "l")

# five minute interval with the maximum mean steps per day.
fiveMinuteSummary[which.max(fiveMinuteSummary$intervalMeanSteps),]

# Imputing missing values
# Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(activityData$steps))

# Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
# Substitute the NA with the mean number of steps for that interval across all days.

# Create a new dataset that is equal to the original dataset but with the missing data filled in.
activityDataWithImputededNAs <- activityData
naSteps <- activityDataWithImputededNAs[is.na(activityDataWithImputededNAs$steps),]
activityDataWithImputededNAs[is.na(activityDataWithImputededNAs$steps),]$steps <- lapply(naSteps$interval, function(x) fiveMinuteSummary$intervalMeanSteps[match(x, fiveMinuteSummary$interval)])

# Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
dailySummaryImputedNAs <- activityDataWithImputededNAs %>%
    group_by(date) %>%
    summarise(dailyTotalSteps=sum(steps))
# clean up labels
hist(dailySummaryImputedNAs$dailyTotalSteps, breaks = 10)