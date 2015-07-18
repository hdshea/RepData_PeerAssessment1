# Loading and preprocessing the data
library(dplyr, warn=F)
library(tidyr)
csvFName <- unzip("activity.zip",list = T)[1,"Name"] # determine internal filename
unzip("activity.zip",file=csvFName) # unzip data
activity <- tbl_df(read.csv(csvFName,stringsAsFactors = F)) %>%
    mutate(date=as.Date(date, format="%Y-%m-%d"))
rm(csvFName)

# What is mean total number of steps taken per day?
library(ggplot2)
byDay <- activity %>%
    group_by(date) %>%
    summarise(steps = sum(steps, na.rm = T))

ggplot(byDay, aes( steps )) +
    geom_histogram(binwidth = max(byDay$steps)/20)

dSummary <- summarise(byDay, Average = mean(steps), Median = median(steps))
dSummary

# What is the average daily activity pattern?
byInterval <- activity %>%
    group_by(interval) %>%
    summarise(steps = mean(steps, na.rm = T))

ggplot(byInterval, aes(interval, steps)) +
    geom_line()

filter( byInterval, steps == max(byInterval$steps))

# Imputing missing values
sum(is.na(activity$steps))

activityAdj <- left_join(activity, byInterval, by="interval")
activityAdj <- bind_rows(
    filter(activityAdj, is.na(steps.x))  %>% select(steps=steps.y, date, interval),
    filter(activityAdj, !is.na(steps.x)) %>% select(steps=steps.x, date, interval))

byDayAdj <- activityAdj %>%
    group_by(date) %>%
    summarise(steps = sum(steps))

ggplot(byDayAdj, aes( steps )) +
    geom_histogram(binwidth = max(byDayAdj$steps)/20)

dSummaryAdj <-summarise(byDayAdj, Average = mean(steps), Median = median(steps))
dSummaryAdj

# Are there differences in activity patterns between weekdays and weekends?
activityAdj <- activityAdj %>%
    mutate(dtype = factor((weekdays(date)=="Saturday") | (weekdays(date)=="Sunday"),
                           labels=c("weekday","weekend")))

byIntervalAdj <- activityAdj %>%
    group_by(dtype,interval) %>%
    summarise(steps = mean(steps, na.rm = T))

ggplot(byIntervalAdj, aes(interval, steps)) +
    geom_line() +
    facet_wrap(~dtype, ncol=2)
    
