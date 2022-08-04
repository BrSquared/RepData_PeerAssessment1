## Unzipping the data
zipped <- "activity.zip"
unzip(zipped)

## Read the data
df <- read.csv("activity.csv", colClasses = c("numeric","character","character"))

#### Creating a histogram of steps/day (with NAs)
TotpDay <- tapply(df$steps,df$date,sum)
hist(TotpDay, main = "Histogram of steps/day", xlab = "Total Steps")

avgsteps <- mean(TotpDay, na.rm = TRUE)
medsteps <- median(TotpDay,na.rm = TRUE)

#### Creating the time series plot 

## Formatting the interval into a time
df$interval <- sub('(\\d{2})$', ':\\1', df$interval)
for (i in 1:nrow(df)) {
    
    if (nchar(df$interval[i]) == 1) {
        df$interval[i] <- paste("0:0",df$interval[i], sep = "")
    } else if (nchar(df$interval[i]) == 3) {
        df$interval[i] <- paste("0",df$interval[i], sep = "")
    }
    
}
df$time <- as.POSIXct(df$interval, format = "%H:%M")

## Getting the average steps per interval
avgpint <- tapply(df$steps,df$interval,FUN = function(x) mean(x,na.rm = TRUE))
plot(x = df$time[1:288], y = avgpint, xlab = "Time of Day", ylab = "Avg Steps", 
     main = "Average steps throughout the day")

## Getting the max
which.max(avgpint)

#### Adding bac in NAs
nas <- sum(is.na(df$steps))

## Finding where the NAs occur and isolating it
napos <- which(is.na(df$steps))
dfnona <- df
for (j in napos) {
    
    dfnona$steps[j] <- avgpint[names(avgpint) == dfnona$interval[j]]
}

## Recreating the initial histogram with no nas
TotpDaynona <- tapply(dfnona$steps,dfnona$date,sum)
hist(TotpDaynona, main = "Histogram of steps/day", xlab = "Total Steps")

## These will be mostly the same because average values were used
avgstepsnona <- mean(TotpDaynona)
medstepsnona <- median(TotpDaynona)

#### Splitting the data between weekday and weekend
dfnona$date <- as.POSIXct(dfnona$date)
dfnona$Weekday <- weekdays(dfnona$date, abbreviate = TRUE)

dfnona$Weekdiff <- "Weekday"
dfnona$Weekdiff[dfnona$Weekday == "Sun" | dfnona$Weekday == "Sat"] <- "weekend"
dfnona$Weekdiff <- as.factor(dfnona$Weekdiff)
dfnona$weekdiffint <- as.factor(paste(dfnona$Weekdiff,dfnona$interval,sep = " "))

avgweekdiff <- tapply(dfnona$steps,dfnona$weekdiffint,mean)

## Making the plot
plotdata <- data.frame(steps = avgweekdiff, interval = rep(df$time[1:288],2), 
                       weekday = as.factor(c(rep("Weekday",288),rep("Weekend",288))))

g1 <- ggplot(data = plotdata, aes(x = interval, y = steps))
g1 <- g1 + facet_wrap(~ weekday, nrow = 2) + geom_line() + 
    ggtitle("Steps across Weekdays and Weekends") + scale_x_datetime(date_breaks = "2 hour", date_labels = "%H:00", expand = c(0, 0))
print(g1)
