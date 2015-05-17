setwd("~/Data_Science/Coursera/Data_Science_Specialization-John_Hopkins_University/Reproducible_Research/R/RepData_PeerAssessment1/")

activity <- read.csv("activity.csv", header = T)
head(activity)
dim(activity)

library(dplyr)

date_activity <- activity %>% 
  group_by(date) %>% 
  summarise(No_of_steps = sum(steps))

png("plot1.png")
hist(date_activity$No_of_steps, 
     breaks = 25, 
     col = "grey", 
     main = "Number of Steps taken per Day",
     xlab = "Number of Steps taken per Day")

mean_no_of_steps <- mean(date_activity$No_of_steps, na.rm = T)
median_no_of_steps <- median(date_activity$No_of_steps, na.rm = T)

abline(v = mean_no_of_steps, col = "green", lwd = 2)
abline(v = median_no_of_steps, col = "blue", lwd = 2)

ab_legend <- c(paste("mean = ", round(mean_no_of_steps), collapse = ""), 
               paste("median = ",round(median_no_of_steps), collapse = ""))
legend("topright", 
       lwd = 3, 
       lty = 1, 
       legend = ab_legend, 
       col = c("green","blue"),
       cex = .8
       )
dev.off()
###################

intreval_activity <- activity %>% 
  group_by(interval) %>% 
  summarise(avg_no_of_steps = mean(steps, na.rm = T))

dim(intreval_activity)
head(intreval_activity)

png("plot2.png")
plot(x = intreval_activity$interval, 
     y = intreval_activity$avg_no_of_steps,   
     type = "l", 
     xlab = "Interval", 
     ylab = "Average Number of Steps Taken",
     main = "Average Number of Steps Taken by Interval", 
     )

max_number_steps  <-  max(intreval_activity$avg_no_of_steps)
interval_max_steps <- intreval_activity[
  which(intreval_activity$avg_no_of_steps == max_number_steps),"interval"]
abline(v = interval_max_steps, col = "green", lwd = 3)

dev.off()
###################

sum(is.na(activity$steps))
sum(is.na(activity$date))
sum(is.na(activity$interval))

###################

tmp_1 <- merge(activity[is.na(activity$steps),], intreval_activity, by = "interval")
activity[is.na(activity$steps),"steps"] <-  tmp_1[,"avg_no_of_steps"]

date_activity <- activity %>% 
  group_by(date) %>% 
  summarise(No_of_steps = sum(steps))

png("plot3.png")
hist(date_activity$No_of_steps, 
     breaks = 25, 
     col = "grey", 
     main = "Number of Steps taken per Day",
     xlab = "Number of Steps taken per Day")

mean_no_of_steps <- mean(date_activity$No_of_steps, na.rm = T)
median_no_of_steps <- median(date_activity$No_of_steps, na.rm = T)

abline(v = mean_no_of_steps, col = "green", lwd = 2)
abline(v = median_no_of_steps, col = "blue", lwd = 2)

ab_legend <- c(paste("mean = ", round(mean_no_of_steps), collapse = ""), 
               paste("median = ",round(median_no_of_steps), collapse = ""))
legend("topright", 
       lwd = 3, 
       lty = 1, 
       legend = ab_legend, 
       col = c("green","blue"),
       cex = .8
)

dev.off()

############

activity$date <- strptime(x = activity$date, "%Y-%m-%d")
head(activity)

activity$weekday <- weekdays(activity$date)

weekend <- function(x) {
  ifelse(any(x == c("Sunday","Saturday")), 
         return("Weekend"),
         return("Weekday"))
}

tmp <- sapply(activity$weekday, weekend)
activity$daytype <- tmp

intreval_activity_weekend <- activity[which(activity$daytype=="Weekend"),-2] %>% 
  group_by(interval) %>% 
  summarise(avg_no_of_steps = mean(steps, na.rm = T))


intreval_activity_weekday <- activity[which(activity$daytype=="Weekday"),-2] %>% 
  group_by(interval) %>% 
  summarise(avg_no_of_steps = mean(steps, na.rm = T))

png("plot4.png")
par(mfrow = c(2, 1))

plot(intreval_activity_weekday,
     type = "l", 
     main = "Weekday", 
     ylab = "Average Number of Steps")
plot(intreval_activity_weekend, 
     type = "l", 
     main = "Weekend",
     ylab = "Average Number of Steps")

dev.off()