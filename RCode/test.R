setwd("C:/GitHub/Repo/Coursera/Reproduce-Research/RepData_PeerAssessment1/RepData_PeerAssessment1/RCode")
library(data.table)

df <- read.csv("../Data/Activity.csv")
dt <- data.table(df, key="date")
dt[,"asDate"] <- as.Date(dt[,date])
dt[,"wday"] <- weekdays(dt$asDate)
head(dt)

noNA <- na.omit(dt)
noNA <- data.table(noNA, key="date,interval")
dailySteps <- noNA [,sum(steps), by=key(noNA)]
setnames(dailySteps, "V1", "steps")
head(dailySteps)

breaks <- c(0,2500,5000,7500,10000,12500,15000,17500,20000,22500)
cuts <- cut(dailySteps$steps, breaks,
            labels=c("2500", "5000", "7500", "10000", "12500", "15000",
                     "17500",  "20000", "22500"))
summary(cuts)

hist(dailySteps$steps, col="green",
     breaks=breaks,
     main="Daily Steps Histogram Excluding NAs", xlab="daily steps")
rug(dailySteps$steps)
abline(v=median(dailySteps$steps), lwd=4, col="blue")
cat("Total Observations:", nrow(noNA))


