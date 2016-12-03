require(digest)
if(!exists("loadAllDataSets", mode = "function")){
  source('loadData.R')
}

# load all data
allUsage <- loadAllDataSets('data/2016/')
View(allUsage)

# extract messages only
msgs <- allUsage[allUsage$Type == 'Text', c("Date", "Timestamp", "Number")]

#View(msgs)

countsPerDay <- aggregate(msgs$Number ~ msgs$Date, msgs, function(x) length(x))

names(countsPerDay) <- c("Day", "NumMsgs")

countsPerDay$Colour <- "lightgray"
countsPerDay$Colour[countsPerDay$NumMsgs > 100] <- "orange"

# plot
plot(countsPerDay$NumMsgs, bty="n",
     lty=1, lwd=1, type="l", col="blue",
     xlab = "Days from start of year",
     ylab="",
     main = "Outgoing messages per day\n 2016",
     xlim = c(0,365), ylim=c(0, 250)
)
abline(h = 100, col="red", lty=8)
text(x=195,y=240, "235 (max value)", col = "red")

