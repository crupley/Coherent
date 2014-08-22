
#date <- readline("Input data date (YYYYMMDD)")
#fileNumber <- readline("Input data number (##) ")

#setwd(paste("C:/Users/rupleyc/Documents/data/", year, "/", month,sep=""))

cp <- data.frame(                       #control parameters
           dataStartDate=20140715,      #YYYMMDD
           dataNum="03",                #2-digit sequential data identifier, pad single digits with leading zero
           collectionStartTime="16:00", #Time the data collection was begun, 24h
           sampleIncrement=1,           #Delay between samples in seconds
           processingStartTime="18:00", #Time where calculation and processing begins, 24h
           hours=8)                     #Length of data to be processed

year=floor(cp$dataStartDate/10000)
month=floor((cp$dataStartDate-10000*year)/100)


if (month < 10) {
    month <- paste("0", as.character(month), sep="")
} else {
    month <- as.character(month)
}


dataName = paste(cp$dataStartDate, cp$dataNum)

filename=paste("C:/Users/rupleyc/Documents/data/",year,"/",month, "/",dataName, ".csv", sep="")


# convert to time and date formats
cp$dataStartDate <- as.Date(as.character(cp$dataStartDate), format="%Y%m%d")
cp$collectionStartTime <- as.POSIXct(paste(cp$dataStartDate, cp$collectionStartTime,sep=" "), format="%Y-%m-%d %H:%M")
cp$processingStartTime <- as.POSIXct(paste(cp$dataStartDate, cp$processingStartTime,sep=" "), format="%Y-%m-%d %H:%M")

header <- read.csv(filename, nrows=3, header = FALSE)
dataImport <- read.csv(filename, header = TRUE, skip=3)
stabData <- dataImport$Measurement

eTime <- seq(cp$collectionStartTime, by=cp$sampleIncrement, along.with=stabData)
window <- (eTime >= cp$processingStartTime & eTime <= cp$processingStartTime + cp$hours * 60 * 60 / cp$sampleIncrement)

##Output Parameter calculations
#calculated on the full data set
powerMeanF <- mean(stabData)
powerSTDevF <- sd(stabData)
RMSpowerDevF <- powerSTDevF / powerMeanF

#calculated on the selected window
powerMeanW <- mean(stabData[window])
powerSTDevW <- sd(stabData[window])
RMSpowerDevW <- powerSTDevW / powerMeanW

fit <- lm(stabData ~ eTime)  #linear fit to full stability data

#additional features to add
#lowest RMS interval, 8 and 4 hours

#Plotting

oldpar <- par()

#Power vs. time plot
oldpar <- par(mfrow=c(3,2))
plot(eTime, stabData, xlab="Time", ylab="Power, W", main="Power Stability, Full Data", sub=dataName)
abline(fit, col="blue")

#Power vs. time selected window
plot(eTime[window], stabData[window], xlab="Time", ylab="Power, W", main="Power Stability, Selection", sub=dataName)
abline(h=powerMeanW, col="blue")
abline(h=powerMeanW + powerSTDev, col="blue", lty="dashed")
abline(h=powerMeanW - powerSTDev, col="blue", lty="dashed")

#Power value density plot
den <- density(stabData)
plot(den, xlab="Power, W", main="Power Stability Density Function", sub=dataName)

#Power value density plot, windowed
den <- density(stabData[window])
plot(den, xlab="Power, W", main="Power Stability Density Function", sub=dataName)

#Q-Q plot
qqnorm(stabData, sub=paste("Power Stability", dataName))
qqline(stabData)

#Q-Q plot, windowed
qqnorm(stabData[window], sub=paste("Power Stability", dataName))
qqline(stabData[window])

par(oldpar)



# Print results to console

duration <- length(eTime) * cp$sampleIncrement #in seconds
hours <- floor(duration/3600)
minutes <- floor((duration - hours*3600)/60)
durationStr <- if (minutes < 10) paste(hours,":0",minutes, sep="") else paste(hours,":",minutes, sep="")


durationW <- length(eTime[window]) * cp$sampleIncrement #in seconds
hours <- floor(durationW/3600)
minutes <- floor((durationW - hours*3600)/60)
durationStrW <- if (minutes < 10) paste(hours,":0",minutes, sep="") else paste(hours,":",minutes, sep="")



#Attempt 1
# sprintf("Mean = %2.2f W", powerMean)
# sprintf("Standard Deviation = %2.4f W", powerSTDev)
# sprintf("RMS = %1.2f%%", RMSpowerDev*100)
# sprintf("Duration = %s", durationStr)

#attempt 2
# cat("Mean = ", format(powerMean, digits=4), "W\n")
# cat("Standard Deviation = ", format(powerSTDev, digits=4), "W\n")
# cat("RMS = ", format(100*RMSpowerDev, digits=2), "%\n")
# cat("Duration = \t", durationStr, "\n")

#attempt 3
x <- data.frame(Parameters=c("Mean", "Standard Deviation", "RMS", "Duration"), 
                "Full Data"=c(paste(format(powerMeanF, digits=4), "W"), 
                         paste(format(powerSTDevF*1000, digits=4), "mW"), 
                         paste(format(100*RMSpowerDevF, digits=2),"%",sep=""), 
                         durationStr),
                Windowed=c(paste(format(powerMeanW, digits=4), "W"), 
                         paste(format(powerSTDevW*1000, digits=4), "mW"), 
                         paste(format(100*RMSpowerDevW, digits=2),"%",sep=""), 
                         durationStrW))
print(x)