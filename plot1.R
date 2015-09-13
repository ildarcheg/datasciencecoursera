## plot1
## This function reads data from household_power_consumption.txt from work directory,
## converts gotten data frame to tbl, adds column DataTime to tbl, uses data 
## only from the dates 2007-02-01 and 2007-02-02, makes plot and saves it in PNG
## in working directory.
## To avoid repeated reading data ftom the file call function with reload = TRUE
ShowPlot <- function(reload = TRUE) {
    
        plotName = "plot1";
        
        # read the data from TXT if mytGlobal does not exist or on request 
        if (!exists("mytGlobal") | reload) {
                message("the data is loading...")
                ## read the data
                colClasses <- c(rep("character", 2), rep("numeric", 7))
                tempDataFrame <- read.csv("household_power_consumption.txt", sep = ";", colClasses = colClasses, na.strings = "?")
                ## convert to tbl
                library("dplyr")
                temptbl <- as.tbl(tempDataFrame)
                ## add column DataTime
                temptbl <- mutate(temptbl, DateTime = as.POSIXct(paste(Date, Time), format = "%d/%m/%Y %H:%M:%S"))
                startTime <- as.POSIXct("2007-02-01 00:00:00")
                endTime <- as.POSIXct("2007-02-03 00:00:00")
                ## leave the dates 2007-02-01 and 2007-02-02
                temptbl <- filter(temptbl, DateTime >= startTime, DateTime < endTime)
                ## rearrange columns
                myt <- select(temptbl, c(10, 3:9))
                ## store myt in mytGlobal in global environment
                mytGlobal <<- myt
                message("the data is loaded in tbl. the data is stored in mytGlobal.")
        } else {
                ## get myt from mytGlobal from global environment
                myt <- mytGlobal
                message("the data was gotten from mytGlobal.")
        }
        
        ## use myt as the data for making plot
        ## Create plot on screen device
        par(mfcol = c(1,1))
        hist(myt$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
        ## Copy my plot to a PNG file
        fileName = paste(plotName, ".png", sep = "")
        dev.copy(png, file = fileName, height=480, width=480)
        dev.off()
}
