# Exploratory Data Analysis Assignment 1
## https://www.coursera.org/learn/exploratory-data-analysis/peer/ylVFo/course-project-1

### !!! NOTE NOTE NOTE !!! ###

input <- "household_power_consumption-TRIMMED.txt"

## The input file above is a pre-filtered version of the original source
## file. It is generated with the script trimDataSet.R, which is also
## in the repository. That script will copy over only the header and
## the 0.1% of rows we actually need for the assignment. This makes
## testing, development and ultimate execution MUCH faster.

### !!! NOTE NOTE NOTE !!! ###


## Read in the data - as mentioned above, row filtering is already done
data <- read.table(file = input, sep = ";", na.strings = "?",
                   header = TRUE,
                   colClasses = c(rep("character",2), rep("numeric", 7)))

## Make a new column that is fully-specified for both date and time:
data$DateTime <- strptime(paste(data$Date, data$Time), "%d/%m/%Y %H:%M:%S")


output <- "plot4.png"
png(output, width = 480, height = 480) # Open the png device

## Establish a 2x2 grid:
par( mfrow = c(2,2), cex = 0.7 )

with(data, {
    ## Generate first plot (Same as plot2.png)
    plot(x = DateTime, y = Global_active_power, type = 'l')
    
    ## Second plot
    plot(x = DateTime, y = Voltage, type = 'l', )

    ## Third plot (Same as plot3.png)
    plot(x = DateTime, y = Sub_metering_1, type = "n",
         ylab = "Energy Sub Metering")
    lines(x = DateTime, y = Sub_metering_1, col = "black")
    lines(x = DateTime, y = Sub_metering_2, col = "red")
    lines(x = DateTime, y = Sub_metering_3, col = "blue")
    legend("topright", col = subMeterColors, lty = 1,
           legend = c("Submeter 1", "Submeter 2", "Submeter 3"))

    ## Fourth Plot
    plot(x = DateTime, y = Global_reactive_power, type = 'l')
})


dev.off() # Close out the image

message(paste("PNG created:", output))

