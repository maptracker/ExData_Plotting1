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


output <- "plot1.png"
png(output, width = 480, height = 480) # Open the png device

## Generate the histogram
hist( data$Global_active_power,
     main = "Global Active Power",
     xlab = "Global Active Power (kilowatts)",
     ylab = "Frequency",
     col = "red")

dev.off() # Close out the image

message(paste("PNG created:", output))

