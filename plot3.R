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

subMeterColors <- c("black", "red", "blue")
output <- "plot3.png"

### Doing it per Lecture 2.2 "Base Plot with Annotation"

png(output, width = 480, height = 480) # Open the png device

## Using SubMeter 1 for the "blank" frame, since it has the most
## extreme values. This seems like an awkward way to scale the Y axis
with(data, plot(x = DateTime, y = Sub_metering_1, type = "n",
                main = "Power Consumption\nBroken down by submetering",
                ylab = "Energy Sub Metering (Watt-hours)",
                xlab = "Time of Day (2007, Thu Feb 1 - Fri Feb 2)"))

## Use lines() to add each individual sub meter
with(data, {
    lines(x = DateTime, y = Sub_metering_1, col = "black")
    lines(x = DateTime, y = Sub_metering_2, col = "red")
    lines(x = DateTime, y = Sub_metering_3, col = "blue") } )

## Manually add a legend
legend("topright", col = subMeterColors, lty = 1,
       legend = c("Submeter 1", "Submeter 2", "Submeter 3"))

dev.off() # Close out the image

message(paste("PNG created:", output))

##########################################################################
### ALTERNATE:
### Doing this with ggplot2

library('ggplot2')

## Interesting way to generate a scale for 3 different plots:
## https://stackoverflow.com/a/10355844
plot <- ggplot( data = data ) +
    labs(x = "Time of Day (2007, Thu Feb 1 - Fri Feb 2)",
         y = "Energy Sub Metering (Watt-hours)",
         title = "Power Consumption\nBroken down by submetering") +
    geom_line( aes(x = DateTime, y = Sub_metering_1, color = "Submeter 1")) +
    geom_line( aes(x = DateTime, y = Sub_metering_2, color = "Submeter 2")) +
    geom_line( aes(x = DateTime, y = Sub_metering_3, color = "Submeter 3")) +
    scale_colour_manual("", 
                      breaks = c("Submeter 1", "Submeter 2", "Submeter 3"),
                      values = subMeterColors)

ggpf <- "plot3-ggplot.png"
ggsave(filename = ggpf, dpi = 120, width = 6, height = 4)

message(paste("GGPlot PNG created:", ggpf))

##########################################################################
### ALTERNATE:
### Using Melt to reshape the three columns into one variable
### Per suggestion https://stackoverflow.com/a/10349375

library(reshape2)
## The ID column needs to be a string
dtFmt <- "%y-%m-%d %H:%M:%S"
data$DtId <- format( data$DateTime, dtFmt)

sm <- c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
## Melt
bySm <- melt( data, id.vars = "DtId", measure.vars = sm )
## Add back a date-formatted column
bySm$DateTime <- strptime( bySm$DtId, dtFmt)

## Change 'variable' to something better
## https://stackoverflow.com/a/6081514
colnames(bySm)[2] <- "Submeter"

## I tried to do this with generic plot(), but it does not seem
## capable of coloring the lines by the Submeter. Instead, you have to
## call lines separately, which kind of defeats the purpose. So using
## ggplot again

plot <- ggplot( data = bySm ) +
    labs(x = "Time of Day (2007, Thu Feb 1 - Fri Feb 2)",
         y = "Energy Sub Metering (Watt-hours)",
         title = "Power Consumption\nBroken down by submetering") +
    geom_line( aes(x = DateTime, y = value, color = Submeter)) +
    scale_color_manual(values = subMeterColors) 

ggpm <- "plot3-melt.png"
ggsave(filename = ggpm, dpi = 120, width = 6, height = 4)
message(paste("Melted GGPlot PNG created:", ggpm))
