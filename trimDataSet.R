## Exploratory Data Analysis Assignment 1
## https://www.coursera.org/learn/exploratory-data-analysis/peer/ylVFo/course-project-1

## The assignment uses a very large file as input, but only requires a
## small subset of the rows for analysis. This script will trim the
## input to a smaller file

input  <- "household_power_consumption.txt"
output <- "household_power_consumption-TRIMMED.txt"

if (file.exists(output)) {
    message(paste("Trimmed file already exists:", output))
    stop("If you need to recalculate, please delete or rename it")
}

## For memory efficiency, we will read one line at a time and keep the
## ones we want. For compatibility with the untrimmed input, we will
## not alter the goofy semicolon-separated format.

minDate <- as.Date("2007-02-01", format = "%Y-%m-%d")
maxDate <- as.Date("2007-02-02", format = "%Y-%m-%d")

message(paste("Generating trimmed data file from:", input))
message(paste("Keeping only lines between",minDate,"and",maxDate))

## Line-by-line example : https://stackoverflow.com/a/4106976

inCon  <- file( input, open = "r" )
outCon <- file( output, open = "w" )

header <- FALSE
total <- 0
kept <- 0

while (length(line <- readLines(inCon, n = 1, warn = FALSE)) > 0) {
    if (!header) {
        ## Always take the first line
        header <- TRUE
        writeLines(line, con = outCon);
        next
    }
    total <- total + 1
    ## Now see if we want the line by checking the date
    row <- strsplit(line, fixed = TRUE, split = ';')
    day <- as.Date(row[[1]], format = "%d/%m/%Y")
    if (day < minDate || day > maxDate) next
    ## data falls within our date range, write it to the trimmed file
    writeLines(line, con = outCon);
    kept <- kept + 1
}

close(inCon)
close(outCon)

message(paste("Trimmed file created:", output))
message(paste(total, "rows reduced to", kept))

