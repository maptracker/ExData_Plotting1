## Coursera assignment
## https://www.coursera.org/learn/exploratory-data-analysis/peer/b5Ecl/course-project-2

Question <- 2
## Have total emissions from PM2.5 decreased in the Baltimore City,
## Maryland (fips == "24510") from 1999 to 2008? Use the base plotting
## system to make a plot answering this question.

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

uniqueYears <- unique(NEI$year) 

## Get a list of all SCC codes that have Lvl4 == "Total"

TotalSCCs <- as.character(SCC[ SCC$SCC.Level.Four == 'Total',]$SCC)

## Build a frame to hold our data
TotalByYear <- data.frame( Year = uniqueYears )
TotalByYear$PM2.5 <- vapply( TotalByYear$Year, function (yr) {
  sum(NEI[NEI$SCC %in% TotalSCCs &
          NEI$fips == "24510" &
          NEI$year == yr,
          "Emissions"], na.rm = TRUE)
}, 0)

## Hm. This seems unlikely
TotalByYear[TotalByYear$Year %in% c(2002, 2005), "PM2.5"]
## [1] 102.46 102.46

plot(x = TotalByYear$Year, y = TotalByYear$PM2.5, type = 'b',
     xlab = "NEI Year Reported", ylab = "PM2.5 (tons)",
     main = "PM2.5 decreased, then increased, in Baltimore",
     sub = expression(italic("2002 exactly equals 2005 --> Suspect data!")))

savePlot(sprintf("plot%d.png",Question))

