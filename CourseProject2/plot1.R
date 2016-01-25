


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## Sanity checks:
chkOnePollutant <- unique(NEI$Pollutant) # Yes, only PM-25
uniqueYears <- unique(NEI$year) # Ah. That's why we have only 4 years

## This is a really wacky data set. It's unclear how redundant the
## Lvl4 "Total" columns are. It's unclear if we should consider `==
## Total` or `grep("^Total")`.

## To be honest, I'm not too interested in the minutiae of PM2.5 data,
## and would rather focus on getting some decent plots.

## Get a list of all SCC codes that have Lvl4 == "Total"

TotalSCCs <- as.character(SCC[ SCC$SCC.Level.Four == 'Total',]$SCC)

## Build a frame to hold our data
TotalByYear <- data.frame( Year = uniqueYears )
TotalByYear$PM2.5 <- vapply( TotalByYear$Year, function (yr) {
  sum(NEI[NEI$SCC %in% TotalSCCs & NEI$year == yr, "Emissions"], na.rm = TRUE)
}, 0)


plot(x = TotalByYear$Year, y = TotalByYear$PM2.5 / 1000, type = 'b',
     xlab = "NEI Year Reported", ylab = "PM2.5 (kilotons)",
     main = "PM2.5 decreases over reported time frame")

savePlot("plot1.png")

