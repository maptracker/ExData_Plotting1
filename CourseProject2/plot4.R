## Coursera assignment
## https://www.coursera.org/learn/exploratory-data-analysis/peer/b5Ecl/course-project-2

Question <- 4
## Across the United States, how have emissions from coal
## combustion-related sources changed from 1999–2008?


NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

uniqueYears <- unique(NEI$year) 

## Get processes that involve both combustion and coal:
## Case-sensitive "Coal" grep to exclude "Charcoal"

BurningCoalSCCs <- as.character(SCC[ grepl('Combustion', SCC$SCC.Level.One) & grepl('Coal', SCC$Short.Name, ignore.case = FALSE), "SCC"])

## Build a frame to hold our data
TotalByYear <- data.frame( Year = uniqueYears )
TotalByYear$PM2.5 <- vapply( TotalByYear$Year, function (yr) {
  sum(NEI[NEI$SCC %in% BurningCoalSCCs &
          NEI$year == yr,
          "Emissions"], na.rm = TRUE)
}, 0)

plot(x = TotalByYear$Year, y = TotalByYear$PM2.5, type = 'b',
     xlab = "NEI Year Reported", ylab = "PM2.5 (tons)",
     main = "PM2.5 production due to burning coal\ndecreased dramatically after 2005")

savePlot(sprintf("plot%d.png",Question))
