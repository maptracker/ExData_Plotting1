## Coursera assignment
## https://www.coursera.org/learn/exploratory-data-analysis/peer/b5Ecl/course-project-2

Question <- 6
## Compare emissions from motor vehicle sources in Baltimore City with
## emissions from motor vehicle sources in Los Angeles County,
## California (fips == "06037"). Which city has seen greater changes
## over time in motor vehicle emissions?

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

uniqueYears <- unique(NEI$year) 

# Taking all "Highway Vehicle" entries:
HighwaySCCs <- SCC[ grepl('^Highway Vehicles', SCC$SCC.Level.Two), c("SCC")]
HighwaySCCs <- as.character(HighwaySCCs)

BaltLA <- merge(x = NEI[NEI$fips %in% c("24510","06037") &
                        NEI$SCC %in% HighwaySCCs,
                        c("year", "SCC", "fips", "Emissions")],
                y = SCC[,c("SCC", "SCC.Level.Three")],
                by.x = "SCC", by.y = as.character("SCC"))

## I don't like that column name
library(dplyr)
BaltLA <- rename(BaltLA, Type = SCC.Level.Three)
BaltLA$Type <- as.character(BaltLA$Type)

## Aggregate by type, year and city code
yearType <- aggregate(Emissions ~ year + Type + fips, data = BaltLA, sum,
                      na.rm = TRUE)

## Add a grand total
gtot <- aggregate(Emissions ~ year + fips, data = yearType, sum)
gtot$Type <- rep("Total", length(uniqueYears))
yearType <- rbind(gtot, yearType)


## Map fips into something legible
yearType$City <- ifelse(yearType$fips == "24510", "Baltimore", "Los Angeles")

## It's really not fair to compare LA to Baltimore on absolute values
## - LA is huge. Let's normalize each city/type to 1999 values

## Start by extracting our reference year data
emis1999 <- yearType[ yearType$year == 1999, c("City", "Emissions", "Type")]

## There's probably a more R-ish way to do this. R tends to be
## column-centric, tand this is a row-centric operation, so I am going
## to fall back to a loop:
nRows <- nrow(yearType)
normExp <- vector("numeric", length = nRows)
for ( i in 1:nRows) {
    normExp[i] <- 100 * yearType[ i, "Emissions" ] /
        emis1999[emis1999$City == yearType[ i, "City" ] &
                 emis1999$Type == yearType[ i, "Type" ] , "Emissions" ]
}
yearType$Normalized <- normExp

## Let's order the type by decreasing emissions for baltimore
lastYear <- yearType[ yearType$year == 2008 & yearType$fips == "24510" , ]
eord <- order( lastYear$Emissions, decreasing = TRUE )
lvls <- lastYear[ eord, "Type"]
## And then factorize the type
yearType$Type <- factor(yearType$Type, levels = lvls)

## Plot

library(ggplot2)
p <- ggplot( data = yearType, aes(year, Normalized)) +
    # scale_y_log10() +
    labs( x = "Year", y = "Emissions (100 = 1999 levels)",
         title = "Baltimore & LA vehicle emissions") + 
    geom_line( aes(color = Type, linetype = City))

p

ggsave(sprintf("plot%d.png",Question), dpi = 100)

