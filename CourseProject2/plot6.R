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

## Now take out 1999, since it is going to be all zeros anyway:
yearType <- yearType[ yearType$year != 1999, ]

## There's probably a more R-ish way to do this. R tends to be
## column-centric, and this is a row-centric operation, so I am going
## to fall back to my comfort zone: a loop
nRows <- nrow(yearType)
normExp <- vector("numeric", length = nRows)
for ( i in 1:nRows) {
    ## This is the relevant value from 1999:
    e1999 <- emis1999[emis1999$City == yearType[ i, "City" ] &
                      emis1999$Type == yearType[ i, "Type" ] , "Emissions" ]
    ## This is percent change from 1999:
    normExp[i] <- (100 * yearType[ i, "Emissions" ] / e1999) - 100
}
yearType$PercentChange <- normExp

## Let's order the type by decreasing emissions for baltimore in 2008:

lastYear <- yearType[ yearType$year == 2008 & yearType$fips == "24510" , ]
eord <- order( lastYear$Emissions, decreasing = TRUE )
lvls <- lastYear[ eord, "Type"]
## And then factorize the type
yearType$Type <- factor(yearType$Type, levels = lvls)

## Plot

library(ggplot2)

## Initially did a line graph:
#p <- ggplot( data = yearType, aes(year, PercentChange)) +
#    # scale_y_log10() +
#    labs( x = "Year", y = "Emissions (Percent change from 1999)",
#         title = "Baltimore & LA vehicle emissions") + 
#    geom_line( aes(color = Type, linetype = City))

## But a heat map would less cluttered
## https://learnr.wordpress.com/2010/01/26/ggplot2-quick-heatmap-plotting/
p <- ggplot(data = yearType, aes(as.factor(year), Type))  +
    geom_tile(aes(fill = PercentChange), colour = "white") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         limits = c(-100,200),
                         guide = guide_legend(title = "%Change\nfrom 1999")) +
    facet_grid(. ~ City) +
    labs( x = "Year", y = "Vehicle Type",
         title = "Baltimore shows consistent emissions decrease\nLos Angeles shows little") 

p

ggsave(sprintf("plot%d.png",Question), dpi = 100)

