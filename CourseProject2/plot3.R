## Coursera assignment
## https://www.coursera.org/learn/exploratory-data-analysis/peer/b5Ecl/course-project-2

Question <- 3
## Of the four types of sources indicated by the type (point,
## nonpoint, onroad, nonroad) variable, which of these four sources
## have seen decreases in emissions from 1999–2008 for Baltimore City?
## Which have seen increases in emissions from 1999–2008? Use the
## ggplot2 plotting system to make a plot answer this question.

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

uniqueYears <- unique(NEI$year) 

## Huh. If I filter by Lvl4 = "Total" then Baltimore only has NONPOINT
## entries. So I will presume that I should ignore the SCC codes for
## this question.

TotByYearAndSrc <- aggregate( Emissions ~ year + type,
                             data = NEI[NEI$fips == "24510", ], sum,
                             na.rm = TRUE)

library(ggplot2)
p <- ggplot( data = TotByYearAndSrc, aes(year, Emissions)) +
    labs( x = "Year", y = "Emissions",
         title = "Baltimore shows decreases from all types except point") + 
    geom_line( aes(color = type))

p

ggsave(sprintf("plot%d.png",Question))

