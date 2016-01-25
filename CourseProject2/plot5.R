## Coursera assignment
## https://www.coursera.org/learn/exploratory-data-analysis/peer/b5Ecl/course-project-2

Question <- 5
## How have emissions from motor vehicle sources changed from
## 1999–2008 in Baltimore City?

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

uniqueYears <- unique(NEI$year)

## I initially tried to limit the data to just the "Total" SCCs:

HighwaySCCs <- SCC[ grepl('^Highway Vehicles', SCC$SCC.Level.Two) &
                    grepl('^Total', SCC$SCC.Level.Four), c("SCC")]


# [1] Highway Veh - Gasoline - Light Duty Vehicles (LDGV) - Total: All Road Types
# [2] Highway Veh - Gasoline - Light Duty Trucks 1 & 2 - Total: All Road Types
# [3] Highway Veh - Gasoline - Light Duty Trucks 3 & 4 - Total: All Road Types
# [4] Highway Veh - Gasoline - NOT USED - Prev all LDGT (1&2) in M5 - Total: All Road Types
# [5] Highway Veh - Gasoline - Heavy Duty Veh 2B thru 8B & Buses (HDGV) - Total: All Road Types
# [6] Highway Veh - Gasoline - Motorcycles (MC) - Total: All Road Types
# [7] Highway Veh - Diesel - Light Duty Vehicles (LDDV) - Total: All Road Types
# [8] Highway Veh - Diesel - Light Duty Trucks 1 thru 4 (LDDT) - Total: All Road Types
# [9] Highway Veh - Diesel - All HDDV incl Buses - Total: All Road Types

## ... but that ends up with no data for Baltimore. So I am just
## taking all "Highway Vehicle" entries:

HighwaySCCs <- SCC[ grepl('^Highway Vehicles', SCC$SCC.Level.Two), c("SCC")]

HighwaySCCs <- as.character(HighwaySCCs)

## I am interested in looking at both the total PM2.5 and the "Level3" classes of vehicles.
## Merge the Lvl3 names from the SCC file and filter down to Baltimore at the same time:

baltVeh <- merge(x = NEI[NEI$fips == "24510" & NEI$SCC %in% HighwaySCCs,
                         c("year", "SCC", "Emissions")],
                 y = SCC[,c("SCC", "SCC.Level.Three")],
                 by.x = "SCC", by.y = as.character("SCC"))

## I don't like that column name
library(dplyr)
baltVeh <- rename(baltVeh, Type = SCC.Level.Three)
baltVeh$Type <- as.character(baltVeh$Type)

## Aggregate by type and year
yearType <- aggregate(Emissions ~ year + Type, data = baltVeh, sum,
                      na.rm = TRUE)

## Add a grand total
gtot <- aggregate(Emissions ~ year, data = yearType, sum)
gtot$Type <- rep("Total", length(uniqueYears))
yearType <- rbind(gtot, yearType)

## Let's order the type by decreasing emissions
lastYear <- yearType[ yearType$year == 2008, ]
eord <- order( lastYear$Emissions, decreasing = TRUE )
lvls <- lastYear[ eord, "Type"]
## And then factorize the type
yearType$Type <- factor(yearType$Type, levels = lvls)

## Plot


library(ggplot2)
p <- ggplot( data = yearType, aes(year, Emissions)) +
    scale_y_log10() +
    labs( x = "Year", y = "Emissions (log10 scale)",
         title = "Baltimore vehicle emissions") + 
    geom_line( aes(color = Type))

p

ggsave(sprintf("plot%d.png",Question), dpi = 100)

