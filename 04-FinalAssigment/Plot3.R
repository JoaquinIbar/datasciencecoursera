NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

library(ggplot2)

baltimoreSubNEI  <- NEI[NEI$fips=="24510", ]

baltimoreEmissionByYearType <- aggregate(Emissions ~ year + type, baltimoreSubNEI, sum)

png("plot3.png", width=640, height=480)
g <- ggplot(baltimoreEmissionByYearType, aes(year, Emissions, color = type))
g <- g + geom_line() +
  xlab("year") +
  ylab("Total PM2.5 Emissions") +
  ggtitle("Total Emissions in Baltimore City, Maryland (fips == 24510) from 1999 to 2008")
print(g)
dev.off()