NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

baltimoreSubNEI  <- NEI[NEI$fips=="24510", ]

baltimoreEmissionByYear <- aggregate(Emissions ~ year, baltimoreSubNEI, sum)

png('plot2.png')
barplot(
  height=baltimoreEmissionByYear$Emissions, 
  names.arg=baltimoreEmissionByYear$year, 
  xlab="years", 
  ylab="Sum PM2.5 emission",
  main="Total PM2.5 emissions per Year in de Baltimore City")
dev.off()