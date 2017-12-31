NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

emissionByYear <- aggregate(Emissions ~ year, NEI, sum)

png('plot1.png')
barplot(
  height = emissionByYear$Emissions, 
  names.arg=emissionByYear$year, 
  xlab="years", 
  ylab="Sum PM2.5 emission",
  main="Total PM2.5 emissions per Year")
dev.off()