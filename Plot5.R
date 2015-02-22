#############################################################################################
# 5 - How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
# Average and median vehicle emissions have all declined between 1999 - 2008.
#############################################################################################

# Load Data
NEI <- readRDS("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/summarySCC_PM25.rds")
SCC <- readRDS("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/Source_Classification_Code.rds")
NEI <- transform(NEI, Year = factor(year))

# Get Baltimore emissions from motor vehicle sources
Maryland.vehilce.emissions <- NEI[NEI$SCC %in% SCC[grep("Vehicle", SCC$EI.Sector), 1], ]
EISector<-subset(SCC, select=c("SCC","EI.Sector"))
Maryland.vehilce.emissions<-merge(EISector, Maryland.vehilce.emissions, by="SCC")
Maryland.vehilce.emissions<-subset(Maryland.vehilce.emissions, fips=="24510", select=c("Emissions", "EI.Sector", "type", "Year", "fips"))

library(doBy); library("ggplot2"); library("gridExtra")

## Used Bargraphs to provide visual comparison of (1) average, (2) median and total emissions
xtrial<-summaryBy(Emissions ~ EI.Sector+Year, data = Maryland.vehilce.emissions, FUN = function(x) { c(mu = mean(x), std = sd(x), md =median(x)) })
xtrial

p1<-ggplot(xtrial, aes(x = Year, y = Emissions.mu)) + 
  geom_point(aes(color = EI.Sector), size = 10, alpha = 0.3) + 
  geom_line(aes(group=1)) + 
  facet_grid(. ~EI.Sector)+guides(colour=FALSE)+
  ggtitle("Average Vehicle Emissions from 1999 - 2008: Baltimore")

p2<-ggplot(xtrial, aes(x = Year, y = Emissions.md)) + 
  geom_point(aes(color = EI.Sector), size = 10, alpha = 0.3) + 
  geom_line(aes(group=1)) + 
  facet_grid(. ~EI.Sector)+guides(colour=FALSE)+
  ggtitle("Median Vehicle Emissions from 1999 - 2008: Baltimore")

png("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/plot5.png", width=1040, height=560)
grid.arrange(p1, p2, nrow=2)
dev.off()