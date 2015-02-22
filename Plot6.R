###############################################################################
# 6 - Baltimore vs LA Vehicle Emissions?
# Greatest relative change is Baltimore (in terms of change - 300% difference).
# In terms of size, LA is the greater 
###############################################################################

# Load Data
NEI <- readRDS("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/summarySCC_PM25.rds")
SCC <- readRDS("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/Source_Classification_Code.rds")
NEI <- transform(NEI, Year = factor(year))

# Prepare LA & Baltimore Data
LA.vehilce.emissions <- NEI[NEI$SCC %in% SCC[grep("Vehicle", SCC$EI.Sector), 1], ]
EISector<-subset(SCC, select=c("SCC","EI.Sector"))
LA.vehilce.emissions<-merge(EISector, LA.vehilce.emissions, by="SCC")
LA.vehilce.emissions<-subset(LA.vehilce.emissions, fips=="06037", select=c("Emissions", "EI.Sector", "type", "Year", "fips"))

LA.Balt.vehicle.emissions<-rbind(Maryland.vehilce.emissions,LA.vehilce.emissions)
LA.Balt.vehicle.emissions$State<-(ifelse(LA.Balt.vehicle.emissions$fips==24510,"Balt","LA"))

## Used Bargraphs to provide visual comparison of (1) average, (2) median and total emissions
xtrial<-summaryBy(Emissions ~ State+Year, data = LA.Balt.vehicle.emissions, FUN = function(x) { c(mu = mean(x), md =median(x), std = sd(x), summation=sum(x))})
xtrial

png("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/plot6.png", width=560, height=300)
ggplot(data=xtrial, aes(x=Year, y=Emissions.summation, fill=State)) + 
  geom_bar(stat="identity", position=position_dodge()) + 
  xlab('Year') + 
  ylab('Total Emissions Tonnes') + 
  ggtitle('Comparison of LA vs Baltimore Emissions Over Time') 
dev.off()