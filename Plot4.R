########################################################################################################
# 4 - Across the US, how have coal emissions changed between 1999 - 2008?
# Statistical summary illustrates declines in average emission rates across all emission types.
# Graphs show outliers influence the averages, thus statistical medians are more representative.

# Using medians and graphs, Commercial/Insitutional coal emissions have declined (small declines)
# Electric generation shows no change and varies between medians of 1 - 3 tonnes for each examined year.
# Boilers (Industrial) are actually increasing in emissions.
########################################################################################################

# Load Data
NEI <- readRDS("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/summarySCC_PM25.rds")
SCC <- readRDS("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/Source_Classification_Code.rds")
NEI <- transform(NEI, Year = factor(year))

## Finds coal sources & respective SCC code
## Derives a temp dataset from NEI where SCC is a coal source 
US.coalsource.emissions <- NEI[NEI$SCC %in% SCC[grep("Coal", SCC$EI.Sector), 1], ]

## For EI Sector associated to SCC code
EISector<-subset(SCC, select=c("SCC","EI.Sector"))
US.coalsource.emissions<-merge(EISector, US.coalsource.emissions, by="SCC")[c(1,2,5,8)]
rm(EISector)

library(doBy); library("ggplot2")

png("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/plot4.png", width=740, height=400)
ggplot(US.coalsource.emissions, aes(x = Year, y = Emissions)) + 
  geom_point(aes(color = EI.Sector), size = 10, alpha = 0.3) + 
  facet_grid(. ~EI.Sector) + 
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median, geom = "crossbar", width = 0.5) + 
  ylim(0,5000) +guides(colour=FALSE)+
  ggtitle("Coal Emissions for US: Median Lines Included")
dev.off()

xtrial<-summaryBy(Emissions ~ EI.Sector + Year, data = US.coalsource.emissions, FUN = function(x) { c(mu = mean(x), std = sd(x), md =median(x)) } )
xtrial