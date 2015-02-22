#################################################################################################
#3 - Which sources have seen decreases/increases in emissions from 1999-2008 for Baltimore City?
# Statistical summary illustrates declines in average emission rates across all emission types.
# Graphs show outliers influence the averages, thus statistical medians are more representative.

# Using medians and graphs, Road and Non-road emissions have initially declined then held steady.
# Point emissions have declined over time continually.
# Non-point emissions vary, initially increasing in emissions then subsiding down in 2008
#################################################################################################
## Code guidelines adapted from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

# Load Data
NEI <- readRDS("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/summarySCC_PM25.rds")
SCC <- readRDS("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/Source_Classification_Code.rds")
NEI <- transform(NEI, Year = factor(year))

## Subset required.
NEI.maryland.emissions<-subset(NEI, fips==24510, select=c("Emissions", "type", "Year"))

## Statistical summary of mean, sd, and median.
library(doBy); library("ggplot2")
xtrial<-summaryBy(Emissions ~ type + 
                    Year, data = NEI.maryland.emissions, FUN = function(x) { c(mu = mean(x), std = sd(x), md =median(x)) } ); xtrial

png("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/plot3.png")
ggplot(NEI.maryland.emissions, aes(Year, Emissions), main="Emissions by type") + 
  geom_point(aes(color = type), size = 5, alpha = 0.5) + 
  facet_grid(. ~type) + 
  stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median, geom = "crossbar", width = 0.5) + 
  ylim(0,250)+ggtitle("Emissions by type: Baltimore")+guides(colour=FALSE)
dev.off()