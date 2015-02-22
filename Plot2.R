#####################################################################################################
# 2 - Have total emissions from PM2.5 decreased in the Baltimore City from 1999 to 2008? 
# Average emissions have declined in tonnes
# Median emissions declined slightly throughout the 9 year period
# Variances have declined. Probably grouping across demographics caused increase in previous variance
#####################################################################################################

# Load Data
NEI <- readRDS("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/summarySCC_PM25.rds")
SCC <- readRDS("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/Source_Classification_Code.rds")
NEI <- transform(NEI, Year = factor(year))

## Subset required.
NEI.maryland.emissions<-subset(NEI, fips==24510, select=c("Emissions", "type", "Year"))

## Statistical summary of mean, sd, and median.
library(doBy)
xtrial<-summaryBy(Emissions ~ Year, data = NEI.maryland.emissions, FUN = function(x) { c(mu = mean(x), std = sd(x), md =median(x)) } ); xtrial

png("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/plot2.png", width=780, height=420)
par(mfrow = c(1, 2))
bp<-barplot(xtrial$Emissions.mu, 
            col = rainbow(20, start = 0, end = 1),
            main = "Average Emissions",
            xlab = "Years",
            ylab = "Average Emissions (Tonnes)")
axis(1,at=bp,labels=xtrial$Year)

boxplot(log(Emissions+1) ~ Year, 
        data=NEI,
        ylim=c(0,1),
        col = rainbow(20, start = 0, end = 1),
        main = "Log(Median) Emissions",
        xlab = "Years",
        ylab = "Log(Median) Emissions (Tonnes)")
dev.off()