#################################################################################################
# 1 - Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Results: 
#Average emissions have declined in tonnes
#Median emissions declined slightly (not as pronounced as averages) throughout the 9 year period
#Statistical significance is questionable as variances are extremely high
#################################################################################################

# Load Data
NEI <- readRDS("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/summarySCC_PM25.rds")
SCC <- readRDS("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/Source_Classification_Code.rds")
NEI <- transform(NEI, Year = factor(year))

## Run a group by summary of statistics to find mean, median, and standard deviation of Emissions by Years (Check Work) 
install.packages("doBy"); library(doBy)
xtrial<-summaryBy(Emissions ~ Year, data = NEI, FUN = function(x) { c(mu=mean(x), std=sd(x), md=median(x))}); xtrial

## Use Bargraphs to provide visual comparison of (1) average, (2) median and total emissions
png("C:/Users/Huitzilopoctli/Desktop/EDA-Course-Project-2/plot1.png", width=780, height=420)
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