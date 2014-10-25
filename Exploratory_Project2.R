#EXPLORATORY DATA ANALYSIS - PROJECT 2
#######################################
#Goal of assignment - address the following questions about the data

## 1.Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
## 2.Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.
## 3.Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
## 4.Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
## 5.How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?
## 6.Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

library(ggplot2)

#read data
emissions <- readRDS("./data/summarySCC_PM25.rds")
codes <- readRDS("./data/Source_Classification_Code.rds")

# Question 1
##Have emissions decreased in US from 1999 to 2008
#aggregate data
aggr <- aggregate(emissions[c("Emissions")], list(year = emissions$year), sum)
#create plot 1
png('plot1.png', width=480, height=480)

plot(aggr$year, aggr$Emissions, type = "l", 
        main = "Total Emissions in the US",
        xlab = "Year", ylab = "Emissions")

# Question 2
##Have emissions decreated in Baltimore City from 1999 to 2008
##subset and aggregate data
baltimore <- subset(emissions, fips == "24510")
aggr1 <- aggregate(baltimore[c("Emissions")], list(year = baltimore$year), sum)
# create plot 2
png('plot2.png', width=480, height=480)

plot(aggr1$year, aggr1$Emissions, type = "l",
        main = "Total Emissions in Baltimore City",
        xlab = "Year", ylab = "Emissions")

# Question 3
##Which of the 4 sources "type" have seen decreases in Baltimore from 1999 to 2008
#prep data
baltimore <- subset(emissions, fips == "24510")
aggrx <- aggregate(baltimore[c("Emissions")], list(type = baltimore$type, year = baltimore$year), sum)
#create plot 3
png('plot3.png', width=600, height=480)

qplot(year, Emissions, data=aggrx, facets= .~type,
      main="Total Emissions by Type in Baltimore City",
      xlab="Year", ylab="Emissions")

#Question 4
##Across the US, how have emissions from coal combusion-related sources changed from 1999-2008
#prep data
x <- grep("coal", codes$EI.Sector, value=T, ignore.case=T)
sub1 <- subset(codes, codes$EI.Sector %in% x, select=SCC)
sub2 <- subset(emissions, emissions$SCC %in% sub1$SCC)
aggr2 <- aggregate(sub2[c("Emissions")], list(year = sub2$year), sum)

# create plot 4
png('plot4.png', width=480, height=480)

plot(aggr2$year, aggr2$Emissions, type="l",
     main = "Total Coal Combustion Emissions in the US",
     xlab = "Year", ylab = "Emissions")

#Question 5
##How have emissions from motor vehicle sources changed in Baltimore City from 1999-2008
#prep data
vehicle <- grep("vehicle",codes$EI.Sector,value=T,ignore.case=T)
sub1_vehicle <- subset(codes, codes$EI.Sector %in% vehicle, select=SCC)
baltimore <- subset(emissions, fips == "24510")
sub2_vehicle <- subset(baltimore, baltimore$SCC %in% sub1_vehicle$SCC)
aggr3<- aggregate(sub2_vehicle[c("Emissions")], 
                  list(year = sub2_vehicle$year), sum)
#create plot 5
png('plot5.png', width=480, height=480)

plot(aggr3$year, aggr3$Emissions, type = "l",
      main = "Total Vehicle Emissions in Baltimore City",
      xlab = "Year", ylab = "Emissions")

#Question 6
##Between Baltimore City and Los Angeles county, which city has seen greater changes over time in motor emissions
#prep data
vehicle <- grep("vehicle",codes$EI.Sector,value=T,ignore.case=T)
sub3_vehicle<- subset(codes, codes$EI.Sector %in% vehicle, select=SCC)
y <- subset(emissions, fips == "24510"|fips == "06037")
sub4_vehicle <- subset(y, y$SCC %in% sub3_vehicle$SCC)
aggr4 <- aggregate(sub4_vehicle[c("Emissions")], list(fips = sub4_vehicle$fips, year = sub4_vehicle$year), sum)
aggr4$city <- rep(NA, nrow(aggr4))
aggr4[aggr4$fips == "06037", ][, "city"] <- "Los Angles County"
aggr4[aggr4$fips == "24510", ][, "city"] <- "Baltimore City"
#create plot 6
png('plot6.png', width=480, height=480)

qplot(year, Emissions, data=aggr4, facets= .~city,
      main= "Total Vehicle Emissions - Baltimore City vs Los Angles County",
      xlab = "Year", ylab = "Emissions")
















