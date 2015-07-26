# Coursera Exploratory Data Analysis
# Course Project 2 - Plot 4
# Assignment at
# https://class.coursera.org/exdata-002/human_grading/view/courses/972082/assessments/4/submissions
# Data file from
# https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip
# See download_data.R

# Question 4:
# Across the United States, how have emissions from coal combustion-related
# sources changed from 1999-2008?

library(plyr)
library(ggplot2)

# Read the data file
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("data/summarySCC_PM25.rds")
SCC <- readRDS("data/Source_Classification_Code.rds")

# merge the two data sets 
NEISCC <- merge(NEI, SCC, by="SCC")

# fetch all NEIxSCC records with Short.Name (SCC) Coal
coalMatches  <- grepl("coal", NEISCC$Short.Name, ignore.case=TRUE)
subsetNEISCC <- NEISCC[coalMatches, ]

aggregatedTotalByYear <- aggregate(Emissions ~ year, subsetNEISCC, sum)

png("plot4.png")
qplot(year, Emissions, data=aggregatedTotalByYear, color=year, geom="line") +
  stat_summary(fun.y = "sum", fun.ymin = "sum", fun.ymax = "sum", 
               color = "black", aes(shape="total"), geom="line") +
  geom_line(aes(size="total", shape = NA)) +
  ggtitle(expression("Coal Combustion" ~ PM[2.5] ~ "Emissions by Year")) +
  xlab("Year") +
  ylab(expression("Total" ~ PM[2.5] ~ "Emissions (tons)"))
dev.off()