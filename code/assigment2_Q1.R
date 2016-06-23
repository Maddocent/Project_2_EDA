##########################################
# Assignment 2: Exploratory Data Analysis, Course 4, Coursera Data Science
###########################################

###############
# Question 1
##############

##############
# packages
###############

# installing packages:
# install.packages("assertthat")
#install.packages("lazyeval")
#install.packages("rmarkdown")
#install.packages("downloader")
#install.packages("dplyr", dependencies = TRUE)
#install.packages("lubridate")
#install.packages("ggplot2")
#install.packages("grid")
#install.packages("gridExtra")
#install.packages("cowplot")
#install.packages("lattice")
#install.packages("magrittr")
#install.packages("DBI")

## loading the packages
library(lazyeval)
library(assertthat)
library(rmarkdown)
library(downloader)
library(lubridate)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)
library(lattice)
library(magrittr)
library(dplyr)

###################
# project structure:
# creates three folders: 
# 1) "data": contains the datafiles 
# 2) "code": contains the R code for the whole project  
# 3) "images": will contain the image files for the project
##########################



############################
# Getting Data
############################
# the data can be downloaded as a zip file from:
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"

# To download and unzip the data:
download(url, dest="./data/project2_dataset.zip", mode="wb") 
unzip("./data/project2_dataset.zip", exdir = "./data")

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")
# fips: A five-digit number (represented as a string) indicating the U.S. county
# SCC: The name of the source as indicated by a digit string (see source code classification table)
# Pollutant: A string indicating the pollutant
# Emissions: Amount of PM2.5 emitted, in tons
# type: The type of source (point, non-point, on-road, or non-road)
# year: The year of emissions recorded

############
# Inspecting data
############
str(NEI)
head(NEI)

str(SCC)

NEI$year <- as.factor(NEI$year)
NEI$Pollutant <- as.factor(NEI$Pollutant)
NEI$fips <- as.factor(NEI$fips)

NEI$year <- as.factor(NEI$year)
levels(NEI$year)
summary(NEI$Emissions)
hist(NEI$Emissions, breaks = 10)

# from the summary and the histogram it can be concluded that the 
# data is largely skewed
# and that there are many zero-emitions
# there seems to be an outlier in 2002 at around an emission at
# just over>6e+05

outlier <- NEI %>% filter(Emissions >= 600000)
str(outlier)

# identifying the outlier with SCC:
str(SCC)
outlier_source <- SCC %>% filter(SCC == outlier[,"SCC"])

##It seems to be from a massive wildfire/forest fire or fires
# removing the outlier

NEI_outlier_removed <- NEI %>% filter(!Emissions >600000) # should produce the oulier
str(NEI_outlier_removed)
class(NEI_outlier_removed)

# From this plot we can see that there are high low and intermediate values
# for emissions

high <- NEI %>% filter(Emissions > 50000)
intermediate <- NEI %>% filter(Emissions <= 50000, Emissions > 500 )
low <- NEI %>% filter(Emissions <= 500)
zero <- NEI %>% filter(Emissions == 0)
non_zero <- NEI %>% filter(!Emissions == 0)

# what sources are the other 'high' observations?
sources_high <- high$SCC
sources_high <- as.factor(sources_high)
sources_high_actual <- levels(sources_high)
# matching these to the SCC dataframe
str(SCC)

match_high_1 <- SCC %>% filter(SCC == sources_high_actual[1])
match_high_2 <- SCC %>% filter(SCC == sources_high_actual[2])

match_list <- list(match_high_1, match_high_2)
match_list[[1]]
match_list[[2]]

percentage_zeros <- (length(zero$Emissions)/length(NEI$Emissions))*100
# shows that appoximately 5% of the detectors measured zero emmission

####################
# summary statistics
####################

summary <- summarise(group_by(NEI, year),
          mean=mean(Emissions), 
          sd=sd(Emissions),
          median = median.default(Emissions),
          observations = length(Emissions),
          total = sum(Emissions)) 
          
years <- as.character(summary$year)

## adressing question 1:
## Have total emissions from PM2.5 decreased in the 
# United States from 1999 to 2008? 
# Using the base plotting system, 
# make a plot showing the total PM2.5 emission 
# from all sources for each of the years 1999, 2002, 2005, 
# and 2008.


str(high)
high$year <- as.character(high$year)
low$year <- as.character(low$year)
intermediate$year <- as.character(intermediate$year)

# panel of plots year vs emissions

label_1 <- c("Big Forest Fires")
label_2 <- c("Fire", "Fire", "Fire", "Fire", "Fire")


##########################################
## Inspecting the data with 
## plot to see whether emission dropped
#########################################

png(filename = "./images/plot1.png", 
    width = 1200, 
    height = 800, 
    units = "px")
par(mfcol = c(2,2))
## a plot with all the data
par(mar = c(5, 5, 4, 1))
with(NEI, plot(year, Emissions, 
     ylab = "Emissions(tons/year)", 
     xlab = "Year", main = "Complete data", cex.axis = 1.5, 
     cex.lab = 1.5, cex.main = 1.5))
with(subset(NEI, Emissions > 600000), 
     points(year, Emissions, col = "red", pch = 8, cex = 1.5))
text(x = outlier$year, y = outlier$Emissions, 
       labels = label_1, pos = 1, offset = 0.5, cex = 1.5)

# plot with high emission
with(high, plot(year, Emissions, 
               ylab = "Emissions(tons/year)", 
               xlab = "Year", main = "High Emissions",
               cex.axis = 1.5, 
               cex.lab = 1.5,
               pch = 8, col = "red",
               cex.main = 1.5))
text(x = high$year, y = high$Emissions, 
     labels = label_2, pos = c(3,2,2,2,2), offset = 0.5, cex = 1.5)


# plotting only intermediate values
with(intermediate, plot(year, Emissions, 
               ylab = "Emissions(tons/year)", 
               xlab = "Year", main = "Intermediate Emissions",
               cex.axis = 1.5, 
               cex.lab = 1.5, cex.main = 1.5))

# plotting a barplot with 
barplot(summary$total,  
        ylab = "Total Emission (tons/year)", cex.axis = 1.5,
        cex.lab = 1.5, main = "Total emission per year", 
        cex.main = 1.5,
        names.arg = years, col = "blue")

dev.off()

###########################################################




