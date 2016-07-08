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


png(filename = "./images/plot1.png", 
    width = 480, 
    height = 480, 
    units = "px")
par(mar = c(3, 5, 4, 8))
par(las = 1)
#par(mgp = 0, 1, 0.5)
# plotting a barplot with total emissions per year
barplot(summary$total, axes = FALSE,  
        ylab = "Total PM2.5 Emissions (tons)", cex.axis = 1.2,
        cex.lab = 1.2, main = "Total PM2.5 Emissions USA per year", 
        cex.main = 1.2,
        names.arg = years, col = "blue",
        cex.names = 1.2)
axis(side = 4, cex = 1.2)

dev.off()

###########################################################




