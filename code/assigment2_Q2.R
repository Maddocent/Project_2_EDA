
## Have total emissions from PM2.5 decreased in 
## the Baltimore City, Maryland (fips == "24510") from 
## 1999 to 2008? Use the base plotting system to make 
## a plot answering this question.

# author: MAT Teunis, June 2016

##########################################
# Assignment 2: Exploratory Data Analysis, 
# Course 4, Coursera Data Science
###########################################

###############
# Question 2
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


##################################
# Selecting only data from Baltimore
##################################

baltimore_data <- NEI %>% filter(fips == "24510")

baltimore_data_summary <- summarise(group_by(baltimore_data, 
                                             year),
                       mean=mean(Emissions), 
                       sd=sd(Emissions),
                       median = median.default(Emissions),
                       observations = length(Emissions),
                       total = sum(Emissions)) 

years <- as.character(baltimore_data_summary$year)





##################################
# plotting Baltimore data
##################################

png(filename = "./images/plot2.png", 
    width = 1200, 
    height = 800, 
    units = "px")
par(mar = c(5, 5, 4, 1))

barplot(baltimore_data_summary$total,  
        ylab = "Total Emission (tons/year)", cex.axis = 1.5,
        cex.lab = 1.5, main = "Total emission per year", 
        cex.main = 1.5,
        names.arg = years, col = "red")

dev.off()


