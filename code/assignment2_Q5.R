##################################################################
# How have emissions from motor vehicle sources 
# changed from 1999–2008 in Baltimore City?

###################################################################

# author: MAT Teunis, June 2016

##########################################
# Assignment 2: Exploratory Data Analysis, 
# Course 4, Coursera Data Science
###########################################

###############
# Question 5
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

# setting variable type
NEI$year <- as.factor(NEI$year)
NEI$Pollutant <- as.factor(NEI$Pollutant)
NEI$fips <- as.factor(NEI$fips)

##################################
# Selecting only data from "vehicle" related sources
##################################

# inspecting SCC
glimpse(SCC)
levels(SCC$SCC.Level.One)
levels(SCC$SCC.Level.Two)
levels(SCC$SCC.Level.Three)
Levels(SCC$SCC.Level.Four)

#### USING LEVEL.TWO: conains "vehicle info on- and off highway

# Subset coal combustion related NEI data
motor_vehicles <- grepl("vehicle", SCC$SCC.Level.Two, 
                        ignore.case=TRUE)

# subsets the SCC id's that are related to "coal combustion" 
motor_SCC <- SCC[motor_vehicles, ]$SCC

# subsets NEI for the SCC id's that are related to "coal combustion"
motor_NEI <- NEI[NEI$SCC %in% motor_SCC, ]

# inspecting result
glimpse(motor_NEI)


########

baltimore_motor_data <- motor_NEI %>% filter(fips == "24510")

# summary for Baltimore per year
baltimore_motor_summary <- summarise(group_by(baltimore_motor_data, 
                                                  year),
                                         mean=mean(Emissions), 
                                         sd=sd(Emissions),
                                         median = median.default(Emissions),
                                         observations = length(Emissions),
                                         total = sum(Emissions)) 


##################################
# plotting 
##################################
library(ggplot2)
# function to save ggplot plots
# A function to make it quick to save graphs in the image directory
# this function takes the argument imageDirectory and filename 
# as arguments


imageDirectory <- "./images"

saveInImageDirectory<-function(imageDirectory,filename){
  imageFile <- file.path(imageDirectory, filename)
  ggsave(imageFile, dpi = 300, width = 8, height = 6)	
}


graph <- ggplot(data = baltimore_motor_summary, 
                aes(year, total, group = 1))

graph + geom_point(color = "purple", size = 4) +
  geom_line(color = "blue", size = 1.5) +
  labs(title = "Total PM2.5 Emissions Baltimore - Motor Vehicles") +
  xlab("year") + 
  ylab("Total PM2.5 Emissions (tons)") 

## saving the graph
saveInImageDirectory(imageDirectory = imageDirectory,
                     filename = "plot5.png")

