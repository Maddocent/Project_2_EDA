##################################################################
# Across the United States, how have emissions from 
# coal combustion-related sources changed from 1999–2008?
###################################################################

# author: MAT Teunis, June 2016

##########################################
# Assignment 2: Exploratory Data Analysis, 
# Course 4, Coursera Data Science
###########################################

###############
# Question 4
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
install.packages("scales")


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
library(scales)

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
# Selecting only data from coal combustion related sources
##################################
# inspecting SCC
glimpse(SCC)
levels(SCC$SCC.Level.One)
levels(SCC$SCC.Level.Two)
levels(SCC$SCC.Level.Three)
levels(SCC$SCC.Level.Four)

# --> Level.One and Level.Four hold information that is relavant for 
# "coal combuation"

# Subset coal combustion related NEI data
combustion <- grepl("comb", SCC$SCC.Level.One, ignore.case=TRUE)
coal <- grepl("coal", SCC$SCC.Level.Four, ignore.case=TRUE) 

# combining the two vectors
coal_combustion <- (combustion & coal)

# subsets the SCC id's that are related to "coal combustion" 
coal_combustion_SCC <- SCC[coal_combustion,]$SCC

# subsets NEI for the SCC id's that are related to "coal combustion"
coal_combustion_NEI <- NEI[NEI$SCC %in% coal_combustion_SCC,]

# inspecting result
glimpse(coal_combustion_NEI)


########
# summary per year

coal_combustion_NEI_summary <- summarise(group_by(coal_combustion_NEI, 
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


graph <- ggplot(data = coal_combustion_NEI_summary, 
                aes(year, total, group = 1))

graph + geom_point(color = "brown", size = 3) +
  geom_line(color = "darkgreen", size = 1.5) +
  labs(title = "Total PM2.5 Emissions USA - Coal Combustion") +
  xlab("year") + 
  ylab("Total PM2.5 Emissions (tons)") 

## saving the graph
saveInImageDirectory(imageDirectory = imageDirectory,
                     filename = "plot4.png")

