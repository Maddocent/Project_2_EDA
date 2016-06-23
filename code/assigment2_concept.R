##########################################
# Assignment 2: Exploratory Data Analysis, Course 4, Coursera Data Science
###########################################
search()

#########
# packages
#########

# installing packages:
install.packages("rmarkdown")
install.packages("downloader")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("grid")
install.packages("gridExtra")
install.packages("cowplot")
install.packages("lattice")
install.packages("magrittr")

## loading the packages
library(rmarkdown)
library(downloader)
library(dplyr)
library(lubridate)
library(ggplot2)
library(grid)
library(gridExtra)
library(cowplot)
library(lattice)
library(magrittr)
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

########## Fisrt plot
plot(NEI$year, NEI$Emissions)
###########

NEI$year <- as.factor(NEI$year)
levels(NEI$year)
summary(NEI$Emissions)
hist(NEI$Emissions, breaks = 10)
# from the summary and the histogram it can be concluded that the 
# data is largely skewed
?hist
boxplot(Emissions ~ year, data = NEI)

plot(NEI$year, NEI$Emissions)
# there seems to be an outlier in 2002 at around an emission at
# just over>6e+05
outlier <- NEI %>% filter(Emissions > 500000)

# removing the outlier
# the oulier is has rowname "1"
# so 

NEI_outlier_removed <- NEI %>% filter(!Emissions >500000) # should produce the oulier
str(NEI_outlier_removed)
class(NEI_outlier_removed)

plot(NEI_outlier_removed$year, NEI_outlier_removed$Emissions)
model <- lm(year, Emissions, data = NEI_outlier_removed)
abline(model, lwd = 2)

# From this plot we can see that there are high low and intermediate values
# for emissions

high <- NEI %>% filter(Emissions > 50000)
intermediate <- NEI %>% filter(Emissions <= 50000, Emissions > 500 )
low <- NEI %>% filter(Emissions <= 500)
zero <- NEI %>% filter(Emissions == 0)
non_zero <- NEI %>% filter(!Emissions == 0)


# setting variable type to factor



# making histograms of these classes of emissions
hist(high$Emissions)
hist(intermediate$Emissions)
hist(low$Emissions)


# panel of plots year vs emissions
par(mfcol = c(3,1))
plot(high$year, high$Emissions, ylab = "Emissions(tons/year)", xlab = "Year")
plot(intermediate$year, intermediate$Emissions, ylab = "Emissions(tons/year)", xlab = "Year")
plot(low$year, low$Emissions, ylab = "Emissions(tons/year)", xlab = "Year")


plot





percentage_zeros <- (length(zero$Emissions)/length(NEI$Emissions))*100
# shows that appoximately 5% of the detectors measured zero emmission

zero_per_year <- table(zero)
barplot(zero$year, )

