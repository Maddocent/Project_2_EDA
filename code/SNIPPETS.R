#

# plotting only low values
with(low, plot(low$year, low$Emissions, 
               ylab = "Emissions in Baltimore (tons/year)", 
               xlab = "Year", main = "Low Emissions",
               cex.axis = 1.5, 
               cex.lab = 1.5, cex.main = 1.5))



