###################
# loading packages
###################


#installs "pacman" if not installed
require("pacman") || utils::install.packages("pacman")
library(pacman)

# installs additonal R packages (CRAN packages)
pacman::p_load(gplots, ggplot2, gdata, car, pastecs, 
               psych, reshape, lubridate,
               nlme, pander, xtable, dplyr, xlsx, 
               rJava, plsdepot, data.table, Rcpp, Matrix,
               parallel, ape, reshape2, phangorn, stringi,
               stringr, downloader, devtools, gridExtra, 
               cowplot, lattice, magrittr)
