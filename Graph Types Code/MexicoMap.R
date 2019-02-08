install.packages('sf')
install.packages('tigris')
install.packages('gghighlight')
library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tigris)
library(gtools)
library(ggrepel)
library(gghighlight)

# Read Mexico Shapefile
mex_shape <- st_read('Data/Mex_adm/Mex_adm2.shp')
