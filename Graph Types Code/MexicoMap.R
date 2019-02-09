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

# Read in Deported from Mexico data
path <- "Data/"
files <- list.files(path=path, pattern="*2016mex.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

elsdev2016mex$pais <- 'El Salvador'
honddev2016mex$pais <- 'Honduras'
guatedev2016mex$pais <- 'Guatemala'

# Pull out where people deported from p34m

honddev2016mex <- honddev2016mex[c('pais', 'p34m')]
guatedev2016mex <- guatedev2016mex[c('pais', 'p34m')]
elsdev2016mex <- elsdev2016mex[c('pais', 'p34m')]

# Pull out values for every municipality in the responses
hond_vals <- read.csv('Values/Honduras_Valores.csv')
guate_vals <- read.csv('Values/Guate_Valores.csv')
els_vals <- read.csv('Values/Els_Valores.csv')
hond_vals <- subset(hond_vals, Variable == 'p34m')
guate_vals <- subset(guate_vals, Variable == 'p34m')
els_vals <- subset(els_vals, Variable == 'p34m')



