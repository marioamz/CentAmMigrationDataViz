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
mex_shape <- st_read('Data/Mex_adm/Mex_adm1.shp')

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

honddev2016mex <- honddev2016mex[c('pais', 'p34e')]
guatedev2016mex <- guatedev2016mex[c('pais', 'p34e')]
elsdev2016mex <- elsdev2016mex[c('pais', 'p34e')]

step1 <- smartbind(honddev2016mex, guatedev2016mex)
final <- smartbind(step1, elsdev2016mex)
rm(step1)

# Group so that each row is a state, and % of people found from each country as columns

ghond <- honddev2016mex %>%
  filter(p34e < 40) %>%
  add_tally() %>%
  add_count(p34e) %>%
  distinct(pais, p34e, nn, n)

ghond$perchond <- ghond$nn / ghond$n

ghond$p34e[ghond$p34e==2] <- "Baja California Sur"
ghond$p34e[ghond$p34e==3] <- "Baja California"
ghond$p34e[ghond$p34e==5] <- "Coahuila"
ghond$p34e[ghond$p34e==6] <- "Chihuahua"
ghond$p34e[ghond$p34e==7] <- "Chiapas"
ghond$p34e[ghond$p34e==8] <- "Colima"

ghond$p34e[ghond$p34e=="Baja California Sur"] <- 3
ghond$p34e[ghond$p34e=="Baja California"] <- 2
ghond$p34e[ghond$p34e=="Coahuila"] <- 7
ghond$p34e[ghond$p34e=="Chihuahua"] <- 8
ghond$p34e[ghond$p34e=="Chiapas"] <- 5
ghond$p34e[ghond$p34e=="Colima"] <- 6

gguate <- guatedev2016mex %>%
  filter(p34e < 40) %>%
  add_tally() %>%
  add_count(p34e) %>%
  distinct(pais, p34e, nn, n)

gguate$percguate <- gguate$nn / gguate$n

gels <- elsdev2016mex %>%
  filter(p34e < 40) %>%
  add_tally() %>%
  add_count(p34e) %>%
  distinct(pais, p34e, nn, n)

gels$percels <- gels$nn / gels$n

# Map

to_map_hond <- geo_join(mex_shape, ghond, by_sp = 'ID_1', by_df='p34e', how='left')
to_map_hond[is.na(to_map_hond)] <- 0

mario_theme <- theme(text = element_text(family='Georgia'),
                     plot.title = element_text(size = 20, margin = margin(b = 10)),
                     plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 25)), 
                     axis.title.x = element_text(color='darkslategrey', size=8),
                     axis.title.y = element_text(color='darkslategrey', size=8), 
                     plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0),
                     legend.title = element_text(face='bold', size=8),
                     legend.position = 'bottom', legend.direction = 'horizontal', legend.box = 'horizontal', 
                     legend.key.size = unit(0.5, 'cm'))

labels <- labs(title = 'The majority of Honduran migrants get deported \n on the southern border or along the Gulf',
               subtitle = 'The states where most migrants get deported from are along the traditional migrant route \n which traverses Chiapas, Tabasco, Veracruz, and Tamaulipas',
               caption = 'Source: EMIF, 2016')

updated_theme <- theme(panel.border = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x.top = element_text(size=12)) +
  theme(axis.ticks = element_blank()) + 
  theme(panel.background = element_blank())

ggplot() +
  geom_sf(data = to_map_hond) + 
  geom_sf(data = to_map_hond, aes(fill=perchond)) +
  scale_fill_gradient(low='#feebe2', high='#7a0177') + 
  labels + mario_theme + updated_theme

