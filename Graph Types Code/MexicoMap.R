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
    read.csv(paste(path,file,sep=""), stringsAsFactors = FALSE))
}

elsdev2016mex$pais <- 'El Salvador'
honddev2016mex$pais <- 'Honduras'
guatedev2016mex$pais <- 'Guatemala'

values <- read.csv('Values/Els_Valores.csv')
values_city <- values %>%
  filter(Variable == 'p34m')
values_state <- values %>%
  filter(Variable == 'p34e')
# Pull out where people deported from p34m

honddev2016mex <- honddev2016mex[c('pais', 'p34m', 'p34e')]
guatedev2016mex <- guatedev2016mex[c('pais', 'p34m', 'p34e')]
elsdev2016mex <- elsdev2016mex[c('pais', 'p34m', 'p34e')]

# Group so that each row is a state, and % of people found from each country as columns

ghond <- honddev2016mex %>%
  filter(p34m > 0 & p34m < 40000) %>%
  add_tally() %>%
  add_count(p34m) %>%
  distinct(pais, p34m, p34e, nn, n)

gguate <- guatedev2016mex %>%
  filter(p34m > 0 & p34m < 40000) %>%
  add_tally() %>%
  add_count(p34m) %>%
  distinct(pais, p34m, p34e, nn, n)

gels <- elsdev2016mex %>%
  filter(p34m > 0 & p34m < 40000) %>%
  add_tally() %>%
  add_count(p34m) %>%
  distinct(pais, p34m, p34e, nn, n)

final <- do.call("rbind", list(gels, gguate, ghond))

final <- merge(final, values_city, by.x='p34m', by.y='Valor')
final <- merge(final, values_state, by.x='p34e', by.y = 'Valor')

final <- data.frame(lapply(final, as.character), stringsAsFactors=FALSE)
mex_shape <- mex_shape %>% mutate_if(is.factor, as.character)

install.packages('fuzzyjoin')
library(fuzzyjoin)

joined <- final %>%
  stringdist_inner_join(mex_shape, by=c(Descripción.x='NAME_2', Descripción.y='NAME_1'), max_dist=1)

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

