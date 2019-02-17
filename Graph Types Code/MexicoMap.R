install.packages('sf')
install.packages('tigris')
install.packages('gghighlight')
install.packages('fuzzyjoin')
install.packages('ggmap')
install.packages('gganimate')
install.packages('gifski')
install.packages('ggrepel')
library(gifski)
library(gganimate)
library(ggmap)
library(fuzzyjoin)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(tigris)
library(gtools)
library(gghighlight)
library(ggrepel)

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

final$geocode <- paste(final$Descripción.x,",",final$Descripción.y)

geocodes <- geocode(as.character(final$geocode))
final <- data.frame(final,geocodes)
final$perc = (final$nn / final$n) * 100


highlight <- final %>%
  filter(perc > 1.5)


joined <- mex_shape %>%
  stringdist_inner_join(final, by=c(NAME_2='Descripción.x', NAME_1='Descripción.y'), max_dist=1)

to_map <- st_simplify(joined)

to_mapa_laea <- st_transform(to_map, 6372)

# Map

labels <- labs(title = 'The majority of Honduran migrants get deported \n on the southern border or along the Gulf',
               subtitle = 'The states where most migrants get deported from are along the traditional migrant route \n which traverses Chiapas, Tabasco, Veracruz, and Tamaulipas',
               caption = 'Source: EMIF, 2016')

updated_theme <- theme(panel.border = element_blank()) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_blank()) +
  theme(axis.text.x = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x.top = element_text(size=12)) +
  theme(axis.ticks = element_blank()) + 
  theme(panel.background = element_blank())

p <- ggplot() +
  geom_sf(data=to_mapa_laea)
  geom_point(data = final, aes(x = lon, y = lat, size = perc), color = 'grey') +
  geom_point(data = highlight, aes(x = lon, y = lat, size = perc, colour=pais)) + 
  geom_text_repel(data = highlight, aes(x= lon, y=lat, label = Descripción.x)) +
  mario_theme + labels + updated_theme

p + transition_states(pais)
