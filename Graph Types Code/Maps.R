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

# Read in departmental level shape file 
honduras_shape <- st_read('Data/HND_adm/HND_adm1.shp')
guate_shape <- st_read('Data/gadm36_GTM_shp/gadm36_GTM_1.shp')
els_shape <- st_read('Data/gadm36_SLV_shp/gadm36_SLV_1.shp')

# Read in Honduras data
els <- read.csv('Data/elsLAPOP.csv', encoding = 'latin1', stringsAsFactors = FALSE)
guate <- read.csv('Data/guateLAPOP.csv', encoding='latin1', stringsAsFactors = FALSE)
hond <- read.csv('Data/hondLAPOP.csv', encoding='latin1', stringsAsFactors = FALSE)

els$pais[els$pais==3] <- "El Salvador"
guate$pais[guate$pais==2] <- "Guatemala"
hond$pais[hond$pais==4] <- "Honduras"

cols_to_keep <- intersect(colnames(els), colnames(guate))
g <- guate[, cols_to_keep, drop=FALSE]
e <- els[, cols_to_keep, drop=FALSE]
bind <- smartbind(g, e)
cols_to_keep <- intersect(colnames(bind), colnames(hond))
h <- hond[, cols_to_keep, drop=FALSE]
b <- bind[, cols_to_keep, drop=FALSE]
final <- smartbind(h, b)

rm(cols_to_keep, g, e, h, b, bind)

# Subset to determine what changes are over time of people wanting to emigrate

subemigrate <- final %>%
  select(year, pais, prov, q14) %>%
  filter(year != 2012, year != 2014, year != 2010, prov != 414, prov != 415, prov != 416, prov != 417, prov != 219) %>%
  na.omit() %>%
  group_by(year, pais, prov) %>%
  add_tally() %>%
  add_count(q14) %>%
  distinct(year, pais, prov, q14, n, nn)

subemigrate$perc <- (subemigrate$nn / subemigrate$n) * 100

emigrate <- subemigrate %>%
  select(year, pais, prov, perc, q14) %>%
  group_by(year, pais, prov) %>%
  arrange(q14) %>%
  slice(1)  %>%
  select(year, pais, prov, perc) %>%
  group_by(year, pais, prov) %>%
  arrange(desc(perc))

emigrate$prov[emigrate$prov=='AtlÃ¡ntida'] <- "Atlántida"
emigrate$prov[emigrate$prov=='ColÃ³n'] <- "Colón"
emigrate$prov[emigrate$prov=='CopÃ¡n'] <- "Copán"
emigrate$prov[emigrate$prov=='CortÃ©s'] <- "Cortés"
emigrate$prov[99] <- "El Paraíso"
emigrate$prov[emigrate$prov=='Francisco MorazÃ¡n'] <- "Francisco Morazán"
emigrate$prov[emigrate$prov=='IntibucÃ¡'] <- "Intibucá"
emigrate$prov[19] <- "Islas de la Bahía"
emigrate$prov[emigrate$prov=='Santa BÃ¡rbara'] <- "Santa Bárbara"
emigrate$prov[emigrate$prov=='SONSONATE'] <- "Sonsonate"
emigrate$prov[emigrate$prov=='LA UNION'] <- "La Unión"
emigrate$prov[emigrate$prov=='CUSCATLAN'] <- "Cuscatlán"
emigrate$prov[emigrate$prov=='CABAÑAS'] <- "Cabañas"
emigrate$prov[emigrate$prov=='QuichÃ©'] <- "Quiché"
emigrate$prov[emigrate$prov=='SAN VICENTE'] <- "San Vicente"
emigrate$prov[emigrate$prov=='LA LIBERTAD'] <- "La Libertad"
emigrate$prov[emigrate$prov=='SANTA ANA'] <- "Santa Ana"
emigrate$prov[emigrate$prov=='CuscatlÃ¡n'] <- "Cuscatlán"
emigrate$prov[emigrate$prov=='LA PAZ'] <- "La Paz"
emigrate$prov[emigrate$prov=='AHUACHAPAN'] <- "Ahuachapán"
emigrate$prov[emigrate$prov=='SAN MIGUEL'] <- "San Miguel"
emigrate$prov[emigrate$prov=='USULUTAN'] <- "Usulután"
emigrate$prov[emigrate$prov=='MORAZAN'] <- "Morazán"
emigrate$prov[emigrate$prov=='La UniÃ³n'] <- "La Unión"
emigrate$prov[emigrate$prov=='UsulutÃ¡n'] <- "Usulután"
emigrate$prov[emigrate$prov=='SuchitepÃ©quez'] <- "Suchitepéquez"
emigrate$prov[emigrate$prov=='CHALATENANGO'] <- "Chalatenango"
emigrate$prov[emigrate$prov=='CabaÃ±as'] <- "Cabañas"
emigrate$prov[emigrate$prov=='SacatepÃ©quez'] <- "Sacatepéquez"
emigrate$prov[emigrate$prov=='TotonicapÃ¡n'] <- "Totonicapán"
emigrate$prov[emigrate$prov=='AhuachapÃ¡n'] <- "Ahuachapán"
emigrate$prov[emigrate$prov=='SololÃ¡'] <- "Sololá"
emigrate$prov[emigrate$prov=='MorazÃ¡n'] <- "Morazán"
emigrate$prov[emigrate$prov=='PetÃ©n'] <- "Petén"
emigrate$prov[emigrate$prov=='SAN SALVADOR'] <- "San Salvador"


diff <- emigrate %>%
  spread(year, perc) %>%
  group_by(prov)

colnames(diff)[colnames(diff)==2016] <- "perc2016"

diff_use <- diff[36:53,]

to_map_hond <- honduras_shape %>%
  stringdist_inner_join(diff_use, by=c(NAME_1='prov'), max_dist=1)
to_map_hond <- geo_join(honduras_shape, diff_use, by_sp = 'NAME_1', by_df='prov', how='left')
#to_map_guate <- geo_join(guate_shape, diff, by_sp='NAME_1', by_df='prov', how='left')
#to_map_els <- geo_join(els_shape, diff_use, by_sp='NAME_1', by_df='prov', how='left')

mario_theme <- theme(text = element_text(family='Georgia'),
                     plot.title = element_text(size = 20, margin = margin(b = 10)),
                     plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 25)), 
                     axis.title.x = element_text(color='darkslategrey', size=8),
                     axis.title.y = element_text(color='darkslategrey', size=8), 
                     plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0),
                     legend.title = element_text(face='bold', size=8),
                     legend.position = 'bottom', legend.direction = 'horizontal', legend.box = 'horizontal', 
                     legend.key.size = unit(0.5, 'cm'))

labels <- labs(title = 'During 2016, more residents in northeastern Honduran \n provinces planned to emigrate than anywhere else',
subtitle = 'In the northeastern provinces of Colón and Gracias a Dios more than 50% of \n respondents indicated plans to emigrate in the next three years',
caption = 'Source: LAPOP Honduras, 2010-2016')

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
  geom_sf(data = to_map_hond, aes(fill=perc2016)) +
  gghighlight(perc2016 > 40) + 
  scale_fill_gradient(low='#feebe2', high='#7a0177') + 
  labels + mario_theme + updated_theme


#+
 # geom_sf(data = to_map_guate) +
  #geom_sf(data = to_map_guate, aes(fill=change)) + 
  #geom_sf(data = to_map_els) + 
  #geom_sf(data = to_map_els, aes(fill=change))
