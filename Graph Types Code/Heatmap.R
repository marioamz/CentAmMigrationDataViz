# Heatmap of percent of people willing to emigrate per country over time
### Might want to consider doing it for provinces

## Read in data
install.packages("ggplot2")
install.packages("tidyverse")

library(tidyverse)
library(gtools)

els <- read.csv('Data/elsLAPOP.csv', encoding = 'latin1')
guate <- read.csv('Data/guateLAPOP.csv', encoding='latin1')
hond <- read.csv('Data/hondLAPOP.csv', encoding='latin1')

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

## Get the data ready: Year, Country, and Percent Wanting to Emigrate

inter <- final %>% 
  select(year, pais, q14) %>%
  group_by(year, pais) %>%
  add_tally() %>%
  add_count(q14)

inter$perc <- (inter$nn / inter$n) * 100

to_graph <- inter %>%
  select(year, pais, perc, q14) %>%
  group_by(year, pais) %>%
  arrange(q14) %>%    
  slice(1)

## Put into a heatmap

mario_theme <- theme(text = element_text(family='sans'),
                     plot.title = element_text(color='#7a0177', size=14, face='bold'),
                     plot.subtitle = element_text(color='black', size=12, face='italic'), 
                     axis.title.x = element_text(color='black', size=10),
                     axis.title.y = element_text(color='black', size=10), 
                     plot.caption = element_text(color='grey', size=8),
                     legend.title = element_text(face='bold', size=8),
                     legend.position = 'bottom', legend.direction = 'horizontal', legend.box = 'horizontal', 
                     legend.key.size = unit(0.5, 'cm')) 

mario_scale_cont <- scale_fill_gradient(low='#feebe2', high='#7a0177')

plot <- ggplot(to_graph, aes(x=as.factor(year), y=pais, fill=perc)) + geom_tile(aes(fill=perc)) + geom_text(aes(label=round(perc, 1), fontface='bold')) + mario_scale_cont 
labels <- labs(title = 'Since 2012, the percentage of Honduran residents planning to emigrate \n has increased nearly 30%',
               subtitle = 'Across the region, there are notable increases in plans to emigrate since 2012',
               caption = 'Source: LAPOP 2010-2016',
               x = 'Year',
               y = 'Country',
               fill = '% planning to emigrate')
update_theme <- theme(axis.title.x = element_blank(),
                      axis.title.y = element_blank(),
                      panel.grid = element_blank(), 
                      axis.ticks = element_blank())

plot + labels + mario_theme + update_theme

