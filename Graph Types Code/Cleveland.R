# CLEVELAND PLOT: Percent wanting to emigrate: 2010-2016 Honduran provinces

install.packages("ggplot2")
install.packages("tidyverse")

library(tidyverse)
library(gtools)

## Read in data
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

## Subset to have emigration question (aoj11)

subemigrate <- hond %>%
  select(year, prov, q14) %>%
  filter(year != 2012, year != 2014, prov != 414, prov != 415, prov != 416, prov != 417) %>%
  na.omit() %>%
  group_by(year, prov) %>%
  add_tally() %>%
  add_count(q14) %>%
  distinct(year, prov, q14, n, nn)

subemigrate$perc <- (subemigrate$nn / subemigrate$n) * 100

emigrate <- subemigrate %>%
  select(year, prov, perc, q14) %>%
  group_by(year, prov) %>%
  arrange(q14) %>%
  slice(1)  %>%
  select(year, prov, perc)



## Stupid encodings don't work

emigrate$prov[emigrate$prov=='AtlÃ¡ntida'] <- "Atlántida"
emigrate$prov[emigrate$prov=='ColÃ³n'] <- "Colón"
emigrate$prov[emigrate$prov=='CopÃ¡n'] <- "Copán"
emigrate$prov[emigrate$prov=='CortÃ©s'] <- "Cortés"
emigrate$prov[7] <- "El Paraíso"
emigrate$prov[emigrate$prov=='Francisco MorazÃ¡n'] <- "Francisco Morazán"
emigrate$prov[emigrate$prov=='IntibucÃ¡'] <- "Intibucá"
emigrate$prov[11] <- "Islas de la Bahía"
emigrate$prov[emigrate$prov=='Santa BÃ¡rbara'] <- "Santa Bárbara"

x <- with(emigrate, tapply(perc, list(prov, year) , I)  )
y <- as.data.frame(na.omit(x))
graph_emigrate <- add_rownames(y, "prov")
rm(x, y, subemigrate, emigrate)

colnames(graph_emigrate)[colnames(graph_emigrate)==2010] <- "perc2010"
colnames(graph_emigrate)[colnames(graph_emigrate)==2016] <- "perc2016"

final_emigrate <- graph_emigrate %>%
  arrange(desc(perc2016))

## Plot

ggplot(final_emigrate) +
  geom_segment( aes(x=prov, xend=prov, y=perc2010, yend=perc2016), color="grey") +
  geom_point( aes(x=prov, y=perc2010), color=rgb(0.2,0.7,0.1,0.5), size=3 ) +
  geom_point( aes(x=prov, y=perc2016), color=rgb(0.7,0.2,0.1,0.5), size=3 ) +
  coord_flip() 


