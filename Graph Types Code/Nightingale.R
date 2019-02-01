# Nightingale attempt
## Challenges faced by Honduran migrants as they cross Mexico (over time)

install.packages("ggplot2")
install.packages("tidyverse")

library(tidyverse)
library(gtools)

hmex <- read.csv('Data/honddev2017mex.csv')
hmex$pais <- 'Honduras'
hmex$deported <- 'Mexico'

husa <- read.csv('Data/honddev2017usa.csv')
husa$pais <- 'Honduras'
husa$deported <- 'USA'

gmex <- read.csv('Data/guatedev2017mex.csv')
gmex$pais <- 'Guatemala'
gmex$deported <- 'Mexico'

gusa <- read.csv('Data/guatedev2017usa.csv')
gusa$pais <- 'Guatemala'
gusa$deported <- 'USA'

emex <- read.csv('Data/elsdev2017mex.csv')
emex$pais <- 'El Salvador'
emex$deported <- 'Mexico'

eusa <- read.csv('Data/elsdev2017usa.csv')
eusa$pais <- 'El Salvador'
eusa$deported <- 'USA'

usa1 <- smartbind(eusa, gusa)
usa <- smartbind(usa1, husa)
rm(usa1)

mex1 <- smartbind(emex, gmex)
mex <- smartbind(mex1, hmex)
rm(mex1)

## Get data into percentages of each event
usa_g <- usa %>% 
  select(sexo, pais, p18_1, p18_2, p18_3, p18_4, p18_5, p18_6, p18_7, p18_8, p18_9, p18_10, p18_11, p18_12, p18_13) %>%
  group_by(sexo, pais) %>%
  filter(p18_1 > 0, p18_2 > 0, p18_3 >0, p18_4 > 0, p18_5 > 0, p18_6 > 0, p18_7 > 0, p18_8 > 0, p18_9 > 0, p18_10 > 0, p18_11 > 0, p18_12 > 0, p18_13 > 0) %>%
  filter(p18_1 < 10, p18_2 < 10, p18_3 <10, p18_4 < 10, p18_5 < 10, p18_6 < 10, p18_7 < 10, p18_8 < 10, p18_9 < 10, p18_10 < 10, p18_11 < 10, p18_12 < 10, p18_13 < 10)
usa_g[usa_g==2] <- 0

# since it's 0 and 1, a mean will work
usa_perc <- usa_g %>%
  group_by(sexo, pais) %>%
  summarise_all(mean) %>%
  select(sexo, pais, p18_1, p18_2, p18_6, p18_8, p18_9) 

# Rename columns
colnames(usa_perc)[colnames(usa_perc)=="p18_1"] <- "Extreme Cold"
colnames(usa_perc)[colnames(usa_perc)=="p18_2"] <- "Lack of Food"
colnames(usa_perc)[colnames(usa_perc)=="p18_6"] <- "Lost"
colnames(usa_perc)[colnames(usa_perc)=="p18_8"] <- "Abandoned by Coyote"
colnames(usa_perc)[colnames(usa_perc)=="p18_9"] <- "Assault or Robbery"

# Melt
library(reshape2)
melt <- melt(usa_perc, id.vars=c('sexo', 'pais'))

# Nightingale graph

mario_theme <- theme(plot.title = element_text(color='#7a0177', size=14, face='bold'),
                     plot.subtitle = element_text(color='black', size=12, face='italic'), 
                     axis.title.x = element_text(color='black', size=10),
                     axis.title.y = element_text(color='black', size=10), 
                     plot.caption = element_text(color='grey', size=8),
                     legend.title = element_text(face='bold', size=8),
                     legend.position = 'bottom', legend.direction = 'horizontal', legend.box = 'horizontal', 
                     legend.key.size = unit(0.5, 'cm'),
                     panel.background = element_blank()) 
plot <- ggplot(melt, aes(x=variable, y=value, fill=pais)) + geom_bar(stat='identity') + 
  coord_polar() +
  scale_fill_brewer(palette='RdPu') + xlab("") + ylab("")

plot + mario_theme

