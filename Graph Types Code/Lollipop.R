#LOLLIPOP: Percent wanting to emigrate from Honduran provinces 2010-2016

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
  select(year, prov, perc) %>%
  group_by(year, prov) %>%
  arrange(desc(perc))



## Stupid encodings don't work

emigrate$prov[emigrate$prov=='AtlÃ¡ntida'] <- "Atlántida"
emigrate$prov[emigrate$prov=='ColÃ³n'] <- "Colón"
emigrate$prov[emigrate$prov=='CopÃ¡n'] <- "Copán"
emigrate$prov[emigrate$prov=='CortÃ©s'] <- "Cortés"
emigrate$prov[33] <- "El Paraíso"
emigrate$prov[emigrate$prov=='Francisco MorazÃ¡n'] <- "Francisco Morazán"
emigrate$prov[emigrate$prov=='IntibucÃ¡'] <- "Intibucá"
emigrate$prov[11] <- "Islas de la Bahía"
emigrate$prov[emigrate$prov=='Santa BÃ¡rbara'] <- "Santa Bárbara"

## Plot

# to add labels to plot
right_label <- emigrate %>%
  filter(year == 2016)

left_label <- emigrate %>%
  filter(year == 2010)

# to highlight only those greater than 30%
diff <- emigrate %>%
  spread(year, perc) %>%
  group_by(prov) 

colnames(diff)[colnames(diff)==2010] <- "perc2010"
colnames(diff)[colnames(diff)==2016] <- "perc2016"

diff_use <- diff %>%
  mutate(change = perc2016 - perc2010)  

highlight <- diff_use %>%
  filter(change > 30)

labels <- labs(title = "Most Honduran provinces saw increases in \n respondents planning to emigrate from 2010 to 2016",
               subtitle = "Out of 18 provinces, three experienced a 30% or greater increase \nin respondents planning to emigrate in the next three years.",
               caption = 'Source: LAPOP data 2010-2016') 

diff_use %>%
  arrange(change) %>%
  ggplot( aes(x=prov, y=change)) +
  geom_segment( aes(x=prov, xend=prov, y=0, yend=change), color="grey", size=1) +
  geom_point( color="grey", size=4, alpha=0.6) +
  geom_segment(data=highlight, aes(x=prov, xend=prov, y=0, yend=change), color = '#df65b0') +
  geom_point(data=highlight, size=4, color = '#67001f') +
  geom_text(data = highlight, aes(label = paste0('+', round(change, 0))),
            size = 3, hjust = -.5, color = '#67001f')  +
  coord_flip() +
  xlab("") +
  ylab("Percent Change from 2010 to 2016") + labels + theme_minimal() + mario_theme +  theme(panel.grid.major.x = element_blank(),
panel.grid.minor = element_blank())




