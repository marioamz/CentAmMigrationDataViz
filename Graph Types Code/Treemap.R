# Treemap graph 
## Funding for Honduras 2017

install.packages('tidyverse')
install.packages('treemapify')

library(tidyverse)
library(treemapify)

sec <- read.csv('Data/security.csv')
hond_sec <- subset(sec, country_name == 'Honduras')
usaid_hond <- na.omit(hond_sec)

## Prep data

uh2017 <- usaid_hond %>%
  filter(fiscal_year == 2017) %>%
  select(dac_sector_name, dac_purpose_name, current_amount) %>%
  group_by(dac_sector_name, dac_purpose_name) %>%
  summarise(total = sum(current_amount))

## Plot

mario_theme <- theme(plot.title = element_text(color='#7a0177', size=14, face='bold'),
                     plot.subtitle = element_text(color='black', size=12, face='italic'), 
                     axis.title.x = element_text(color='black', size=10),
                     axis.title.y = element_text(color='black', size=10), 
                     plot.caption = element_text(color='grey', size=8),
                     legend.title = element_text(face='bold', size=8),
                     legend.position = 'bottom', legend.direction = 'horizontal', legend.box = 'horizontal', 
                     legend.key.size = unit(0.5, 'cm'),
                     panel.background = element_blank()) 

plot <- ggplot(uh2017, aes(area = total, label = dac_sector_name, subgroup = dac_purpose_name)) + geom_treemap()
plot          
