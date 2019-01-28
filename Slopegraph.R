# How much has the price for a successful trip changed over time - slopegraph

# Read in data for all countries and years

library(dplyr)
library(ggplot2)
library(gtools)
library(ggrepel)

path <- "Data/"
files <- list.files(path=path, pattern="*usa.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

# Add country identifiers
elsdev2014usa$pais <- 'El Salvadorean migrants'
elsdev2015usa$pais <- 'El Salvadorean migrants'
elsdev2016usa$pais <- 'El Salvadorean migrants'
elsdev2017usa$pais <- 'El Salvadorean migrants'

honddev2014usa$pais <- 'Honduran migrants'
honddev2015usa$pais <- 'Honduran migrants'
honddev2016usa$pais <- 'Honduran migrants'
honddev2017usa$pais <- 'Honduran migrants'

guatedev2014usa$pais <- 'Guatemalan migrants'
guatedev2015usa$pais <- 'Guatemalan migrants'
guatedev2016usa$pais <- 'Guatemalan migrants'
guatedev2017usa$pais <- 'Guatemalan migrants'

# subset dataframes
e2014 <- elsdev2014usa[c('year', 'pais', 'p17_1c', 'p17_1u')]
e2015 <- elsdev2015usa[c('year', 'pais', 'p17_1c', 'p17_1u')]
e2016 <- elsdev2016usa[c('year', 'pais', 'p17_1c', 'p17_1u')]
e2017 <- elsdev2017usa[c('year', 'pais', 'p17_1c', 'p17_1u')]

h2014 <- honddev2014usa[c('year', 'pais', 'p17_1c', 'p17_1u')]
h2015 <- honddev2015usa[c('year', 'pais', 'p17_1c', 'p17_1u')]
h2016 <- honddev2016usa[c('year', 'pais', 'p17_1c', 'p17_1u')]
h2017 <- honddev2017usa[c('year', 'pais', 'p17_1c', 'p17_1u')]

g2014 <- guatedev2014usa[c('year', 'pais', 'p17_1c', 'p17_1u')]
g2015 <- guatedev2015usa[c('year', 'pais', 'p17_1c', 'p17_1u')]
g2016 <- guatedev2016usa[c('year', 'pais', 'p17_1c', 'p17_1u')]
g2017 <- guatedev2017usa[c('year', 'pais', 'p17_1c', 'p17_1u')]

rm(elsdev2014usa, elsdev2015usa, elsdev2016usa, elsdev2017usa, honddev2014usa, honddev2015usa, honddev2016usa, honddev2017usa, guatedev2014usa, guatedev2015usa, guatedev2016usa, guatedev2017usa)

# bind them 
final <- do.call("rbind", list(e2014, e2015, e2016, e2017, h2014, h2015, h2016, h2017, g2014, g2015, g2016, g2017))

# calculate mean of money spent in dollars, after filtering out NA and not dollars

tograph <- final %>%
  group_by(year, pais) %>%
  filter(p17_1c >0, p17_1u == 3) %>%
  summarize(mean_spent = mean(p17_1c)) %>%
  mutate_if(is.numeric, round, 0)

rm(e2014, e2015, e2016, e2017, h2014, h2015, h2016, h2017, g2014, g2015, g2016, g2017)

# Slope graph

ggplot(data = tograph, aes(x = year, y = mean_spent, group = pais)) +
  geom_line(aes(color = pais, alpha = 1), size = 1) + 
  geom_text_repel(data = tograph %>% filter(year == 2017), 
                  aes(label = pais) , 
                  hjust = "right", 
                  fontface = "bold", 
                  size = 3.5, 
                  nudge_x = -0.25, 
                  direction = "y") + 
  geom_label(aes(label = paste0("$", mean_spent)), 
             size = 3, 
             label.padding = unit(0.05, "lines"), 
             label.size = 0.0) +
  # move the x axis labels up top
  #scale_x_discrete(position = "top") +
  theme_bw() +
  # Format tweaks
  # Remove the legend
  theme(legend.position = "none") +
  # Remove the panel border
  theme(panel.border     = element_blank()) +
  # Remove just about everything from the y axis
  theme(axis.title.y     = element_blank()) +
  theme(axis.text.y      = element_blank()) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x     = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x.top      = element_text(size=12)) +
  # Remove x & y tick marks
  theme(axis.ticks       = element_blank()) +
  # Format title & subtitle
  theme(plot.title       = element_text(size=14, face = "bold", hjust = 0.5)) +
  theme(plot.subtitle    = element_text(hjust = 0.5)) +
  #  Labelling as desired
  labs(
    title = "Price paid by Honduran migrants to the coyotes successfully \n trafficking them through Mexico has increased since 2014",
    subtitle = "Guatemalan migrants have experienced similar increases, \n but Salvadorean migrants have paid less for a successful trip",
    caption = "Source: EMIF Sur (2014-2017)"
  )

ggsave('Slope.pdf', path="Visualizations/", width=11, height=6, units='in')

