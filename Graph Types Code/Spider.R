# SPIDER PLOT: Answer to biggest challenge facing the country today

install.packages("ggplot2")
install.packages("tidyverse")
install.packages('ggradar')

library(tidyverse)
library(gtools)
library(ggradar)

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

## Get data ready

sub <- final %>% 
  select(pais, year, a4) %>%
  na.omit() %>%
  group_by(pais, year) %>% 
  add_tally %>% 
  add_count(a4)

sub$perc <- sub$nn / sub$n

sub_to_graph <- sub %>%
  select(pais, year, a4, perc) %>%
  group_by(pais, year) %>%
  arrange(desc(perc),.by_group=TRUE) %>%
  distinct(pais, year, a4, perc) %>%
  slice(1:3) 

sub_to_graph$perc <- sub_to_graph$perc * 100

sub_to_graph$a4[sub_to_graph$a4 == 5] <- 'Crime'
sub_to_graph$a4[sub_to_graph$a4 == 1] <- 'Economy'
sub_to_graph$a4[sub_to_graph$a4 == 4] <- 'Poverty'
sub_to_graph$a4[sub_to_graph$a4 == 57] <- 'Violence'
sub_to_graph$a4[sub_to_graph$a4 == 3] <- 'Unemployment'
sub_to_graph$a4[sub_to_graph$a4 == 70] <- 'Other'
sub_to_graph$a4[sub_to_graph$a4 == 14] <- 'Gangs'
sub_to_graph$a4[sub_to_graph$a4 == 27] <- 'Security'
sub_to_graph$a4[sub_to_graph$a4 == 13] <- 'Corruption'
sub_to_graph$a4[sub_to_graph$a4 == 18] <- 'Roads'





to_graph <- dcast(sub_to_graph, pais~a4)

rm(sub, sub_to_graph)

to_graph[is.na(to_graph)] <- 0
cols <- sapply(to_graph, is.numeric)
to_graph[, cols] <- to_graph[, cols] * 100
rm(cols)



##Spider graph

coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

ggplot(sub_to_graph, aes(x = a4, y = perc)) +
  geom_polygon(aes(group = pais, color = pais,fill = pais),alpha=0.4, size = 1, show.legend = TRUE) +
  xlab("") + ylab("") +scale_y_discrete(limits = c(0, 30), breaks = seq(0, 30, 10))+
  coord_radar()+
  facet_wrap(~year) +
  guides(fill = guide_legend(keywidth = rel(1.3), keyheight = rel(1.3)))



