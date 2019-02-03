# SPIDER PLOT: Answer to biggest challenge facing the country today

install.packages("ggplot2")
install.packages("tidyverse")
install.packages('ggradar')
install.packages('car')

library(tidyverse)
library(gtools)
library(ggradar)
library(car)

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



final$a4 <- recode(final$a4,"c(5, 12, 14, 31, 27, 57)='Crime/Violence';c(9, 3, 58, 26, 1, 2, 4, 55)='Economy'; c(19, 18, 25, 21, 24, 59, 15, 6, 7, 60) = 'Gov/Services'; c(23, 11, 10, 22, 20, 25)='Health/Social Issues'; c(30, 13, 56, 32, 17, 61, 16, 33)='Corruption/Conflict'")


## Get data ready

sub <- final %>% 
  filter(a4 != 70, a4 != 4704, a4 != 4701, a4 != 4702) %>%
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
  distinct(pais, year, a4, perc) 

sub_to_graph$perc <- sub_to_graph$perc * 100

rm(sub)



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



