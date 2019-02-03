# WAFFLE PLOT

install.packages("ggplot2")
install.packages("tidyverse")
install.packages('car')
install.packages('waffle')

library(tidyverse)
library(gtools)
library(car)
library(waffle)

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

# Start plot

waffle(sub_to_graph$perc *100, 
       rows = 10, 
       size=0.5, 
       colors=c("#cc0000", "#ff9900", "#ff6699", "#6699ff", "#006666", "#33cc33"), 
       title="All NCBI Taxonomy Nodes by Kingdom", 
       xlab="1 square is 1,000 nodes.")
