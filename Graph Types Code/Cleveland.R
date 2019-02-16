# CLEVELAND PLOT: Percent wanting to emigrate: 2010-2016 Honduran provinces

install.packages("ggplot2")
install.packages("tidyverse")

library(tidyverse)
library(gtools)

## Read in data
hond <- read.csv('Data/hondLAPOP.csv', encoding='latin1')
hond$pais[hond$pais==4] <- "Honduras"
hond2016 <- hond %>%
  filter(year == 2016)

## Subset to have just the variables we care about

touse <- hond2016 %>%
  select(q14, ur, q1, q2, idio2, it1, l1, aoj11, aoj17, aoj12, ed) %>%
  na.omit() 

## Create bins
### For age, 1 is less than 35 and 2 is greater than 35
touse$q2 <- cut(touse$q2, breaks = c(0, 35, Inf), include.lowest=TRUE, labels=c(1, 2))
### For personal economic situation, 1 is worse off than before, 2 is better off
touse$idio2 <- cut(touse$idio2, breaks=c(0, 2, Inf), include.lowest=TRUE, labels=c(2,1))
### For thinking the neighborhood is unsafe, 1 is unsafe and 2 is not
touse$it1 <- cut(touse$it1, breaks=c(0, 2, Inf), include.lowest=TRUE, labels=c(2,1))
### For political leaning, 1 is right wing and 2 is moderate or left wing
touse$l1 <- cut(touse$l1, breaks=c(0, 5, Inf), include.lowest=TRUE, labels=c(2,1))
### For confidence in judicial system if robbed, 1 is not confident, 2 is confident
touse$aoj12 <- cut(touse$aoj12, breaks=c(0, 2, Inf), include.lowest=TRUE, labels=c(2,1))
### For those who finished elementary school, 1 is not finished, 2 is finished
touse$ed <- cut(touse$ed, breaks=c(0, 6, Inf), include.lowest=TRUE, labels=c(1,2))
### Rural is 1, urban is 2
touse$ur<-as.numeric(touse$ur) 

## Let's get percentages of all 
percs <- touse %>%
  group_by(q14) %>%
  add_tally() %>%
  add_count(q2) 
percs$age <- (percs$nn / percs$n) * 100

percs <- percs %>%
  select (-c(n, nn)) %>%
  group_by(q14) %>%
  add_tally() %>%
  add_count(ur)
percs$rural <- (percs$nn / percs$n) * 100

percs <- percs %>%
  select (-c(n, nn)) %>%
  group_by(q14) %>%
  add_tally() %>%
  add_count(q1)
percs$gender <- (percs$nn / percs$n) * 100

percs <- percs %>%
  select (-c(n, nn)) %>%
  group_by(q14) %>%
  add_tally() %>%
  add_count(idio2)
percs$econ <- (percs$nn / percs$n) * 100

percs <- percs %>%
  select (-c(n, nn)) %>%
  group_by(q14) %>%
  add_tally() %>%
  add_count(it1)
percs$hood <- (percs$nn / percs$n) * 100

percs <- percs %>%
  select (-c(n, nn)) %>%
  group_by(q14) %>%
  add_tally() %>%
  add_count(l1)
percs$politics <- (percs$nn / percs$n) * 100

percs <- percs %>%
  select (-c(n, nn)) %>%
  group_by(q14) %>%
  add_tally() %>%
  add_count(aoj12)
percs$justice <- (percs$nn / percs$n) * 100

percs <- percs %>%
  select (-c(n, nn)) %>%
  group_by(q14) %>%
  add_tally() %>%
  add_count(ed)
percs$education <- (percs$nn / percs$n) * 100


## Pull out only those who answered 1 per question

rural <- percs %>%
  group_by(q14) %>%
  distinct(q14, ur, rural) %>%
  slice(1) %>%
  select(q14, rural)

age <- percs %>%
  group_by(q14) %>%
  distinct(q14, q2, age) %>%
  arrange(q2) %>%
  slice(1) %>%
  select(q14, age)

econ <- percs %>%
  group_by(q14) %>%
  distinct(q14, idio2, econ) %>%
  arrange(desc(idio2)) %>%
  slice(1) %>%
  select(q14, econ)

politics <- percs %>%
  group_by(q14) %>%
  distinct(q14, l1, politics) %>%
  arrange(desc(l1)) %>%
  slice(1) %>%
  select(q14, politics)

neighborhood <- percs %>%
  group_by(q14) %>%
  distinct(q14, it1, hood) %>%
  arrange(desc(it1)) %>%
  slice(1) %>%
  select(q14, hood)

confidence <- percs %>%
  group_by(q14) %>%
  distinct(q14, aoj12, justice) %>%
  arrange(desc(aoj12)) %>%
  slice(1) %>%
  select(q14, justice)

elementary <- percs %>%
  group_by(q14) %>%
  distinct(q14, ed, education) %>%
  arrange(ed) %>%
  slice(1) %>%
  select(q14, education)



percentages <- list(age, confidence, econ, elementary, neighborhood, politics, rural) %>%
  reduce(left_join, by='q14')

percentages <- as.data.frame(t(percentages))
colnames(percentages) = percentages[1, ] # the first row will be the header
percentages = percentages[-1, ]   
names(percentages) <- c("emigrate", "not_emigrate")
percentages <- cbind(answers = rownames(percentages), percentages)
rownames(percentages) <- 1:nrow(percentages)

percentages <- percentages %>%
  mutate(change = emigrate - not_emigrate) %>%
  arrange(desc(change))
  

emigratepos <- percentages %>%
  filter(change > 0) %>%
  arrange(desc(change))

emigrateneg <- percentages %>%
  filter(change < 0) %>%
  arrange(desc(change))

percentages$answers <- factor(percentages$answers, levels = percentages$answers[order(percentages$change)])

colors <- c('c1' = '#67001f', 'c2' = '#df65b0')
# Actually plot

plot <- ggplot(percentages, aes(emigrate, answers)) +
  geom_point(data = emigratepos, aes(emigrate),color = '#67001f', size =4) + 
  geom_point(data = emigrateneg, aes(emigrate), color = '#67001f', size =4) + 
  geom_point(data = emigratepos, aes(not_emigrate), color = '#df65b0', size=4) +
  geom_point(data = emigrateneg, aes(not_emigrate), color = '#df65b0', size=4) +
  geom_text(data = emigratepos, aes(label=paste0(round(emigrate, 0), '%')), color = '#67001f', hjust = -1, size = 3,
            position = position_dodge(width = 1),
            inherit.aes = TRUE) +
  geom_text(data = emigrateneg, aes(label=paste0(round(emigrate, 0), '%')), color = '#67001f', hjust = 2, size = 3,
            position = position_dodge(width = 1),
            inherit.aes = TRUE) +
  geom_segment(data = emigratepos, aes(x=emigrate, xend=not_emigrate, y=answers, yend=answers), color = '#67001f', size=1) + 
  geom_segment(data = emigrateneg, aes(x=emigrate, xend=not_emigrate, y=answers, yend=answers), color = '#df65b0', size=1) +
  expand_limits(x=0) +
  geom_text(data = emigratepos, aes(x = not_emigrate, y= answers, label=paste0(round(not_emigrate, 0), '%')), color = '#df65b0', hjust = 1.5, size = 3,
            position = position_dodge(width = 1)) +
  geom_text(data = emigrateneg, aes(x = not_emigrate, y=answers, label=paste0(round(not_emigrate, 0), '%')), color = '#df65b0', hjust = -1.25, size = 3,
            position = position_dodge(width = 1)) +
  geom_label(data = emigratepos, aes(label=c('% who are younger than 35',
                                             '% who do not trust neighbors', 
                                             '% whose economic situation has gotten worse in last year', 
                                             '% who do not trust the justice system to prosecute a robbery')), position=position_nudge(-35), colour = '#67001f', fontface = 'bold') +
  geom_label(data = emigrateneg, aes(label=c('% who live in rural communities',
                                             '% who self-identify as conservative', 
                                             '% who did not complete elementary school ')), position=position_nudge(-30), colour = '#df65b0', fontface = 'bold') +
  scale_color_manual(name="Hondurans", 
                     breaks=c('c1', 'c2'), 
                     values = colors,
                     labels = c('Planning to Emigrate', 'Not Planning to Emigrate'))

labels <- labs(title = "Hondurans planning to emigrate (dark purple) are more \n likely report living in unsafe, gang-filled neighborhoods \n than those who don't (light purple)",
     subtitle = "Surprisingy, those who plan to emigrate are more likely to have finished elementary school, live in a city, be younger, \n and be less politically conservative than those who don't.") 

update_themes <- 
  theme(axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())

mario_theme <- theme(text = element_text(family='Georgia'),
                     plot.title = element_text(size = 20, margin = margin(b = 10)),
                     plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 25)), 
                     #axis.title.x = element_text(color='darkslategrey', size=8),
                     #axis.title.y = element_text(color='darkslategrey', size=8), 
                     #plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0),
                     #legend.title = element_text(face='bold', size=8),
                     #legend.position = 'bottom', legend.direction = 'horizontal', legend.box = 'horizontal', 
                     legend.key.size = unit(0.5, 'cm'))

plot + labels  + mario_theme + update_themes

