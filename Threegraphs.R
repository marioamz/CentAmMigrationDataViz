# LAPOP and Security Data Visualization
## Using ggplot2

install.packages("ggplot2")
install.packages("tidyverse")
install.packages("gghighlight")

library(tidyverse)
library(gtools)
library(gghighlight)

setwd("~/Desktop/Grad School/Data Viz/Migration Project")

els <- read.csv('Data/elsLAPOP.csv')
guate <- read.csv('Data/guateLAPOP.csv')
hond <- read.csv('Data/hondLAPOP.csv', encoding='latin1')
sec <- read.csv('Data/security.csv')

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

## This graph shows the percent of people who've considered emigration per country over time

# Calulcate the percent of people who've considered emigration over time

new <- final %>%
  select(year, pais, q14) %>%
  group_by(year, pais) %>%
  add_tally() %>%
  add_count(q14)

new$perc <- (new$nn / new$n) * 100

f <- new %>%
  select(year, pais, perc, q14) %>%
  group_by(year, pais) %>%
  arrange(q14) %>%    
  slice(1)

# Plot
 
plot <- ggplot(f,aes(x = year, y = perc, color= pais)) + 
  geom_line(stat='identity', width=1) 

plot + labs(title = "More people are planning to emigrate \n from Honduras than ever before",
            subtitle = "Across Central America, plans to emigrate \n have increased substantially since 2010",
            caption = "Source: LAPOP data, 2010-2017", 
            x = "Years", y = "Percent Planning to Emigrate",
            fill='Country') + theme(
              plot.title = element_text(color="black", size=14, face="bold", hjust=0.5),
              plot.subtitle = element_text(color="black", size=12, face="italic", hjust=0.5),
              axis.title.x = element_text(color="black", size=10),
              axis.title.y = element_text(color="black", size=10),
              plot.caption = element_text(color="black", size=8, face="italic"))

ggsave('EmigrationAcrossCountries.pdf', path="Visualizations/", width=9, height=9, units='in')

rm(new, f)




## Will focus on Honduras now that I can see it's the the one with most signifcant
## increases in people saying they will emigrate
## This graph will show perceptions in violence and economic opportunity
## relative to answers on willingness to emigrate in the next 3 years for 2017

hond2017 <- subset(hond, year==2016)

# Calculate the percent of people in each province who said they felt
# "Very unsafe" in the community

unsafe <- hond2017 %>%
  select(year, prov, aoj11) %>%
  group_by(year, prov) %>%
  add_tally() %>%
  add_count(aoj11)

# Calculate the percent of people in each province who said their economic
# situation has worsnened in the last 12 months

econ <- hond2017 %>%
  select(year, prov, idio2) %>%
  group_by(year, prov) %>%
  add_tally() %>%
  add_count(idio2)

# Calculate the percent of people in each province who want to emigrate
# in the next 3 years

emigrate <- hond2017 %>%
  select(year, prov, q14) %>%
  group_by(year, prov) %>%
  add_tally() %>%
  add_count(q14)

# Group by and replace those provinces that don't have those values with zero
# percent

emigrate$perc_emigrate <- (emigrate$nn / emigrate$n) * 100
unsafe$perc_aoj11 <- (unsafe$nn / unsafe$n) * 100
econ$perc_idio2 <- (econ$nn / econ$n) * 100

emigrate.na <- na.omit(emigrate)
unsafe.na <- na.omit(unsafe)
econ.na <- na.omit(econ)

to_plot_emigrate <- emigrate.na %>%
  select(year, prov, perc_emigrate, q14) %>%
  group_by(year, prov) %>%
  arrange(q14) %>%
  slice(1)

to_plot_econ <- econ.na %>%
  select(year, prov, perc_idio2, idio2) %>%
  group_by(year, prov) %>%
  arrange(desc(idio2)) %>%    
  slice(1)

to_plot_unsafe <- unsafe.na %>%
  select(year, prov, perc_aoj11, aoj11) %>%
  group_by(year, prov) %>%
  arrange(desc(aoj11)) %>%    
  slice(1)

to_plot_final <- merge(to_plot_econ, to_plot_unsafe, by = c("year", "prov"))
to_plot_final <- merge(to_plot_final, to_plot_emigrate, by = c("year", "prov"))
to_plot_f <- to_plot_final %>% mutate(perc_aoj11 = ifelse(aoj11 != 4, 0, perc_aoj11))
to_plot_ff <- to_plot_f %>% mutate(perc_idio2 = ifelse(idio2 != 3, 0, perc_idio2))

rm(to_plot_final, to_plot_f, to_plot_econ, to_plot_unsafe, unsafe, econ, unsafe.na, econ.na)                                   

# Plot

plot <- ggplot(to_plot_ff, 
               aes(y = perc_aoj11, x = perc_idio2, color = perc_emigrate)) + geom_point( alpha=0.2) +          
  geom_text_repel(aes(label=prov)) + scale_color_distiller(name = '% planning to emigrate', palette='RdPu', trans='reverse') + 
  scale_y_continuous(limits = c(0, 60)) + expand_limits(x=0)
#expand_limits(x=0, y=0)

plot + labs(title = "In Honduran provinces with largest percentage of people \n planning to emigrate, economic concerns prevail",
            subtitle = "Economics might be playing a larger push role than violence",
            caption = "Source: LAPOP Honduras data 2017", 
            x = "% who feel economic prospects have worsened", y = "% who feel very unsafe from crime",
            fill='Province') + theme(
              plot.title = element_text(color="black", size=14, face="bold", hjust=0.5),
              plot.subtitle = element_text(color="black", size=12, face="italic", hjust=0.5),
              axis.title.x = element_text(color="black", size=10),
              axis.title.y = element_text(color="black", size=10),
              plot.caption = element_text(color="black", size=8, face="italic"),
              legend.justification = c(0.75, 0.75), 
              legend.position = c(0.25,.75), 
              legend.title = element_text(colour="black", size=12, 
                                          face="bold"))

ggsave('EconAndViolenceEmigration.pdf', path="Visualizations/", width=9, height=9, units='in')

rm(emigrate, emigrate.na, hond2017, to_plot_emigrate, to_plot_ff)

## Last graph will be a line graph that tracks American aid to Honduras by program area.

hond_sec <- subset(sec, country_name == 'Honduras')
hond_sec$dac_sector_name[is.na(hond_sec$dac_sector_name)] <- "Conflict, Peace, and Security"

to_plot_sec <- hond_sec %>%
  select(fiscal_year, dac_sector_name, constant_amount) %>%
  group_by(fiscal_year, dac_sector_name) %>%
  tally(constant_amount)

#Plot

plot <- gghighlight_line(to_plot_sec, aes(fiscal_year, n, colour = dac_sector_name), predicate = max(n),
                         max_highlight = 3)

plot + scale_y_continuous(name = "Amount (in 10,000,000)", labels = c(0, 2, 
                                                                      4, 6, 8)) + 
  labs(title = "US Investments in Honduras have focused on civil society \n and government",
       subtitle = "There's notable increases since 2015, the year \n after the unaccompanied minors crisis",
       caption = "Source: USAID Foreign Aid Explorer and Security Assistance Monitor data, 2010-2017", 
       x = "Fiscal Year",
       fill='Province') + theme(
         plot.title = element_text(color="black", size=14, face="bold", hjust = 0.5),
         plot.subtitle = element_text(color="black", size=12, face="italic", hjust =0.5),
         axis.title.x = element_text(color="black", size=10),
         axis.title.y = element_text(color="black", size=10),
         plot.caption = element_text(color="black", size=8, face="italic"))

ggsave('USaidHonduras.pdf', path="Visualizations/", width=9, height=9, units='in')

