# Read in all 2017 data

install.packages('ggalluvial')

library(dplyr)
library(ggplot2)
library(ggalluvial)

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

# 1) Do Central American migrants take different paths to arrive at the United States?

## This is an alluvial graph charting travel from Guatemala border to 
## US-Mexico border for all countries in 2017

### Subsetting the data
alluvial <- usa[c('pais', 'deported', 'p14_1', 'p19_1e', 'p20e', 'p26', 'p35cn')]

### Dropping NA observations for each question
drop1 <- alluvial %>% filter(p14_1 > 0, p19_1e > 0, p20e > 0, p26 > 0, p35cn > 0)
clean <- drop1 %>% filter(p14_1 < 2000000, p19_1e < 40, p20e < 40, p26 < 285000000, p35cn < 58000)
rm(drop1)

### Frequency dataframe
alluvialnodes <- rename(count(clean, pais, p14_1, p19_1e, p20e, p26, p35cn), Freq = n)
touse <- alluvialnodes %>% filter(Freq>5)
touse$Freq <- order(touse$Freq)

#### Get string values
touse$p15[touse$p15 == 1217001] <- 'Tecun Uman'
touse$p15[touse$p15 == 1705099] <- 'Naranjo'
touse$p15[touse$p15 == 1705016] <- 'El Ceibo'
touse$p15[touse$p15 == 1705015] <- 'Bethel'
touse$p15[touse$p15 == 1312047] <- 'La Mesilla'
touse$p15[touse$p15 == 1305024] <- 'Gracias a Dios'
touse$p15[touse$p15 == 1215013] <- 'El Carmen'

touse$p22_1e[touse$p22_1e == 7] <- 'Chiapas'
touse$p22_1e[touse$p22_1e == 27] <- 'Tabasco'

touse$p22_2e[touse$p22_2e == 7] <- 'Chiapas'
touse$p22_2e[touse$p22_2e == 27] <- 'Tabasco'
touse$p22_2e[touse$p22_2e == 30] <- 'Veracruz'
touse$p22_2e[touse$p22_2e == 28] <- 'Tamaulipas'
touse$p22_2e[touse$p22_2e == 24] <- 'San Luis Potosi'
touse$p22_2e[touse$p22_2e == 21] <- 'Puebla'
touse$p22_2e[touse$p22_2e == 20] <- 'Oaxaca'
touse$p22_2e[touse$p22_2e == 19] <- 'Nuevo Leon'
touse$p22_2e[touse$p22_2e == 11] <- 'Guanajuato'
touse$p22_2e[touse$p22_2e == 9] <- 'Mexico City'

touse$p34e[touse$p34e == 7] <- 'Chiapas'
touse$p34e[touse$p34e == 27] <- 'Tabasco'
touse$p34e[touse$p34e == 30] <- 'Veracruz'
touse$p34e[touse$p34e == 28] <- 'Tamaulipas'
touse$p34e[touse$p34e == 24] <- 'San Luis Potosi'
touse$p34e[touse$p34e == 21] <- 'Puebla'
touse$p34e[touse$p34e == 20] <- 'Oaxaca'
touse$p34e[touse$p34e == 19] <- 'Nuevo Leon'
touse$p34e[touse$p34e == 11] <- 'Guanajuato'
touse$p34e[touse$p34e == 9] <- 'Mexico City'

ggplot(touse,
       aes(y = Freq,
           axis1 = p14_1, axis2 = p19_1e, axis3 = p20e, axis4 = p26)) +
  geom_alluvium(aes(fill = pais),
                width = 0, knot.pos = 0, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE) +
  geom_text(stat = "stratum", label.strata = TRUE, reverse = FALSE) +
  scale_x_continuous(breaks = 1:4, labels = c("Mexico Port of Entry", "Mexican City Visited", "Mexican City Where Spent Most Time", "US Port of Entry")) +
  #coord_flip() +
  ggtitle("Titanic survival by class and sex")


# 2) Has the path changed over years for Honduran migrants?

## This is an alluvial graph of Honduran migrants from 2014 to 2017,
## where each flow is a year of migrants. 

### Read in 2014, 2015, 2017 Honduras data

husa14 <- read.csv('Data/honddev2014usa.csv')
husa15 <- read.csv('Data/honddev2015usa.csv')
husa16 <- read.csv('Data/honddev2016usa.csv')
husa17 <- read.csv('Data/honddev2017usa.csv')

h17 <- husa17[c('year', 'p14_1', 'p19_1e', 'p20e', 'p26')]
h16 <- husa16[c('year', 'p14_1', 'p19e1', 'p20e', 'p26')]
h15 <- husa15[c('year', 'p14_1', 'p19e1', 'p20e', 'p26')]
h14 <- husa14[c('year', 'p14_1', 'p19e1', 'p20e', 'p25')]

colnames(h14)[colnames(h14)=="p25"] <- "p26"
colnames(h17)[colnames(h17)=="p19_1e"] <- "p19e1"

bind1 <- smartbind(h14, h15)
bind2 <- smartbind(bind1, h16)
hond <- smartbind(bind2, h17)
rm(h14, h15, h16, h17, bind1, bind2)

### Dropping NA observations for each question
drop1 <- hond %>% filter(p14_1 > 0, p19e1 > 0, p20e > 0, p26 > 0)
clean <- drop1 %>% filter(p14_1 < 2000000, p19e1 < 40, p20e < 40, p26 < 285000000)
rm(drop1)

### Frequency dataframe
alluvialnodes <- rename(count(clean, year,p14_1, p19e1, p20e, p26), Freq = n)
touse <- alluvialnodes %>% filter(Freq>5)
touse$Freq <- order(touse$Freq)
rm(clean, alluvialnodes)

#### Get string values
touse$p15[touse$p15 == 1217001] <- 'Tecun Uman'
touse$p15[touse$p15 == 1705099] <- 'Naranjo'
touse$p15[touse$p15 == 1705016] <- 'El Ceibo'
touse$p15[touse$p15 == 1705015] <- 'Bethel'
touse$p15[touse$p15 == 1312047] <- 'La Mesilla'
touse$p15[touse$p15 == 1305024] <- 'Gracias a Dios'
touse$p15[touse$p15 == 1215013] <- 'El Carmen'

touse$p22_1e[touse$p22_1e == 7] <- 'Chiapas'
touse$p22_1e[touse$p22_1e == 27] <- 'Tabasco'

touse$p22_2e[touse$p22_2e == 7] <- 'Chiapas'
touse$p22_2e[touse$p22_2e == 27] <- 'Tabasco'
touse$p22_2e[touse$p22_2e == 30] <- 'Veracruz'
touse$p22_2e[touse$p22_2e == 28] <- 'Tamaulipas'
touse$p22_2e[touse$p22_2e == 24] <- 'San Luis Potosi'
touse$p22_2e[touse$p22_2e == 21] <- 'Puebla'
touse$p22_2e[touse$p22_2e == 20] <- 'Oaxaca'
touse$p22_2e[touse$p22_2e == 19] <- 'Nuevo Leon'
touse$p22_2e[touse$p22_2e == 11] <- 'Guanajuato'
touse$p22_2e[touse$p22_2e == 9] <- 'Mexico City'

touse$p34e[touse$p34e == 7] <- 'Chiapas'
touse$p34e[touse$p34e == 27] <- 'Tabasco'
touse$p34e[touse$p34e == 30] <- 'Veracruz'
touse$p34e[touse$p34e == 28] <- 'Tamaulipas'
touse$p34e[touse$p34e == 24] <- 'San Luis Potosi'
touse$p34e[touse$p34e == 21] <- 'Puebla'
touse$p34e[touse$p34e == 20] <- 'Oaxaca'
touse$p34e[touse$p34e == 19] <- 'Nuevo Leon'
touse$p34e[touse$p34e == 11] <- 'Guanajuato'
touse$p34e[touse$p34e == 9] <- 'Mexico City'

ggplot(touse,
       aes(y = Freq,
           axis1 = p14_1, axis2 = p19e1, axis3 = p20e, axis4 = p26)) +
  geom_alluvium(aes(fill = year),
                width = 0, knot.pos = 0, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/8, reverse = FALSE) +
  geom_text(stat = "stratum", label.strata = TRUE, reverse = FALSE) +
  scale_x_continuous(breaks = 1:4, labels = c("Mexico Port of Entry", "Mexican City Visited", "Mexican City Where Spent Most Time", "US Port of Entry")) +
  #coord_flip() +
  ggtitle("Titanic survival by class and sex")





# 3) Are the paths different for Honduran immigrants that make it to the US versus
# those who don't?

hmex <- read.csv('Data/honddev2017mex.csv')
hmex$pais <- 'Honduras'
hmex$deported <- 'Mexico'

husa <- read.csv('Data/honddev2017usa.csv')
husa$pais <- 'Honduras'
husa$deported <- 'USA'

cols_to_keep_mex <- c('pais', 'deported', 'p15', 'p22_1e', 'p22_1l', 'p22_2e', 'p22_2l', 'p24l', 'p34l')
cols_to_keep_usa <- c('pais', 'deported', 'p14_1', 'p19_1e', 'p19_1l', 'p19_2e', 'p19_2l', 'p20l', 'p26')

husa1 <- husa[cols_to_keep_usa]
hmex1 <- hmex[cols_to_keep_mex]

colnames(husa1)[colnames(husa1)=="p14_1"] <- "p15"
colnames(husa1)[colnames(husa1)=="p19_1e"] <- "p22_1e"
colnames(husa1)[colnames(husa1)=="p19_1l"] <- "p22_1l"
colnames(husa1)[colnames(husa1)=="p19_2e"] <- "p22_2e"
colnames(husa1)[colnames(husa1)=="p19_2l"] <- "p22_2l"
colnames(husa1)[colnames(husa1)=="p20l"] <- "p24l"

finalhond <- dplyr::bind_rows(husa1, hmex1)
finalhond[is.na(finalhond)] <- 0
## Alluvial graph charting travel from Guatemala to Mexico or US for Honduran migrants

drop1 <- finalhond %>% filter(p15 >= 0, p22_1l >= 0, p22_2l >= 0, p26 >= 0, p24l >= 0)
clean <- drop1 %>% filter(p15 < 2000000, p22_1l < 900000000, p22_2l < 900000000, p26 < 300000000, p24l < 9000000000)
rm(drop1)

alluvialnodeshond <- rename(count(clean, deported, p15, p22_1l, p22_2l, p24l, p26, p34l), Freq = n)
touse <- alluvialnodeshond %>% filter(Freq>2)

## Make names for it

touse$p15[touse$p15 == 1217001] <- 'Tecun Uman'
touse$p15[touse$p15 == 1705099] <- 'Naranjo'
touse$p15[touse$p15 == 1705016] <- 'El Ceibo'
touse$p15[touse$p15 == 1705015] <- 'Bethel'
touse$p15[touse$p15 == 1312047] <- 'La Mesilla'

touse$p22_1l[touse$p22_1l == 301930001] <- 'Veracruz, VER'
touse$p22_1l[touse$p22_1l == 300390001] <- 'Coatzacoalcos, VER'
touse$p22_1l[touse$p22_1l == 270170001] <- 'Tenosique de Pino Suarez, TAB'
touse$p22_1l[touse$p22_1l == 270040001] <- 'Villahermosa, TAB'
touse$p22_1l[touse$p22_1l == 240280001] <- 'San Luis Potosi, SLP'
touse$p22_1l[touse$p22_1l == 190390001] <- 'Monterrey, NL'
touse$p22_1l[touse$p22_1l == 70650001] <- 'Palenque, CHIS'
touse$p22_1l[touse$p22_1l == 70270001] <- 'Chiapa de Corzo, CHIS'


touse$p24l[touse$p24l == 300030001] <- 'Acayucan, VER'
touse$p24l[touse$p24l == 280320001] <- 'Reynosa, TAMPS'
touse$p24l[touse$p24l == 280270001] <- 'N. Laredo, TAMPS'
touse$p24l[touse$p24l == 270020001] <- 'Cardenas, TAB'
touse$p24l[touse$p24l == 301930001] <- 'Veracruz, VER'
touse$p24l[touse$p24l == 300390001] <- 'Coatzacoalcos, VER'
touse$p24l[touse$p24l == 270170001] <- 'Tenosique de Pino Suarez, TAB'
touse$p24l[touse$p24l == 270040001] <- 'Villahermosa, TAB'
touse$p24l[touse$p24l == 70650001] <- 'Palenque, CHIS'

touse$p34l[touse$p34l == 0] <- 'Avoided Deportation'
touse$p34l[touse$p34l == 300030001] <- 'Acayucan, VER'
touse$p34l[touse$p34l == 270020001] <- 'Cardenas, TAB'
touse$p34l[touse$p34l == 301930001] <- 'Veracruz, VER'
touse$p34l[touse$p34l == 300390001] <- 'Coatzacoalcos, VER'
touse$p34l[touse$p34l == 270170001] <- 'Tenosique de Pino Suarez, TAB'
touse$p34l[touse$p34l == 70650001] <- 'Palenque, CHIS'
touse$p34l[touse$p34l == 270040001] <- 'Villahermosa, TAB'


touse$p26[touse$p26 == 0] <- 'Deported Prior'
touse$p26[touse$p26 == 280320001] <- 'Reynosa, TAMPS'
touse$p26[touse$p26 == 280270001] <- 'N. Laredo, TAMPS'

ggplot(touse,
       aes(y = Freq,
           axis1 = p15, axis2 = p22_1l, axis3 = p24l, axis4 = p26)) +
  geom_alluvium(aes(fill = deported),
                width = 0.1, knot.pos = 0, reverse = FALSE) +
  guides(fill = FALSE) +
  geom_stratum(width = 1/10, reverse = FALSE) +
  geom_text(stat = "stratum", label.strata = TRUE, reverse = FALSE) +
  scale_x_continuous(breaks = 1:4, labels = c("Mexican Port of Entry", "Mexican City Visited", "Mexican City Where Spent Most Time" ,"US Port of Entry")) +
  labs(
    title = "As they journey northward, Honduran migrants who get \n deported in Mexico (red) get stuck in cities that border the Gulf of Mexico",
    subtitle = "Most migrants who make it to the US (blue) enter through Reynosa",
    caption = "Source: EMIF Sur (2014-2017)") + theme(
      plot.title = element_text(color="black", size=12, face="bold", hjust=0.5),
      plot.subtitle = element_text(color="black", size=10, face="italic", hjust=0.5),
      axis.title.x = element_text(color="black", size=8),
      axis.title.y = element_text(color="black", size=8),
      plot.caption = element_text(color="black", size=6, face="italic"))

ggsave('Alluvial.pdf', path="Visualizations/", width=11, height=8, units='in')
