library(gtools)

hmex2017 <- read.csv('Data/honddev2017mex.csv')
hmex2017$pais <- 'Honduras'
hmex2017$deported <- 'Mexico'

husa2017 <- read.csv('Data/honddev2017usa.csv')
husa2017$pais <- 'Honduras'
husa2017$deported <- 'USA'

gmex2017 <- read.csv('Data/guatedev2017mex.csv')
gmex2017$pais <- 'Guatemala'
gmex2017$deported <- 'Mexico'

gusa2017 <- read.csv('Data/guatedev2017usa.csv')
gusa2017$pais <- 'Guatemala'
gusa2017$deported <- 'USA'

emex2017 <- read.csv('Data/elsdev2017mex.csv')
emex2017$pais <- 'El Salvador'
emex2017$deported <- 'Mexico'

eusa2017 <- read.csv('Data/elsdev2017usa.csv')
eusa2017$pais <- 'El Salvador'
eusa2017$deported <- 'USA'

usa1 <- smartbind(eusa2017, gusa2017)
usa2017 <- smartbind(usa1, husa2017)
rm(usa1)

mex1 <- smartbind(emex2017, gmex2017)
mex2017 <- smartbind(mex1, hmex2017)
rm(mex1)

valmex <- read.csv('Values/ValoreMex2017.csv', encoding='latin1')
valusa <- read.csv('Values/ValoresUSA2017.csv', encoding='latin1')

write.csv(usa2017, 'usa2017.csv')
write.csv(mex2017, 'mex2017.csv')
