# Mario Moreno 
# Data Vizualization 

##THIS IS DIFFERENT FROM BRUTE IN THAT IT ONLY CONSIDERS
##COLUMNS THAT MATCH ACROSS TIMEFRAME AND WITHIN DATAFRAMES

# This R notebook is reading and merging all the relevant data, with the hopes of getting to ten merged datasets

install.packages('janitor')
install.packages('gtools')

setwd('~Desktop/Grad School/Data Viz/OriginalData')
library(foreign)
library(janitor)
library(gtools)
library(dplyr)

##Reading LAPOP data and creating LAPOP datasets for each country

guate2017 <- read.dta("Guatemala/LAPOP/GuateLAPOP2017.dta")
guatemerged <- read.spss("Guatemala/LAPOP/m.sav", to.data.frame=TRUE)

hond2017 <- read.dta('Honduras/LAPOP/HondLAPOP17.dta')
hondmerged <- read.spss('Honduras/LAPOP/HondLAPOPm.sav', to.data.frame=TRUE)

els2017 <- read.dta('ElSalvador/LAPOP/ELSLAPOP17.dta')
elsmerged <- read.spss('ElSalvador/LAPOP/ELSLAPOPm.sav', to.data.frame=TRUE)

## Light cleaning of the data

l2017 <- lapply(list(guate2017, hond2017, els2017), cbind, year=2016)
lmerged <- lapply(list(guatemerged, hondmerged, elsmerged), subset, year != 2004 & year != 2006 & year != 2008) 

guate <- Filter(function(x) !all(is.na(x)), lmerged[[1]])
hond <- Filter(function(x) !all(is.na(x)), lmerged[[2]])
els <- Filter(function(x) !all(is.na(x)), lmerged[[3]])

guateLAPOPwords <- smartbind(guate, l2017[[1]])
hondLAPOPwords <- smartbind(hond, l2017[[2]])
elsLAPOPwords <- smartbind(els, l2017[[3]])

rm(els, els2017, elsmerged, guate, guate2017, guatemerged, hond, hond2017, hondmerged, l2017, lmerged)





##Reading LAPOP data and creating LAPOP datasets for each country

guate2017 <- read.dta('Guatemala/LAPOP/GuateLAPOP2017.dta', convert.factors = FALSE)
colnames(guate2017)[colnames(guate2017) == 'municipio'] <- 'muni'
hond2017 <- read.dta('Honduras/LAPOP/HondLAPOP17.dta', convert.factors = FALSE)
colnames(hond2017)[colnames(hond2017) == 'municipio'] <- 'muni'
els2017 <- read.dta('ElSalvador/LAPOP/ELSLAPOP17.dta', convert.factors = FALSE)
colnames(els2017)[colnames(els2017) == 'municipio'] <- 'muni'

l2017 <- lapply(list(guate2017, hond2017, els2017), cbind, year=2016)

guatemerged <- read.spss("Guatemala/LAPOP/m.sav", to.data.frame=TRUE, use.value.labels = FALSE)
colnames(guatemerged)[colnames(guatemerged) == 'municipio10'] <- 'muni'
hondmerged <- read.spss('Honduras/LAPOP/HondLAPOPm.sav', to.data.frame=TRUE, use.value.labels = FALSE)
colnames(hondmerged)[colnames(hondmerged) == 'municipio10'] <- 'muni'
elsmerged <- read.spss('ElSalvador/LAPOP/ELSLAPOPm.sav', to.data.frame=TRUE, use.value.labels = FALSE)
colnames(elsmerged)[colnames(elsmerged) == 'municipio10'] <- 'muni'


cols_to_keep <- intersect(colnames(l2017[[1]]), colnames(guatemerged))
g2017 <- l2017[[1]][, cols_to_keep, drop=FALSE]
gmerge <- guatemerged[, cols_to_keep, drop=FALSE]
rm(guate2017, guatemerged)

cols_to_keep <- intersect(colnames(l2017[[2]]), colnames(hondmerged))
h2017 <- l2017[[2]][, cols_to_keep, drop=FALSE]
hmerge <- hondmerged[, cols_to_keep, drop=FALSE]
rm(hond2017, hondmerged)

cols_to_keep <- intersect(colnames(l2017[[3]]), colnames(elsmerged))
e2017 <- l2017[[3]][, cols_to_keep, drop=FALSE]
emerge <- elsmerged[, cols_to_keep, drop=FALSE]
rm(els2017, elsmerged, cols_to_keep)

## Light cleaning of the data

lmerged <- lapply(list(gmerge, hmerge, emerge), subset, year != 2004 & year != 2006 & year != 2008) 

guate <- Filter(function(x) !all(is.na(x)), lmerged[[1]])
hond <- Filter(function(x) !all(is.na(x)), lmerged[[2]])
els <- Filter(function(x) !all(is.na(x)), lmerged[[3]])

guateLAPOP <- smartbind(guate, g2017)
hondLAPOP <- smartbind(hond, h2017)
elsLAPOP <- smartbind(els, e2017)

rm(els, e2017, emerge, guate, g2017, gmerge, hond, h2017, hmerge, l2017, lmerged)

guateLAPOP$pais = guateLAPOPwords$pais
guateLAPOP$estratopri = guateLAPOPwords$estratopri
guateLAPOP$prov = guateLAPOPwords$prov
guateLAPOP$ur = guateLAPOPwords$ur
guateLAPOP$tamano = guateLAPOPwords$tamano

hondLAPOP$pais = hondLAPOPwords$pais
hondLAPOP$estratopri = hondLAPOPwords$estratopri
hondLAPOP$prov = hondLAPOPwords$prov
hondLAPOP$ur = hondLAPOPwords$ur
hondLAPOP$tamano = hondLAPOPwords$tamano

elsLAPOP$pais = elsLAPOPwords$pais
elsLAPOP$estratopri = elsLAPOPwords$estratopri
elsLAPOP$prov = elsLAPOPwords$prov
elsLAPOP$ur = elsLAPOPwords$ur
elsLAPOP$tamano = elsLAPOPwords$tamano




# Reading in Security Data and creating one joint security dataset

USAID <- read.csv('US Foreign Aid/USAID.csv')
Sec <- read.csv('US Foreign Aid/SecurityAid.csv')[, c('year', 'country', 'program', 'amount', 'footnote')]

CentAmSec <- subset(Sec, (country=='Guatemala' | country=='Honduras' | country=='El Salvador') & year>2009 & year < 2018)
rm(Sec)

renamed <- setNames(CentAmSec, c("fiscal_year","country_name", "activity_name", "current_amount", "funding_agency_name"))
rm(CentAmSec)

security <- smartbind(USAID, renamed)
rm(USAID, renamed)

# Reading EMIF Sur Data and creating two datsets for each country: one for deported from MEX and one for deported from USA

## El Salvador Mexico data

path <- "ElSalvador/EMIF/"
files <- list.files(path=path, pattern="*mex.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

## There is differences in case of column names, so making them all lower

var.names<-tolower(colnames(elsdev2010mex))
new_df <- colnames(elsdev2010mex)<-var.names

var.names<-tolower(colnames(elsdev2011mex))
new_df <- colnames(elsdev2011mex)<-var.names

var.names<-tolower(colnames(elsdev2012mex))
new_df <- colnames(elsdev2012mex)<-var.names

var.names<-tolower(colnames(elsdev2013mex))
new_df <- colnames(elsdev2013mex)<-var.names

var.names<-tolower(colnames(elsdev2014mex))
new_df <- colnames(elsdev2014mex)<-var.names

var.names<-tolower(colnames(elsdev2015mex))
new_df <- colnames(elsdev2015mex)<-var.names

var.names<-tolower(colnames(elsdev2016mex))
new_df <- colnames(elsdev2016mex)<-var.names

var.names<-tolower(colnames(elsdev2017mex))
new_df <- colnames(elsdev2017mex)<-var.names

## Binding it all together

### 2010 and 2011 share the same exact columns
e2011 <- smartbind(elsdev2010mex, elsdev2011mex)

### 2011 and 2012 have different columns, so need to rename several 

names(e2011)[names(e2011) == 'p10_1u'] <- 'p10_1l'
names(e2011)[names(e2011) == 'p10_2u'] <- 'p10_2l'
names(e2011)[names(e2011) == 'p11_0'] <- 'P11_1'
names(e2011)[names(e2011) == 'p11_1'] <- 'P11_2'
names(e2011)[names(e2011) == 'p11_1_1'] <- 'P11_2_1'
names(e2011)[names(e2011) == 'p11_2'] <- 'P11_7'
names(e2011)[names(e2011) == 'p11_3'] <- 'P11_8'
names(e2011)[names(e2011) == 'p11_4c'] <- 'P11_9C'
names(e2011)[names(e2011) == 'p11_4u'] <- 'P11_9U'
names(e2011)[names(e2011) == 'p11_4t'] <- 'P11_9T'
names(e2011)[names(e2011) == 'p11_5'] <- 'P11_4'
names(e2011)[names(e2011) == 'p11_7'] <- 'P11_10'
names(e2011)[names(e2011) == 'p11_8'] <- 'P11_5'
names(e2011)[names(e2011) == 'p11_10_1'] <- 'P11_6_1'
names(e2011)[names(e2011) == 'p11_10_2'] <- 'p11_6_2'
names(e2011)[names(e2011) == 'p11_10_3'] <- 'p11_6_3'
names(e2011)[names(e2011) == 'p11_10_4'] <- 'p11_6_4'
names(e2011)[names(e2011) == 'p11_10_5'] <- 'p11_6_5'
names(e2011)[names(e2011) == 'p11_10_6'] <- 'p11_6_6'
names(e2011)[names(e2011) == 'p11_6_8'] <- 'p11_6_7'
names(e2011)[names(e2011) == 'p12_2'] <- 'P12_1'
names(e2011)[names(e2011) == 'p19u'] <- 'P19l1'
names(e2011)[names(e2011) == 'p19e'] <- 'p19e1'
names(e2011)[names(e2011) == 'p19c'] <- 'p19c1'
names(e2011)[names(e2011) == 'p20e'] <- 'P21e'
names(e2011)[names(e2011) == 'p20m'] <- 'P21m'
names(e2011)[names(e2011) == 'p20u'] <- 'P21c'
names(e2011)[names(e2011) == 'p21'] <- 'P23'
names(e2011)[names(e2011) == 'p22'] <- 'P26'
names(e2011)[names(e2011) == 'p22_2e'] <- 'P27e'
names(e2011)[names(e2011) == 'p22_2m'] <- 'P27m'
names(e2011)[names(e2011) == 'p22_2u'] <- 'P27c'
names(e2011)[names(e2011) == 'p22_3c'] <- 'P27_1c'
names(e2011)[names(e2011) == 'p22_3t'] <- 'P27_1t'
names(e2011)[names(e2011) == 'p23'] <- 'P28'
names(e2011)[names(e2011) == 'p24'] <- 'P28_1'
names(e2011)[names(e2011) == 'p25'] <- 'P29'
names(e2011)[names(e2011) == 'p25_1'] <- 'P29_1'
names(e2011)[names(e2011) == 'p25_2'] <- 'P29_2'
names(e2011)[names(e2011) == 'p26'] <- 'P30'
names(e2011)[names(e2011) == 'p27'] <- 'P31'
names(e2011)[names(e2011) == 'p28'] <- 'P32'
names(e2011)[names(e2011) == 'p28_1'] <- 'P32_1'
names(e2011)[names(e2011) == 'p29'] <- 'P33'
names(e2011)[names(e2011) == 'p30'] <- 'P34'
names(e2011)[names(e2011) == 'p30_1'] <- 'P34_1'
names(e2011)[names(e2011) == 'p32_2'] <- 'P36_2'
names(e2011)[names(e2011) == 'p33'] <- 'P37'
names(e2011)[names(e2011) == 'p34c'] <- 'P38c'
names(e2011)[names(e2011) == 'p34t'] <- 'P38t'
names(e2011)[names(e2011) == 'p35c'] <- 'P39c'
names(e2011)[names(e2011) == 'p35u'] <- 'P39u'
names(e2011)[names(e2011) == 'p35_1'] <- 'P39_1'
names(e2011)[names(e2011) == 'p36'] <- 'P40'
names(e2011)[names(e2011) == 'p37e'] <- 'P41E'
names(e2011)[names(e2011) == 'p37c'] <- 'P41c'
names(e2011)[names(e2011) == 'p37m'] <- 'P41m'
names(e2011)[names(e2011) == 'p37u'] <- 'P41u'
names(e2011)[names(e2011) == 'p42_1'] <- 'P20_1'
names(e2011)[names(e2011) == 'p42_2'] <- 'p20_2'
names(e2011)[names(e2011) == 'p43'] <- 'P46'
names(e2011)[names(e2011) == 'p44'] <- 'P48'
names(e2011)[names(e2011) == 'p46'] <- 'P51'
names(e2011)[names(e2011) == 'p50'] <- 'P53'

var.names<-tolower(colnames(e2011))
new_df <- colnames(e2011)<-var.names

cols_to_keep <- intersect(colnames(e2011), colnames(elsdev2012mex))
e11 <- e2011[, cols_to_keep, drop=FALSE]
edm2012 <- elsdev2012mex[, cols_to_keep, drop=FALSE]

e2012 <- smartbind(e11, edm2012)

names(e2012)[names(e2012) == 'p10_m'] <- 'p10_1mun'
names(e2012)[names(e2012) == 'p10_1u'] <- 'p10_1l'
names(e2012)[names(e2012) == 'p10_2u'] <- 'p10_2l'
names(e2012)[names(e2012) == 'p19l1'] <- 'p19u1'
names(e2012)[names(e2012) == 'p19l2'] <- 'p19u2'
names(e2012)[names(e2012) == 'p21l'] <- 'p21u'

cols_to_keep <- intersect(colnames(e2012), colnames(elsdev2013mex))
e12 <- e2012[, cols_to_keep, drop=FALSE]
esm2013 <- elsdev2013mex[, cols_to_keep, drop=FALSE]

e2013 <- smartbind(e12, esm2013)

names(e2013)[names(e2013) == 'p4'] <- 'P3'
names(e2013)[names(e2013) == 'p3_1'] <- 'P6'
names(e2013)[names(e2013) == 'p5g'] <- 'P4G'
names(e2013)[names(e2013) == 'p11_1'] <- 'P12_1'
names(e2013)[names(e2013) == 'p11_2_1'] <- 'P12_2_1'
names(e2013)[names(e2013) == 'p11_2'] <- 'P12_2'
names(e2013)[names(e2013) == 'p11_4'] <- 'P12_4'
names(e2013)[names(e2013) == 'p11_9c'] <- 'P12_9c'
names(e2013)[names(e2013) == 'p11_9u'] <- 'P12_9U'
names(e2013)[names(e2013) == 'p11_9t'] <- 'P12_9T'
names(e2013)[names(e2013) == 'p11_10'] <- 'P12_10'
names(e2013)[names(e2013) == 'p11_7'] <- 'P12_7'
names(e2013)[names(e2013) == 'p11_8'] <- 'P12_8'
names(e2013)[names(e2013) == 'P11_6_1'] <- 'P12_6_1'
names(e2013)[names(e2013) == 'p11_6_2'] <- 'P12_6_2'
names(e2013)[names(e2013) == 'p11_6_3'] <- 'P12_6_3'
names(e2013)[names(e2013) == 'p11_6_4'] <- 'P12_6_4'
names(e2013)[names(e2013) == 'p11_6_5'] <- 'P12_6_5'
names(e2013)[names(e2013) == 'p11_6_6'] <- 'P12_6_6'
names(e2013)[names(e2013) == 'p12'] <- 'P13'
names(e2013)[names(e2013) == 'p12_1'] <- 'P13_1' ##
names(e2013)[names(e2013) == 'p19l1'] <- 'P21u' #
names(e2013)[names(e2013) == 'p19e1'] <- 'p21e1' #
names(e2013)[names(e2013) == 'p19c1'] <- 'p21c1'#
names(e2013)[names(e2013) == 'p21e'] <- 'P23e' #
names(e2013)[names(e2013) == 'p21m'] <- 'P23m' #
names(e2013)[names(e2013) == 'p21c'] <- 'P23c' #
names(e2013)[names(e2013) == 'p23'] <- 'P25' #
names(e2013)[names(e2013) == 'p26'] <- 'P28' #
names(e2013)[names(e2013) == 'p27e'] <- 'P29e' #
names(e2013)[names(e2013) == 'p27m'] <- 'P29m' #
names(e2013)[names(e2013) == 'p27c'] <- 'P29c' #
names(e2013)[names(e2013) == 'p27_1c'] <- 'P29_1c' #
names(e2013)[names(e2013) == 'p27_1t'] <- 'P29_1t' #
names(e2013)[names(e2013) == 'p28'] <- 'P30' #
names(e2013)[names(e2013) == 'p28_1'] <- 'P30_1' #
names(e2013)[names(e2013) == 'p29'] <- 'P31' #
names(e2013)[names(e2013) == 'p29_1'] <- 'P31_1'#
names(e2013)[names(e2013) == 'p29_2'] <- 'P31_2' #
names(e2013)[names(e2013) == 'p30'] <- 'P32' #
names(e2013)[names(e2013) == 'p31'] <- 'P33' #
names(e2013)[names(e2013) == 'p32'] <- 'P34' #
names(e2013)[names(e2013) == 'p32_1'] <- 'P34_1' #
names(e2013)[names(e2013) == 'p33'] <- 'P35'#
names(e2013)[names(e2013) == 'p34'] <- 'P36'#
names(e2013)[names(e2013) == 'p34_1'] <- 'P36_1'#
names(e2013)[names(e2013) == 'p36_2'] <- 'P38_2'#
names(e2013)[names(e2013) == 'p37'] <- 'P39' #
names(e2013)[names(e2013) == 'p38c'] <- 'P40c'#
names(e2013)[names(e2013) == 'p38t'] <- 'P40t'#
names(e2013)[names(e2013) == 'p39c'] <- 'P41c' #
names(e2013)[names(e2013) == 'p39u'] <- 'P41u' #
names(e2013)[names(e2013) == 'p39_1'] <- 'P41_1'#
names(e2013)[names(e2013) == 'p40'] <- 'P42'#
names(e2013)[names(e2013) == 'p41e'] <- 'P43E'
names(e2013)[names(e2013) == 'p41c'] <- 'P43c'
names(e2013)[names(e2013) == 'p41m'] <- 'P43m'
names(e2013)[names(e2013) == 'p41u'] <- 'P43u'
names(e2013)[names(e2013) == 'p20_1'] <- 'P22_1'
names(e2013)[names(e2013) == 'p20_2'] <- 'p22_2'
names(e2013)[names(e2013) == 'p46'] <- 'P48'#
names(e2013)[names(e2013) == 'p46_1'] <- 'P48_1'
names(e2013)[names(e2013) == 'p51'] <- 'P53'
names(e2013)[names(e2013) == 'p47'] <- 'P49'

names(e2013)[names(e2013) == 'p6'] <- 'P7'
names(e2013)[names(e2013) == 'p7'] <- 'P8'
names(e2013)[names(e2013) == 'p5n'] <- 'P4n'
names(e2013)[names(e2013) == 'p7_1'] <- 'P8_1'
names(e2013)[names(e2013) == 'p8'] <- 'P9'
names(e2013)[names(e2013) == 'p8_1'] <- 'P9_1'
names(e2013)[names(e2013) == 'p8_2'] <- 'P9_2'
names(e2013)[names(e2013) == 'p8_3'] <- 'P9_3'
names(e2013)[names(e2013) == 'p8_4_1'] <- 'P9_4_1'
names(e2013)[names(e2013) == 'p8_4_2'] <- 'P9_4_2'
names(e2013)[names(e2013) == 'p8_4_3'] <- 'P9_4_3'
names(e2013)[names(e2013) == 'p8_4_4'] <- 'P9_4_4'
names(e2013)[names(e2013) == 'p8_5_1'] <- 'P9_5_1'
names(e2013)[names(e2013) == 'P8_5_2'] <- 'P9_5_2'
names(e2013)[names(e2013) == 'p8_5_3'] <- 'P9_5_3'
names(e2013)[names(e2013) == 'p8_5_4'] <- 'P9_5_4'
names(e2013)[names(e2013) == 'p9'] <- 'P10'
names(e2013)[names(e2013) == 'p10'] <- 'P11'

names(e2013)[names(e2013) == 'p10_1d'] <- 'P11_1d'
names(e2013)[names(e2013) == 'p10_1m'] <- 'P11_1m'
names(e2013)[names(e2013) == 'p10_1u'] <- 'P11_1u'
names(e2013)[names(e2013) == 'p10_2m'] <- 'P11_2m'
names(e2013)[names(e2013) == 'p10_2e'] <- 'P11_2e'
names(e2013)[names(e2013) == 'p10_2c'] <- 'P11_2c'
names(e2013)[names(e2013) == 'p13'] <- 'P14'
names(e2013)[names(e2013) == 'p14_1'] <- 'P15_1'
names(e2013)[names(e2013) == 'p14'] <- 'P15'
names(e2013)[names(e2013) == 'p15'] <- 'P17'
names(e2013)[names(e2013) == 'p15_1'] <- 'P17_1'
names(e2013)[names(e2013) == 'p15_2'] <- 'P17_2'
names(e2013)[names(e2013) == 'p15_2_1'] <- 'P17_2_1'
names(e2013)[names(e2013) == 'P15_3'] <- 'P17_3'
names(e2013)[names(e2013) == 'p16'] <- 'P18'
names(e2013)[names(e2013) == 'p17'] <- 'P19'
names(e2013)[names(e2013) == 'p17_1c'] <- 'P19_1c'
names(e2013)[names(e2013) == 'p17_1c'] <- 'P19_1u'

names(e2013)[names(e2013) == 'p17_2'] <- 'P19_2'
names(e2013)[names(e2013) == 'p18'] <- 'P20'
names(e2013)[names(e2013) == 'p19e2'] <- 'P21e2'
names(e2013)[names(e2013) == 'P19c2'] <- 'P21c2'
names(e2013)[names(e2013) == 'p19u2'] <- 'P21u2'
names(e2013)[names(e2013) == 'p53'] <- 'P55'
names(e2013)[names(e2013) == 'p54'] <- 'P56'

var.names<-tolower(colnames(e2013))
new_df <- colnames(e2013)<-var.names

cols_to_keep <- intersect(colnames(e2013), colnames(elsdev2014mex))
e13 <- e2013[, cols_to_keep, drop=FALSE]
edm2014 <- elsdev2014mex[, cols_to_keep, drop=FALSE]

e2014 <- smartbind(e13, edm2014)


rm(elsdev2010mex, elsdev2011mex, elsdev2012mex, elsdev2013mex, elsdev2014mex, elsdev2015mex, elsdev2016mex, elsdev2017mex)


# El Salvador USA data

files <- list.files(path=path, pattern="*usa.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

## There is differences in case of column names, so making them all lower

var.names<-tolower(colnames(elsdev2010usa))
new_df <- colnames(elsdev2010usa)<-var.names

var.names<-tolower(colnames(elsdev201usa))
new_df <- colnames(elsdev2011usa)<-var.names

var.names<-tolower(colnames(elsdev2012usa))
new_df <- colnames(elsdev2012usa)<-var.names

var.names<-tolower(colnames(elsdev2013usa))
new_df <- colnames(elsdev2013usa)<-var.names

var.names<-tolower(colnames(elsdev2014usa))
new_df <- colnames(elsdev2014usa)<-var.names

var.names<-tolower(colnames(elsdev2015usa))
new_df <- colnames(elsdev2015usa)<-var.names

var.names<-tolower(colnames(elsdev2016usa))
new_df <- colnames(elsdev2016usa)<-var.names

var.names<-tolower(colnames(elsdev2017usa))
new_df <- colnames(elsdev2017usa)<-var.names

elsusaEMIF <- smartbind(elsdev2010usa, elsdev2011usa, elsdev2012usa, elsdev2013usa, elsdev2014usa, elsdev2015usa, elsdev2016usa, elsdev2017usa)
rm(elsdev2010usa, elsdev2011usa, elsdev2012usa, elsdev2013usa, elsdev2014usa, elsdev2015usa, elsdev2016usa, elsdev2017usa)


# Guatemala Mexico 

path <- "Guatemala/EMIF/"

files <- list.files(path=path, pattern="*mex.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

var.names<-tolower(colnames(guatedev2010mex))
new_df <- colnames(guatedev2010mex)<-var.names

var.names<-tolower(colnames(guatedev2011mex))
new_df <- colnames(guatedev2011mex)<-var.names

var.names<-tolower(colnames(guatedev2012mex))
new_df <- colnames(guatedev2012mex)<-var.names

var.names<-tolower(colnames(guatedev2013mex))
new_df <- colnames(guatedev2013mex)<-var.names

var.names<-tolower(colnames(guatedev2014mex))
new_df <- colnames(guatedev2014mex)<-var.names

var.names<-tolower(colnames(guatedev2015mex))
new_df <- colnames(guatedev2015mex)<-var.names

var.names<-tolower(colnames(guatedev2016mex))
new_df <- colnames(guatedev2016mex)<-var.names

var.names<-tolower(colnames(guatedev2017mex))
new_df <- colnames(guatedev2017mex)<-var.names

guatemexEMIF <- smartbind(guatedev2010mex, guatedev2011mex, guatedev2012mex, guatedev2013mex, guatedev2014mex, guatedev2015mex, guatedev2016mex, guatedev2017mex)
rm(guatedev2010mex, guatedev2011mex, guatedev2012mex, guatedev2013mex, guatedev2014mex, guatedev2015mex, guatedev2016mex, guatedev2017mex)

# Guatemala USA

files <- list.files(path=path, pattern="*usa.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

var.names<-tolower(colnames(guatedev2010usa))
new_df <- colnames(guatedev2010usa)<-var.names

var.names<-tolower(colnames(guatedev2011usa))
new_df <- colnames(guatedev2011usa)<-var.names

var.names<-tolower(colnames(guatedev2012usa))
new_df <- colnames(guatedev2012usa)<-var.names

var.names<-tolower(colnames(guatedev2013usa))
new_df <- colnames(guatedev2013usa)<-var.names

var.names<-tolower(colnames(guatedev2014usa))
new_df <- colnames(guatedev2014usa)<-var.names

var.names<-tolower(colnames(guatedev2015usa))
new_df <- colnames(guatedev2015usa)<-var.names

var.names<-tolower(colnames(guatedev2016usa))
new_df <- colnames(guatedev2016usa)<-var.names

var.names<-tolower(colnames(guatedev2017usa))
new_df <- colnames(guatedev2017usa)<-var.names


guateusaEMIF <- smartbind(guatedev2010usa, guatedev2011usa, guatedev2012usa, guatedev2013usa, guatedev2014usa, guatedev2015usa, guatedev2016usa, guatedev2017usa)
rm(guatedev2010usa, guatedev2011usa, guatedev2012usa, guatedev2013usa, guatedev2014usa, guatedev2015usa, guatedev2016usa, guatedev2017usa)

# Honduras Mexico 

path <- "Honduras/EMIF/"

files <- list.files(path=path, pattern="*mex.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

var.names<-tolower(colnames(honddev2010mex))
new_df <- colnames(honddev2010mex)<-var.names

var.names<-tolower(colnames(honddev2011mex))
new_df <- colnames(honddev2011mex)<-var.names

var.names<-tolower(colnames(honddev2012mex))
new_df <- colnames(honddev2012mex)<-var.names

var.names<-tolower(colnames(honddev2013mex))
new_df <- colnames(honddev2013mex)<-var.names

var.names<-tolower(colnames(honddev2014mex))
new_df <- colnames(honddev2014mex)<-var.names

var.names<-tolower(colnames(honddev2015mex))
new_df <- colnames(honddev2015mex)<-var.names

var.names<-tolower(colnames(honddev2016mex))
new_df <- colnames(honddev2016mex)<-var.names

var.names<-tolower(colnames(honddev2017mex))
new_df <- colnames(honddev2017mex)<-var.names

hondmexEMIF <- smartbind(honddev2010mex, honddev2011mex, honddev2012mex, honddev2013mex, honddev2014mex, honddev2015mex, honddev2016mex, honddev2017mex)
rm(honddev2010mex, honddev2011mex, honddev2012mex, honddev2013mex, honddev2014mex, honddev2015mex, honddev2016mex, honddev2017mex)

# Honduras USA

files <- list.files(path=path, pattern="*usa.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

var.names<-tolower(colnames(honddev2010usa))
new_df <- colnames(honddev2010usa)<-var.names

var.names<-tolower(colnames(honddev2011usa))
new_df <- colnames(honddev2011usa)<-var.names

var.names<-tolower(colnames(honddev2012usa))
new_df <- colnames(honddev2012usa)<-var.names

var.names<-tolower(colnames(honddev2013usa))
new_df <- colnames(honddev2013usa)<-var.names

var.names<-tolower(colnames(honddev2014usa))
new_df <- colnames(honddev2014usa)<-var.names

var.names<-tolower(colnames(honddev2015usa))
new_df <- colnames(honddev2015usa)<-var.names

var.names<-tolower(colnames(honddev2016usa))
new_df <- colnames(honddev2016usa)<-var.names

var.names<-tolower(colnames(honddev2017usa))
new_df <- colnames(honddev2017usa)<-var.names

hondusaEMIF <- smartbind(honddev2010usa, honddev2011usa, honddev2012usa, honddev2013usa, honddev2014usa, honddev2015usa, honddev2016usa, honddev2017usa)
rm(honddev2010usa, honddev2011usa, honddev2012usa, honddev2013usa, honddev2014usa, honddev2015usa, honddev2016usa, honddev2017usa)
rm(z, file, files, path, perpos)

cols_to_keep_mex <- intersect(colnames(elsmexEMIF), colnames(guatemexEMIF))
e <- elsmexEMIF[, cols_to_keep_mex, drop=FALSE]
g <- guatemexEMIF[, cols_to_keep_mex, drop=FALSE]
egbind <- smartbind(e, g)
cols_to_keep_mex <- intersect(colnames(egbind), colnames(hondmexEMIF))
b <- egbind[, cols_to_keep_mex, drop=FALSE]
h <- hondmexEMIF[, cols_to_keep_mex, drop=FALSE]
finalbind <- smartbind(b, h)

cols_to_keep_usa <- intersect(colnames(elsusaEMIF), colnames(guateusaEMIF))
e <- elsusaEMIF[, cols_to_keep_usa, drop=FALSE]
g <- guateusaEMIF[, cols_to_keep_usa, drop=FALSE]
egbind <- smartbind(e, g)
cols_to_keep_usa <- intersect(colnames(egbind), colnames(hondusaEMIF))
b <- egbind[, cols_to_keep_usa, drop=FALSE]
h <- hondusaEMIF[, cols_to_keep_usa, drop=FALSE]
finalbindusa <- smartbind(b, h)

rm(b, e, egbind, g, h, cols_to_keep_mex, cols_to_keep_usa, var.names, new_df)

# Exporting to CSV

write.csv(elsLAPOP, 'elsLAPOP.csv')
write.csv(elsmexEMIF, 'elsmexEMIF.csv')
write.csv(elsusaEMIF, 'elsusaEMIF.csv')
write.csv(guateLAPOP, 'guateLAPOP.csv')
write.csv(guatemexEMIF, 'guatemexEMIF.csv')
write.csv(guateusaEMIF, 'guateusaEMIF.csv')
write.csv(hondLAPOP, 'hondLAPOP.csv')
write.csv(hondmexEMIF, 'hondmexEMIF.csv')
write.csv(hondusaEMIF, 'hondusaEMIF.csv')
write.csv(security, 'security.csv')
write.csv(finalbind, 'deportedmex.csv')
write.csv(finalbindusa, 'deportedusa.csv')

