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

guate2017 <- read.dta('Guatemala/LAPOP/GuateLAPOP2017.dta', convert.factors = FALSE)
colnames(guate2017)[colnames(guate2017) == 'municipio'] <- 'muni'
hond2017 <- read.dta('Honduras/LAPOP/HondLAPOP17.dta', convert.factors = FALSE)
colnames(hond2017)[colnames(hond2017) == 'municipio'] <- 'muni'
els2017 <- read.dta('ElSalvador/LAPOP/ELSLAPOP17.dta', convert.factors = FALSE)
colnames(els2017)[colnames(els2017) == 'municipio'] <- 'muni'

l2017 <- lapply(list(guate2017, hond2017, els2017), cbind, year=2017)

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


path <- "ElSalvador/EMIF/"
files <- list.files(path=path, pattern="*mex.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

to_keep <- intersect(colnames(elsdev2014mex), colnames(elsdev2017mex))

elsmexEMIF <- smartbind(elsdev2010mex, elsdev2011mex,elsdev2012mex, elsdev2013mex, elsdev2014mex, elsdev2015mex, elsdev2016mex, elsdev2017mex)
rm(elsdev2010mex, elsdev2011mex, elsdev2012mex, elsdev2013mex, elsdev2014mex, elsdev2015mex, elsdev2016mex, elsdev2017mex)

files <- list.files(path=path, pattern="*usa.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

elsusaEMIF <- smartbind(elsdev2010usa, elsdev2011usa, elsdev2012usa, elsdev2013usa, elsdev2014usa, elsdev2015usa, elsdev2016usa, elsdev2017usa)
rm(elsdev2010usa, elsdev2011usa, elsdev2012usa, elsdev2013usa, elsdev2014usa, elsdev2015usa, elsdev2016usa, elsdev2017usa)


path <- "Guatemala/EMIF/"

files <- list.files(path=path, pattern="*mex.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

guatemexEMIF <- smartbind(guatedev2010mex, guatedev2011mex, guatedev2012mex, guatedev2013mex, guatedev2014mex, guatedev2015mex, guatedev2016mex, guatedev2017mex)
rm(guatedev2010mex, guatedev2011mex, guatedev2012mex, guatedev2013mex, guatedev2014mex, guatedev2015mex, guatedev2016mex, guatedev2017mex)


files <- list.files(path=path, pattern="*usa.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

guateusaEMIF <- smartbind(guatedev2010usa, guatedev2011usa, guatedev2012usa, guatedev2013usa, guatedev2014usa, guatedev2015usa, guatedev2016usa, guatedev2017usa)
rm(guatedev2010usa, guatedev2011usa, guatedev2012usa, guatedev2013usa, guatedev2014usa, guatedev2015usa, guatedev2016usa, guatedev2017usa)



path <- "Honduras/EMIF/"

files <- list.files(path=path, pattern="*mex.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

hondmexEMIF <- smartbind(honddev2010mex, honddev2011mex, honddev2012mex, honddev2013mex, honddev2014mex, honddev2015mex, honddev2016mex, honddev2017mex)
rm(honddev2010mex, honddev2011mex, honddev2012mex, honddev2013mex, honddev2014mex, honddev2015mex, honddev2016mex, honddev2017mex)


files <- list.files(path=path, pattern="*usa.csv")
for(file in files)
{
  perpos <- which(strsplit(file, "")[[1]]==".")
  assign(
    gsub(" ","",substr(file, 1, perpos-1)), 
    read.csv(paste(path,file,sep="")))
}

hondusaEMIF <- smartbind(honddev2010usa, honddev2011usa, honddev2012usa, honddev2013usa, honddev2014usa, honddev2015usa, honddev2016usa, honddev2017usa)
rm(honddev2010usa, honddev2011usa, honddev2012usa, honddev2013usa, honddev2014usa, honddev2015usa, honddev2016usa, honddev2017usa)
rm(z, file, files, path, perpos)


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

