##### Dissertation Paper 1 Post-Midwest v6 #####
##### Run in R 4.3.1
##### Most recent update to file Jan 4, 2024

#### Set-up 
rm(list=ls()) 
setwd("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/Post-Midwest v6/Data")
options(scipen=999) #turns off sci notation


library(tidyverse)
library(readxl)
library(MASS) #for simulation
library(dplyr)
library(WDI)
library(countrycode) #for merging all the data--provides country codes in variety of formats
library(spData) #for basic logit
library(texreg)
library(stargazer)
library(performance) #for robustness checks
library(broom)
library(zoo)
library(survival)
library(AER) #for dispersion tests
library(car) #for vif
library(sjPlot) #for marginal effects plots
library(sjmisc) #note this masks tidyr:replace_na
library(ggplot2)
library(ggeffects)
library(kableExtra) #for descriptive stats table. note this masks dplyr:group_rows

##################################
##custom fix for Stargazer is.na>1 error from https://gist.github.com/alexeyknorre/b0780836f4cec04d41a863a683f91b53
# for background see https://www.reddit.com/r/rstats/comments/ucmtdn/issue_with_stargazer_package_after_update_to_r_420/
# Unload stargazer if loaded
#detach("package:stargazer",unload=T)
# Delete it
#remove.packages("stargazer")
# Download the source
#download.file("https://cran.r-project.org/src/contrib/stargazer_5.2.3.tar.gz", destfile = "stargazer_5.2.3.tar.gz")
# Unpack
#untar("stargazer_5.2.3.tar.gz")
# Read the sourcefile with .inside.bracket fun
#stargazer_src <- readLines("stargazer/R/stargazer-internal.R")
# Move the length check 5 lines up so it precedes is.na(.)
#stargazer_src[1990] <- stargazer_src[1995]
#stargazer_src[1995] <- ""
# Save back
#writeLines(stargazer_src, con="stargazer/R/stargazer-internal.R")
# Compile and install the patched package
#install.packages("stargazer", repos = NULL, type="source")
#library(stargazer)
#################################### Stargazer tables (not updated)

#### Preliminary Read-Ins
### Polity 5 (ignore warnings) (necessary for mergers)
P5 <- read_excel("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/Post-Midwest v5/Data/Polity5v2018.xls")
politysmall <- P5 %>% filter(year > 1994) %>% filter(year < 2015) %>% dplyr::select(scode,ccode,country,year,polity2)
rm(P5)
politysmall$ccode[politysmall$ccode == 529] <- 530 #brings Ethiopia ccode in line with cown/gwn, enabling use of alt formatter 
politysmall$ccode[politysmall$ccode == 525] <- 626 #brings South Sudan ccode in line with cown/gwn, enabling use of alt formatter 
politysmall <- politysmall %>% filter(!politysmall$country == "Sudan-North") #removes 4 years of double-counting "Sudan-North" from 2011-2014, which existed alongside South Sudan, creating merger issues

### V-Dem (necessary for mergers)
vdem <- read_csv("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/Post-Midwest v5/Data/V-Dem-CY-Core_csv_v13/V-Dem-CY-Core-v13.csv")
vdemsmall <- vdem %>% filter(year > 1994) %>% filter(year < 2015) %>% dplyr::select(country_name,year,COWcode,v2x_polyarchy,v2x_libdem,v2x_partipdem,v2x_delibdem,v2x_egaldem,v2x_regime)
rm(vdem)
#Ethiopia and South Sudan codes already in line with cown/gwn

### UCDP Datasets (necessary for mergers)
ACD <- read.csv("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/Post-Midwest v5/Data/ACDv22_1.csv")
GED <- read.csv("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/Post-Midwest v5/Data/GEDv22_1.csv")


#### Function definitions
### Define function to format raw aiddata inputs into something that the other functions accept
preprocessor = function(dataframe){
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.bf1de3f.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_1995" = "worldbank_geocodedresearchrelease_level1_v1_4_2.bf1de3f.sum")
  } else {
    dataframe <- add_column(dataframe, aid_1995 = NA, .after = "asdf_id")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.a4b4c97.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_1996" = "worldbank_geocodedresearchrelease_level1_v1_4_2.a4b4c97.sum")
  } else {
    dataframe <- add_column(dataframe, aid_1996 = NA, .after = "aid_1995")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.f99c939.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_1997" = "worldbank_geocodedresearchrelease_level1_v1_4_2.f99c939.sum")
  } else {
    dataframe <- add_column(dataframe, aid_1997 = NA, .after = "aid_1996")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.485401a.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_1998" = "worldbank_geocodedresearchrelease_level1_v1_4_2.485401a.sum")
  } else {
    dataframe <- add_column(dataframe, aid_1998 = NA, .after = "aid_1997")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.a127a6e.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_1999" = "worldbank_geocodedresearchrelease_level1_v1_4_2.a127a6e.sum")
  } else {
    dataframe <- add_column(dataframe, aid_1999 = NA, .after = "aid_1998")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.c2de8df.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_2000" = "worldbank_geocodedresearchrelease_level1_v1_4_2.c2de8df.sum")
  } else {
    dataframe <- add_column(dataframe, aid_2000 = NA, .after = "aid_1999")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.887790a.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_2001" = "worldbank_geocodedresearchrelease_level1_v1_4_2.887790a.sum")
  } else {
    dataframe <- add_column(dataframe, aid_2001 = NA, .after = "aid_2000")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.653b730.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_2002" = "worldbank_geocodedresearchrelease_level1_v1_4_2.653b730.sum")
  } else {
    dataframe <- add_column(dataframe, aid_2002 = NA, .after = "aid_2001")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.034e602.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_2003" = "worldbank_geocodedresearchrelease_level1_v1_4_2.034e602.sum")
  } else {
    dataframe <- add_column(dataframe, aid_2003 = NA, .after = "aid_2002")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.6142400.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_2004" = "worldbank_geocodedresearchrelease_level1_v1_4_2.6142400.sum")
  } else {
    dataframe <- add_column(dataframe, aid_2004 = NA, .after = "aid_2003")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.381e47f.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_2005" = "worldbank_geocodedresearchrelease_level1_v1_4_2.381e47f.sum")
  } else {
    dataframe <- add_column(dataframe, aid_2005 = NA, .after = "aid_2004")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.be4c261.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_2006" = "worldbank_geocodedresearchrelease_level1_v1_4_2.be4c261.sum")
  } else {
    dataframe <- add_column(dataframe, aid_2006 = NA, .after = "aid_2005")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.98af255.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_2007" = "worldbank_geocodedresearchrelease_level1_v1_4_2.98af255.sum")
  } else {
    dataframe <- add_column(dataframe, aid_2007 = NA, .after = "aid_2006")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.993244c.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_2008" = "worldbank_geocodedresearchrelease_level1_v1_4_2.993244c.sum")
  } else {
    dataframe <- add_column(dataframe, aid_2008 = NA, .after = "aid_2007")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.0cf8a0f.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_2009" = "worldbank_geocodedresearchrelease_level1_v1_4_2.0cf8a0f.sum")
  } else {
    dataframe <- add_column(dataframe, aid_2009 = NA, .after = "aid_2008")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.c3898aa.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_2010" = "worldbank_geocodedresearchrelease_level1_v1_4_2.c3898aa.sum")
  } else {
    dataframe <- add_column(dataframe, aid_2010 = NA, .after = "aid_2009")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.e9437ae.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_2011" = "worldbank_geocodedresearchrelease_level1_v1_4_2.e9437ae.sum")
  } else {
    dataframe <- add_column(dataframe, aid_2011 = NA, .after = "aid_2010")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.56d6ad1.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_2012" = "worldbank_geocodedresearchrelease_level1_v1_4_2.56d6ad1.sum")
  } else {
    dataframe <- add_column(dataframe, aid_2012 = NA, .after = "aid_2011")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.a8d2793.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_2013" = "worldbank_geocodedresearchrelease_level1_v1_4_2.a8d2793.sum")
  } else {
    dataframe <- add_column(dataframe, aid_2013 = NA, .after = "aid_2012")
  }
  if("worldbank_geocodedresearchrelease_level1_v1_4_2.d56e944.sum" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "aid_2014" = "worldbank_geocodedresearchrelease_level1_v1_4_2.d56e944.sum")
  } else {
    dataframe <- add_column(dataframe, aid_2014 = NA, .after = "aid_2013")
  } 
  if("accessibility_to_cities_2015_v1.0.mean" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "citytime_2015" = "accessibility_to_cities_2015_v1.0.mean")
  } 
  if("access_50k.none.mean" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "citytime_2000" = "access_50k.none.mean")
  }  
  #everything to this point converts aid data column names over to the aid_year format, creating a col of NAs if there was no data that year.
  
  #Angola's naming appears to be unique. This checks for it and changes names to prevent errors later.
  if("ADM1_NAME" %in% colnames(dataframe)){
    dataframe <- rename(dataframe, "shapeName" = "ADM1_NAME")
  }
  
  #Some countries lack an "id" column. This checks for it and adds one if missing.
  if(!("id" %in% colnames(dataframe))){
    dataframe <- add_column(dataframe, id = NA)
  }
  
  #now formatting UCDP deaths
  dataframe <- subset(dataframe, select = -c(ucdp_deaths_171.1989.sum, 
                                             ucdp_deaths_171.1990.sum, 
                                             ucdp_deaths_171.1991.sum, 
                                             ucdp_deaths_171.1992.sum,
                                             ucdp_deaths_171.1993.sum,
                                             ucdp_deaths_171.1994.sum)) #removes years outside timeframe
  dataframe <- subset(dataframe, select = -c(ucdp_deaths_171.2015.sum, ucdp_deaths_171.2016.sum)) #removes years outside timeframe
  dataframe <- rename(dataframe, 
                      "ucdpdeaths_1995" = "ucdp_deaths_171.1995.sum",
                      "ucdpdeaths_1996" = "ucdp_deaths_171.1996.sum",
                      "ucdpdeaths_1997" = "ucdp_deaths_171.1997.sum",
                      "ucdpdeaths_1998" = "ucdp_deaths_171.1998.sum",
                      "ucdpdeaths_1999" = "ucdp_deaths_171.1999.sum",
                      "ucdpdeaths_2000" = "ucdp_deaths_171.2000.sum",
                      "ucdpdeaths_2001" = "ucdp_deaths_171.2001.sum",
                      "ucdpdeaths_2002" = "ucdp_deaths_171.2002.sum",
                      "ucdpdeaths_2003" = "ucdp_deaths_171.2003.sum",
                      "ucdpdeaths_2004" = "ucdp_deaths_171.2004.sum",
                      "ucdpdeaths_2005" = "ucdp_deaths_171.2005.sum",
                      "ucdpdeaths_2006" = "ucdp_deaths_171.2006.sum",
                      "ucdpdeaths_2007" = "ucdp_deaths_171.2007.sum",
                      "ucdpdeaths_2008" = "ucdp_deaths_171.2008.sum",
                      "ucdpdeaths_2009" = "ucdp_deaths_171.2009.sum",
                      "ucdpdeaths_2010" = "ucdp_deaths_171.2010.sum",
                      "ucdpdeaths_2011" = "ucdp_deaths_171.2011.sum",
                      "ucdpdeaths_2012" = "ucdp_deaths_171.2012.sum",
                      "ucdpdeaths_2013" = "ucdp_deaths_171.2013.sum",
                      "ucdpdeaths_2014" = "ucdp_deaths_171.2014.sum",
  )
  #removing NAs from UCDP data per email from them that NA should be 0
  dataframe <- dataframe %>% mutate(ucdpdeaths_1995 = ifelse(is.na(ucdpdeaths_1995), 0, ucdpdeaths_1995))
  dataframe <- dataframe %>% mutate(ucdpdeaths_1996 = ifelse(is.na(ucdpdeaths_1996), 0, ucdpdeaths_1996))
  dataframe <- dataframe %>% mutate(ucdpdeaths_1997 = ifelse(is.na(ucdpdeaths_1997), 0, ucdpdeaths_1997))
  dataframe <- dataframe %>% mutate(ucdpdeaths_1998 = ifelse(is.na(ucdpdeaths_1998), 0, ucdpdeaths_1998))
  dataframe <- dataframe %>% mutate(ucdpdeaths_1999 = ifelse(is.na(ucdpdeaths_1999), 0, ucdpdeaths_1999))
  dataframe <- dataframe %>% mutate(ucdpdeaths_2000 = ifelse(is.na(ucdpdeaths_2000), 0, ucdpdeaths_2000))
  dataframe <- dataframe %>% mutate(ucdpdeaths_2001 = ifelse(is.na(ucdpdeaths_2001), 0, ucdpdeaths_2001))
  dataframe <- dataframe %>% mutate(ucdpdeaths_2002 = ifelse(is.na(ucdpdeaths_2002), 0, ucdpdeaths_2002))
  dataframe <- dataframe %>% mutate(ucdpdeaths_2003 = ifelse(is.na(ucdpdeaths_2003), 0, ucdpdeaths_2003))
  dataframe <- dataframe %>% mutate(ucdpdeaths_2004 = ifelse(is.na(ucdpdeaths_2004), 0, ucdpdeaths_2004))
  dataframe <- dataframe %>% mutate(ucdpdeaths_2005 = ifelse(is.na(ucdpdeaths_2005), 0, ucdpdeaths_2005))
  dataframe <- dataframe %>% mutate(ucdpdeaths_2006 = ifelse(is.na(ucdpdeaths_2006), 0, ucdpdeaths_2006))
  dataframe <- dataframe %>% mutate(ucdpdeaths_2007 = ifelse(is.na(ucdpdeaths_2007), 0, ucdpdeaths_2007))
  dataframe <- dataframe %>% mutate(ucdpdeaths_2008 = ifelse(is.na(ucdpdeaths_2008), 0, ucdpdeaths_2008))
  dataframe <- dataframe %>% mutate(ucdpdeaths_2009 = ifelse(is.na(ucdpdeaths_2009), 0, ucdpdeaths_2009))
  dataframe <- dataframe %>% mutate(ucdpdeaths_2010 = ifelse(is.na(ucdpdeaths_2010), 0, ucdpdeaths_2010))
  dataframe <- dataframe %>% mutate(ucdpdeaths_2011 = ifelse(is.na(ucdpdeaths_2011), 0, ucdpdeaths_2011))
  dataframe <- dataframe %>% mutate(ucdpdeaths_2012 = ifelse(is.na(ucdpdeaths_2012), 0, ucdpdeaths_2012))
  dataframe <- dataframe %>% mutate(ucdpdeaths_2013 = ifelse(is.na(ucdpdeaths_2013), 0, ucdpdeaths_2013))
  dataframe <- dataframe %>% mutate(ucdpdeaths_2014 = ifelse(is.na(ucdpdeaths_2014), 0, ucdpdeaths_2014))
  #formatting worldpop titles
  dataframe <- subset(dataframe, select = -c(worldpop_pop_count_1km_mosaic.2015.sum,
                                             worldpop_pop_count_1km_mosaic.2016.sum,
                                             worldpop_pop_count_1km_mosaic.2017.sum,
                                             worldpop_pop_count_1km_mosaic.2018.sum,
                                             worldpop_pop_count_1km_mosaic.2019.sum,
                                             worldpop_pop_count_1km_mosaic.2020.sum)) #removes years outside timeframe
  dataframe <- rename(dataframe, 
                      "worldpop_2000" = "worldpop_pop_count_1km_mosaic.2000.sum",
                      "worldpop_2001" = "worldpop_pop_count_1km_mosaic.2001.sum",
                      "worldpop_2002" = "worldpop_pop_count_1km_mosaic.2002.sum",
                      "worldpop_2003" = "worldpop_pop_count_1km_mosaic.2003.sum",
                      "worldpop_2004" = "worldpop_pop_count_1km_mosaic.2004.sum",
                      "worldpop_2005" = "worldpop_pop_count_1km_mosaic.2005.sum",
                      "worldpop_2006" = "worldpop_pop_count_1km_mosaic.2006.sum",
                      "worldpop_2007" = "worldpop_pop_count_1km_mosaic.2007.sum",
                      "worldpop_2008" = "worldpop_pop_count_1km_mosaic.2008.sum",
                      "worldpop_2009" = "worldpop_pop_count_1km_mosaic.2009.sum",
                      "worldpop_2010" = "worldpop_pop_count_1km_mosaic.2010.sum",
                      "worldpop_2011" = "worldpop_pop_count_1km_mosaic.2011.sum",
                      "worldpop_2012" = "worldpop_pop_count_1km_mosaic.2012.sum",
                      "worldpop_2013" = "worldpop_pop_count_1km_mosaic.2013.sum",
                      "worldpop_2014" = "worldpop_pop_count_1km_mosaic.2014.sum",
  )
  
  dataframe <- add_column(dataframe, worldpop_1995 = NA, .before = "worldpop_2000")
  dataframe <- add_column(dataframe, worldpop_1996 = NA, .after = "worldpop_1995")
  dataframe <- add_column(dataframe, worldpop_1997 = NA, .after = "worldpop_1996")
  dataframe <- add_column(dataframe, worldpop_1998 = NA, .after = "worldpop_1997")
  dataframe <- add_column(dataframe, worldpop_1999 = NA, .after = "worldpop_1998")
  
  #removing non-essential columns left over from raw download
  dataframe <- dataframe %>% dplyr::select(starts_with("aid"),starts_with("ucdp"),starts_with("world"),starts_with("citytime"),shapeGroup,shapeName,shapeName2,id)
  dataframe$id <- 1:nrow(dataframe) #creates ID counter for each ADM1-level subnational division that starts at 1 instead of 0
  
  return(dataframe)
}

### Define function to format aiddata inputs as "long" dataframe
aiddata_formatter = function (dataframe) {
  intermed <- dataframe %>% pivot_longer(
    cols = !shapeName & !shapeGroup & !id & !shapeName2 & !citytime_2000 & !citytime_2015,
    names_to = c("thing","year"),
    names_sep = "_",
    values_to = "total"
  )
  output <- intermed %>% spread(thing,total)
  output$year <- as.numeric(output$year)
  output <- output %>% rename(iso3c = shapeGroup) #shapeGroup is an iso3c code per own observation. Necessary for later merges.
  output$laggedaid <- lag(output$aid,1) #make lagged aid variable, lagged by 1 year. So aid from 1995 appears in rows for 1996
  return(output)
}

### Define function to pull WDI GDPpc data, using iso3c code extracted from output of aiddata_formatter
WDIgrabber = function (dataframe){
  WDIoutput <- WDI(country = dataframe$iso3c[[1]], #gets countryname iso3c code from shapeGroup column
                   indicator = c("NY.GDP.PCAP.KD")) #GDP per capita in constant 2015 USD
  WDIoutput <- WDIoutput %>% filter(year > 1994) %>% filter(year < 2015) 
  return(WDIoutput)
}

### Define function to merge in polity and WDI variables
merger = function (WDIdataframe,aiddataframe) {
  base2 <- codelist_panel %>% filter(year > 1994) %>% filter(year < 2015) %>% dplyr::select(country.name.en,year,cown,gwn,gwc,iso2c,iso3c) #using per recommendation of countrycode doc
  combodata2 <- left_join(WDIdataframe,base2,by = c("iso3c", "year"))
  combodata2 <- left_join(combodata2, politysmall, by = c("gwc" = "scode", "year" = "year"))
  combodata2 <- left_join(combodata2, vdemsmall, by = c("cown" = "COWcode", "year" = "year"))
  combodata2 <- left_join(combodata2, aiddataframe, by = c("iso3c", "year"))
  return(combodata2)
}

#this one uses numerical ccodes from polity and cown/gwn codes from WDI as mergers to fix divergent scodes
#this fixes polity merge failures for Congo (Dem Rep), Cote Divore, Ethiopia, South Sudan, Sudan
#this is implemented on case-by-case basis
alt_merger = function (WDIdataframe,aiddataframe) {
  base2 <- codelist_panel %>% filter(year > 1994) %>% filter(year < 2015) %>% dplyr::select(country.name.en,year,cown,gwn,gwc,iso2c,iso3c) #using per recommendation of countrycode doc
  combodata2 <- left_join(WDIdataframe,base2,by = c("iso3c", "year"))
  combodata2 <- left_join(combodata2, politysmall, by = c("gwn" = "ccode", "year" = "year"))
  combodata2 <- left_join(combodata2, vdemsmall, by = c("cown" = "COWcode", "year" = "year"))
  combodata2 <- left_join(combodata2, aiddataframe, by = c("iso3c", "year"))
  return(combodata2)
}

### Define function to clean up extraneous variables from merging, and do other misc cleaning
cleaner = function (mergeddataframe) {
  intermed <- dplyr::select(mergeddataframe, -c(iso2c.x,country.name.en,cown,gwn,gwc,iso2c.y,ccode,country.y)) #clean up leftover and duplicate columns for ease of view
  intermed <- rename(intermed, gdppc = NY.GDP.PCAP.KD, country = country.x) #rename variables for ease of use
  intermed[sapply(intermed,is.infinite)] <- NA #Clean up leftover Inf values in population column, or glm throws error
  return(intermed)
}

#leaves in ccode and gwn
alt_cleaner = function (mergeddataframe) {
  intermed <- dplyr::select(mergeddataframe, -c(iso2c.x,country.name.en,cown,gwc,iso2c.y,country.y)) #clean up leftover and duplicate columns for ease of view
  intermed <- rename(intermed, gdppc = NY.GDP.PCAP.KD, country = country.x) #rename variables for ease of use
  intermed[sapply(intermed,is.infinite)] <- NA #Clean up leftover Inf values in population column, or glm throws error
  return(intermed)
}

### Define function to log everything that needs logging (aid, lagged aid, and worldpop variables)
lumberjack = function (dataframe) {
  dataframe$logaid <- ifelse(dataframe$aid == 0, dataframe$aid+1,dataframe$aid)
  dataframe$logaid <- log(dataframe$logaid)
  
  dataframe$loglaggedaid <- ifelse(dataframe$laggedaid == 0, dataframe$laggedaid+1,dataframe$laggedaid)
  dataframe$loglaggedaid <- log(dataframe$loglaggedaid)
  
  dataframe$logworldpop <- ifelse(dataframe$worldpop == 0, dataframe$worldpop+1,dataframe$worldpop)
  dataframe$logworldpop <- log(dataframe$logworldpop)
  return(dataframe)
}

### A function containing all of the above in the proper order
overall_formatter = function (dataframe) {
  output1 <- preprocessor(dataframe)
  output2 <- aiddata_formatter(output1)
  output3 <- WDIgrabber(output2)
  #View(output3) #visually check that iso3c codes are correct, since I cannot find shapeName documentation to confirm
  output4 <- merger(output3,output2)
  output5 <- cleaner(output4)
  output6 <- lumberjack(output5)
  return(output6)
}

#version that uses alt_merger discussed above
alt_overall_formatter = function (dataframe) {
  output1 <- preprocessor(dataframe)
  output2 <- aiddata_formatter(output1)
  output3 <- WDIgrabber(output2)
  #View(output3) #visually check that iso3c codes are correct, since I cannot find shapeName documentation to confirm
  output4 <- alt_merger(output3,output2)
  output5 <- alt_cleaner(output4)
  output6 <- lumberjack(output5)
  return(output6)
}

#### Other Data Cleaning

### Obtain list of unique conflict IDs for civil wars in my timeframe and region
# filter ACD for civil wars and internationalized civil wars, in Africa (region 4), from 1995 to 2014
ACD1 <- ACD %>% filter(type_of_conflict == 3 | type_of_conflict == 4) %>% filter(region == 4) %>% filter(year >= 1995) %>% filter(year <= 2014)
ACD_conflict_id <- unique(ACD1$conflict_id) #list of unique conflict ids from ACD1

## Filter GED to only contain incidents consistent with ACD civil wars listed above, at sufficient level of precision
GED1 <- GED %>% filter(conflict_new_id %in% ACD_conflict_id) %>% filter(where_prec < 5)
#get list of states with conflict events (note this list includes states outside SSA)
countrylist <- unique(GED1$country)

## Filter GED1 to only have the states in my analysis (those in SubSaharan Africa)
GED2 <- GED1 %>% filter(country == "Angola" | country == "Burundi" | country == "Cameroon" | country == "Central African Republic" |
                          country == "Chad" | country == "Comoros" | country == "DR Congo (Zaire)" | country == "Congo" | 
                          country == "Ivory Coast" | country == "Eritrea" | country == "Ethiopia" | country == "Guinea-Bissau" | 
                          country == "Guinea" | country == "Kenya" | country == "Lesotho" | country == "Liberia" | country == "Mali" | 
                          country == "Mauritania" | country == "Mozambique" | country == "Niger" | country == "Nigeria" |
                          country == "Rwanda" | country == "Senegal" | country == "Sierra Leone" | country == "Somalia" | 
                          country == "South Sudan" | country == "Sudan")
#### End set-up


### Read in country raw data


## Angola
raw <- read.csv("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/Post-Midwest v5/Data/Aiddata/Angola/639d54f7adcea02b03196402_results.csv")
#add shapeName2 to enable merging with GED data by admin subdivision later
#note ADM1_NAME is called shapeName in most (all?) other than Angola
raw <- raw %>% mutate(shapeName2 = case_when(ADM1_NAME == "LUNDA SUL" ~ "Lunda Sul province",
                                             ADM1_NAME == "CABINDA" ~ "Cabinda province",
                                             ADM1_NAME == "LUNDA NORTE" ~ "Lunda Norte province",
                                             ADM1_NAME == "KWANZA NORTE" ~ "Cuanza Norte province",
                                             ADM1_NAME == "MOXICO" ~ "Moxico province",
                                             ADM1_NAME == "UÍGE" ~ "Uige province",
                                             ADM1_NAME == "BIÉ" ~ "Bie province",
                                             ADM1_NAME == "KWANZA SUL" ~ "Cuanza Sul province",
                                             ADM1_NAME == "HUAMBO" ~ "Huambo province",
                                             ADM1_NAME == "BENGUELA" ~ "Benguela province",
                                             ADM1_NAME == "MALANJE" ~ "Malanje province",
                                             ADM1_NAME == "HUÍLA" ~ "Huila province",
                                             ADM1_NAME == "KUANDO KUBANGO" ~ "Cuando Cubango province",
                                             ADM1_NAME == "CUNENE" ~ "Cunene province",
                                             ADM1_NAME == "LUANDA" ~ "Luanda province",
                                             ADM1_NAME == "NAMIBE" ~ "Namibe province",
                                             ADM1_NAME == "BENGO" ~ "Bengo province",
                                             ADM1_NAME == "ZAIRE" ~ "Zaire province",
                                             ADM1_NAME == TRUE ~ "UNKNOWN"))
angola_final <- overall_formatter(raw)


## Burundi
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Burundi/639b5b6087d2bd02fc7277a2_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Burundi") %>% filter(year >= 1995) %>% filter(year < 2015) %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Bujumbura Mairie" ~ "Bujumbura Mairie province",
                                             shapeName == "Bubanza" ~ "Bubanza province",
                                             shapeName == "Cibitoke" ~ "Cibitoke province",
                                             shapeName == "Kayanza" ~ "Kayanza province",
                                             shapeName == "Bujumbura Rural" ~ "Bujumbura Rural province",
                                             shapeName == "Bururi" ~ "Bururi province",
                                             shapeName == "Muyinga" ~ "Muyinga province",
                                             shapeName == "Muramvya" ~ "Muramvya province",
                                             shapeName == "Ngozi" ~ "Ngozi province",
                                             shapeName == "Karuzi" ~ "Karuzi province",
                                             shapeName == "Makamba" ~ "Makamba province",
                                             shapeName == "Gitega" ~ "Gitega province",
                                             shapeName == "Ruyigi" ~ "Ruyigi province",
                                             shapeName == "Rutana" ~ "Rutana province",
                                             #shapeName == "" ~ "Bujumbura province", #this was split into bujumbura marie/rural in 2000
                                             shapeName == "Cankuzo" ~ "Cankuzo province",
                                             shapeName == "Mwaro" ~ "Mwaro province",
                                             shapeName == TRUE ~ "UNKNOWN"))

#drops 42 instances

# run final formatter
burundi_final <- overall_formatter(raw)


## Cameroon
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Cameroon/639b7e2d01db915a1533bf92_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Cameroon")  %>% filter(year >= 1995) %>% filter(year < 2015) %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Far-North" ~ "Far North region",
                                             
                                             shapeName == TRUE ~ "UNKNOWN"))
# run final formatter
cameroon_final <- overall_formatter(raw)

## Central African Republic
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Central African Republic/639b609c4626df723441f802_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Central African Republic") %>% filter(year >= 1995) %>% filter(year < 2015)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Haut-Mbomou" ~ "Haut-Mbomou province",
                                             shapeName == "Mbomou" ~ "Mbomou province",
                                             shapeName == "Haute-Kotto" ~ "Haute-Kotto province",
                                             shapeName == "Bangui" ~ "Bangui province",
                                             shapeName == "Ouham" ~ "Ouham province",
                                             shapeName == "Ombella M'Poko" ~ "Ombella-M'Poko province",
                                             shapeName == "Ouham-Pendé" ~ "Ouham-Pendé province",
                                             shapeName == "Vakaga" ~ "Vakaga province",
                                             shapeName == "Bamingui-Bangoran" ~ "Bamingui-Bangoran province",
                                             shapeName == "Ouaka" ~ "Ouaka province",
                                             shapeName == "Basse-Kotto" ~ "Basse-Kotto province",
                                             shapeName == "Nana-Mambéré" ~ "Nana-Mambéré province",
                                             shapeName == "Mambéré-Kadéï" ~ "Mambéré-Kadéi province",
                                             shapeName == "Sangha-Mbaéré" ~ "Sangha-Mbaéré province",
                                             shapeName == "Nana-Grébizi" ~ "Nana-Grébizi province",
                                             shapeName == TRUE ~ "UNKNOWN"))
     

# run final formatter
central_africa_republic_final <- overall_formatter(raw)

## Chad
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Chad/639ba22a57131f19f0112eb4_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Chad")  %>% filter(year >= 1995) %>% filter(year < 2015) %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Lac" ~ "Lac region",
                                             shapeName == "Logone Oriental" ~ "Logone Oriental department",
                                             shapeName == "Logone Occidental" ~ "Logone Occidental region",
                                             shapeName == "Tibesti" ~ "Tibesti department",
                                            # shapeName == "Wadi Fira" ~ "Biltine department", #drop 2
                                             #shapeName == "" ~ "Bet department", #drops 2
                                             #shapeName == "" ~ "Ennedi department", #drops 5
                                             shapeName == "Borkou" ~ "Borkou department",
                                             #shapeName == "" ~ "Borkou-Ennedi-Tibesti region", #drops 9
                                             shapeName == "Ouaddaï" ~ "Ouaddai region",
                                             shapeName == "Guéra" ~ "Guera region",
                                             shapeName == "N'Djamena Region" ~ "Ville de N'Djamena",
                                             shapeName == "Wadi Fira" ~ "Wadi Fira region",
                                             shapeName == "Salamat" ~ "Salamat region",
                                             shapeName == "Sila" ~ "Sila region",
                                             shapeName == "N'Djamena Region" ~ "N'Djamena region",
                                             shapeName == "Ennedi-Est" ~ "Ennedi region",
                                             shapeName == "Ennedi-Ouest" ~ "Ennedi region",
                                             shapeName == TRUE ~ "UNKNOWN"))
# run final formatter
chad_final <- overall_formatter(raw)


## Comoros (is island)
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Comoros/639b8bbf57131f19f0112eb3_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Comoros") %>% filter(year >= 1995) %>% filter(year < 2015) %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Anjouan" ~ "Anjouan province"))
# run final formatter
comoros_final <- overall_formatter(raw)

## Congo (Dem Rep)
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Congo (Dem. Rep.)/639b601e5e8da36c8d322902_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "DR Congo (Zaire)") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(#shapeName == "North Kivu" ~ "Nord Kivu region", #drop 17
                                             # == "South Kivu" ~ "Sud Kivu region", #drop 10
                                            # shapeName == "" ~ "Haut-Zaïre region", #drops 13
                                             shapeName == "Maniema" ~ "Maniema region",
                                             shapeName == "Tanganyika" ~ "Shaba region",
                                             shapeName == "Haut-Lomami" ~ "Shaba region",
                                             shapeName == "Lualaba" ~ "Shaba region",
                                             shapeName == "Haut-Katanga" ~ "Shaba region",
                                             shapeName == "Kwango" ~ "Bandundu region",
                                             shapeName == "Kwilu" ~ "Bandundu region",
                                             shapeName == "Mai-Ndombe" ~ "Bandundu region",
                                             shapeName == "Kinshasa" ~ "Kinshasa city region",
                                             shapeName == "Lower Uele" ~ "Orientale province",
                                             shapeName == "Upper Uele" ~ "Orientale province",
                                             shapeName == "Ituri" ~ "Orientale province",
                                             shapeName == "Tshopo" ~ "Orientale province",
                                             shapeName == "Équateur" ~ "Équateur province",
                                             shapeName == "Kasai-Oriental" ~ "Kasaï Oriental province",
                                             shapeName == "South Kivu" ~ "Sud Kivu province",
                                             shapeName == "Kinshasa" ~ "Kinshasa city province",
                                             shapeName == "Kongo-Central" ~ "Bas Congo province",
                                             shapeName == "North Kivu" ~ "Nord Kivu province",
                                             shapeName == "Haut-Katanga" ~ "Katanga province",
                                             shapeName == "Maniema" ~ "Maniema province",
                                             shapeName == "Ituri" ~ "Ituri province",
                                             shapeName == "Kasai" ~ "Kasaï Occidental province",
                                             shapeName == "Central Kasai" ~ "Kasaï Occidental province",
                                             shapeName == "Kongo-Central" ~ "Bas Congo region",
                                             shapeName == TRUE ~ "UNKNOWN"))
# run final formatter
congo_dem_rep_final <- alt_overall_formatter(raw)

## Congo (Rep)
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Congo (Rep.)/639b612f87d2bd02fc7277a3_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Congo")
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Pool" ~ "Pool region",
                                             shapeName == "Brazzaville" ~ "Brazzaville region",
                                             shapeName == "Kouilou" ~ "Kouilou region",
                                             shapeName == "Bouenza" ~ "Bouenza region",
                                             shapeName == "Niari" ~ "Niari region",
                                             shapeName == "Lékoumou" ~ "Lekoumou region",
                                             shapeName == "Plateaux" ~ "Plateaux region",
                                             shapeName == "Pointe-Noire" ~ "Pointe-Noire region",
                                             shapeName == TRUE ~ "UNKNOWN"))

# run final formatter
congo_rep_final <- overall_formatter(raw)

## Cote D'Ivoire/Ivory Coast
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Cote D'Ivoire/639b6691bea77353f22e2283_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Ivory Coast") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Valle Du Bandama" ~ "Vallée du Bandama region",
                                             shapeName == "Savanes" ~ "Savanes region",
                                             shapeName == "Sassandra-Marahoue" ~ "Haut-Sassandra region",
                                             shapeName == "Lacs" ~ "N'zi-Comoé region",
                                            # shapeName == "Montagnes" ~ "Dix-Huit Montagnes region", #drops 18
                                             #shapeName == "Montagnes" ~ "Moyen-Cavally region", #drops 2
                                             shapeName == "Bas-Sassandra" ~ "Bas-Sassandra region",
                                             shapeName == "Sassandra-Marahoue" ~ "Marahoue region",
                                             shapeName == "Lagunes" ~ "Lagunes district",
                                             shapeName == "Lacs" ~ "Lacs district",
                                             shapeName == "Montagnes" ~ "Dix-Huit Montagnes district",
                                            # shapeName == "Montagnes" ~ "Moyen-Cavally district", #drops 4
                                            # shapeName == "Lagunes" ~ "Lagunes region", #drops 2
                                             shapeName == TRUE ~ "UNKNOWN"))


cote_divoire_final <- alt_overall_formatter(raw)

## Eritrea
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Eritrea/639b81224626df723441f803_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Eritrea") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Anseba Region" ~ "Anseba region",
                                             shapeName == "Gash-Barka Region" ~ "Gash-Barka region",
                                            # ADM1_NAME == "" ~ "Gash-Setit province", #drop 1
                                             shapeName == TRUE ~ "UNKNOWN"))
# run final formatter
eritrea_final <- overall_formatter(raw)


## Ethiopia
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Ethiopia/639b82a32dce58383a1baa22_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Ethiopia") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Addis Ababa" ~ "Addis Ababa Chartered city",
                                             shapeName == "Somali" ~ "Somali state",
                                             shapeName == "Oromia" ~ "Oromiya state",
                                             shapeName == "Hareri" ~ "Harari state",
                                             shapeName == "Afar" ~ "Afar state",
                                             shapeName == "SNNPR" ~ "SNNP state",
                                             shapeName == "Dire Dawa" ~ "Dire Dawa Chartered city", 
                                             shapeName == "Beneshangul Gumu" ~ "Benishangul-Gumuz state",
                                             shapeName == "Gambela" ~ "Gambella state",
                                             shapeName == "Amhara" ~ "Amhara state",
                                             #shapeName == "" ~ "Dire Dawa Chartered City state", #drop 7
                                             shapeName == TRUE ~ "UNKNOWN"))
        
# run final formatter
ethiopia_final <- alt_overall_formatter(raw)


## Guinea
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Guinea/639b89b987d2bd02fc7277a5_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Guinea")  %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Nzerekore" ~ "Nzerekore region",
                                             shapeName == "Kindia" ~ "Kindia region",
                                             shapeName == "Mamou" ~ "Mamou region",
                                             shapeName == "Faranah" ~ "Faranah region",
                                             shapeName == TRUE ~ "UNKNOWN"))
# run final formatter
guinea_final <- overall_formatter(raw)

## Guinea-Bissau
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Guinea-Bissau/639b8a5b87d2bd02fc7277a6_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Guinea-Bissau") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Oio" ~ "Oio region",
                                             shapeName == "Bissau" ~ "Sector Autonomo de Bissau region",
                                             shapeName == "Biombo" ~ "Biombo region",
                                             shapeName == "Cacheu" ~ "Cacheu region",
                                             shapeName == "Gabu" ~ "Gabu region",
                                             shapeName == "Bafatá" ~ "Bafata region",
                                             shapeName == TRUE ~ "UNKNOWN"))
# run final formatter
guinea_bissau_final <- overall_formatter(raw)

## Kenya
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Kenya/639b8b5557131f19f0112eb2_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Kenya") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Garissa" ~ "Garissa county",
                                             shapeName == "Nairobi" ~ "Nairobi city county",
                                             shapeName == "Kwale" ~ "Kwale county",
                                             shapeName == "Mandera" ~ "Mandera county",
                                             shapeName == "Lamu" ~ "Lamu county",
                                             shapeName == "Tana River" ~ "Tana River county",
                                             shapeName == "Mombasa" ~ "Mombasa county",
                                             shapeName == "Wajir" ~ "Wajir county",
                                             #shapeName == "" ~ "Eastern province", #drop 3
                                             #shapeName == "" ~ "Nairobi province", #drop 1
                                             #shapeName == "" ~ "North Eastern province", #drop 14
                                             #shapeName == "" ~ "Coast province", #drop 6
                                             shapeName == TRUE ~ "UNKNOWN"))
# run final formatter
kenya_final <- overall_formatter(raw)

## Lesotho
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Lesotho/639b8c81bb8dda51744fb4c2_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Lesotho") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Maseru District" ~ "Maseru district",
                                             shapeName == "Thaba-Tseka District" ~ "Thaba-Tseka district",
                                             shapeName == TRUE ~ "UNKNOWN"))
# run final formatter
lesotho_final <- overall_formatter(raw)

## Liberia
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Liberia/639b8c172dce58383a1baa25_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Liberia") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Lofa" ~ "Lofa county",
                                             shapeName == "Gbarpolu" ~ "Gbarpolu county",
                                             shapeName == "Bomi" ~ "Bomi county",
                                             shapeName == "Margibi" ~ "Margibi county",
                                             shapeName == "Bong" ~ "Bong county",
                                             shapeName == "Grand Cape Mount" ~ "Grand Cape Mount county",
                                             shapeName == "Grand Gedeh" ~ "Grand Gedeh county",
                                             shapeName == "Montserrado" ~ "Montserrado county",
                                             shapeName == "Nimba" ~ "Nimba county",
                                             shapeName == "Grand Bassa" ~ "Grand Bassa county",
                                             shapeName == "Sinoe" ~ "Sinoe county",
                                             shapeName == "River Gee" ~ "River Gee county",
                                             shapeName == TRUE ~ "UNKNOWN"))
# run final formatter
liberia_final <- overall_formatter(raw)



## Mali
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Mali/639b8e1ebb8dda51744fb4c3_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Mali") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Segou" ~ "Segou region",
                                             shapeName == "Tombouctou" ~ "Tombouctou region",
                                             shapeName == "Koulikouro" ~ "Koulikoro region",
                                             shapeName == "Kidal" ~ "Kidal region",
                                             shapeName == "Gao" ~ "Gao region",
                                             shapeName == "Mopti" ~ "Mopti region",
                                             shapeName == "Bamako" ~ "Bamako region",
                                             #shapeName == "" ~ "Timetrine region", #drop 1. Timetrine is in Tessalit in Kidal region
                                             shapeName == "Kayes" ~ "Kayes region",
                                             shapeName == TRUE ~ "UNKNOWN"))
# run final formatter
mali_final <- overall_formatter(raw)

## Mauritania
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Mauritania/639b97fcbb8dda51744fb4c4_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Mauritania") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Tiris Zemmour" ~ "Tiris Zemmour region",
                                             shapeName == "Hodh ech Chargui" ~ "Hodh ech Chargui region",
                                             shapeName == "Nouakchott" ~ "Nouakchott region",
                                             shapeName == "Trarza" ~ "Trarza region",
                                             shapeName == "Brakna" ~ "Brakna region",
                                             shapeName == TRUE ~ "UNKNOWN"))

# run final formatter
mauritania_final <- overall_formatter(raw)



## Mozambique
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Mozambique/639b9bc601db915a1533bf93_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Mozambique") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Sofala" ~ "Sofala province",
                                             shapeName == "Cabo Delgado" ~ "Cabo Delgado province",
                                             shapeName == "Nampula" ~ "Nampula province",
                                             shapeName == "Inhambane" ~ "Inhambane province",
                                             shapeName == "Tete" ~ "Tete province",
                                             shapeName == "Zambézia" ~ "Zambézia province",
                                             shapeName == TRUE ~ "UNKNOWN"))
 
# run final formatter
mozambique_final <- overall_formatter(raw)

## Namibia - NOT INCLUDED
#raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
#                       "Post-Midwest v3/Data/Aiddata/Namibia/639b9c0c01db915a1533bf94_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Namibia") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
#raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "" ~ "Caprivi region", #drop 6
#                                             shapeName == "" ~ "Okavango region", #drop 2
#                                             shapeName == TRUE ~ "UNKNOWN"))
# Could not resolve split of Caprivi region into 2 provinces. Could not find Okavango
#namibia_final <- overall_formatter(raw)

## Niger
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Niger/639b9d96bea77353f22e2285_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Niger") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "" ~ "Tahoua region", #drop 2
                                             shapeName == "" ~ "Diffa region", #drop 4
                                             shapeName == "" ~ "Agadez region", #drop 30
                                             shapeName == "Dossa" ~ "Dosso region",
                                             shapeName == "Niamey" ~ "Niamey region",
                                             shapeName == "" ~ "Zinder region", #drop 1
                                             shapeName == "Tillaberi" ~ "Tillabéri region",
                                             shapeName == TRUE ~ "UNKNOWN"))
# run final formatter
niger_final <- overall_formatter(raw)

## Nigeria
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Nigeria/639b9e240eab000f0e4d7c42_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Nigeria") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Borno" ~ "Borno state",
                                             shapeName == "Bauchi" ~ "Bauchi state",
                                             shapeName == "Yobe" ~ "Yobe state",
                                             shapeName == "Adamawa" ~ "Adamawa state",
                                             shapeName == "Kano" ~ "Kano state",
                                             shapeName == "Kaduna" ~ "Kaduna state",
                                             shapeName == "Fct" ~ "Federal Capital territory",
                                             shapeName == "Katsina" ~ "Katsina state",
                                             shapeName == "Sokoto" ~ "Sokoto state",
                                             shapeName == "Gombe" ~ "Gombe state",
                                             shapeName == "Plateau" ~ "Plateau state",
                                             shapeName == "Jigawa" ~ "Jigawa state",
                                             shapeName == "Niger" ~ "Niger state",
                                             shapeName == "Kogi" ~ "Kogi state",
                                             shapeName == "Taraba" ~ "Taraba state",
                                             shapeName == "Ekiti" ~ "Ekiti state",
                                             shapeName == "Rivers" ~ "Rivers state",
                                             shapeName == TRUE ~ "UNKNOWN"))


# run final formatter
nigeria_final <- overall_formatter(raw)

## Rwanda (lots of drops)
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Rwanda/639b9e79b55c4676a54e95e2_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Rwanda") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)
#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(#shapeName == "" ~ "Gikongoro prefecture", #drops 108 total
                                             #shapeName == "" ~ "Gisenyi prefecture",
                                             #shapeName == "" ~ "Kibuye prefecture",
                                             #shapeName == "" ~ "Cyangugu prefecture",
                                             #shapeName == "" ~ "Ruhengeri prefecture",
                                             #shapeName == "" ~ "Kigali prefecture",
                                             #shapeName == "" ~ "Gitarama prefecture",
                                             #shapeName == "" ~ "Kigali-Rural prefecture",
                                             #shapeName == "" ~ "Byumba prefecture",
                                             shapeName == "Western Province" ~ "Western province",
                                             #shapeName == "" ~ "Gisenyi province",
                                             shapeName == "Northern Province" ~ "Northern province",
                                             shapeName == TRUE ~ "UNKNOWN"))
# run final formatter
rwanda_final <- overall_formatter(raw)

## Senegal
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Senegal/639ba0f72b8d081d15638343_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Senegal") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)

#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Ziguinchor" ~ "Ziguinchor region",
                                             shapeName == "Kolda" ~ "Kolda region",
                                             shapeName == "Tambacounda" ~ "Tambacounda region",
                                             shapeName == "Sedhiou" ~ "Sédhiou region",
                                             shapeName == TRUE ~ "UNKNOWN"))

# run final formatter
senegal_final <- overall_formatter(raw)

## Sierra Leone
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Sierra Leone/639ba0422b8d081d15638342_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Sierra Leone") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)

#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Norther" ~ "Northern province",
                                             shapeName == "Eastern" ~ "Eastern province",
                                             shapeName == "Southern" ~ "Southern province",
                                             shapeName == "Western Area" ~ "Western province",
                                             shapeName == TRUE ~ "UNKNOWN"))

# run final formatter
sierra_leone_final <- overall_formatter(raw)

## Somalia
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Somalia/639ba14701db915a1533bf95_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Somalia") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)

#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Gedo" ~ "Gedo region",
                                             shapeName == "Hiiraan" ~ "Hiran region",
                                             shapeName == "Banadir" ~ "Banaadir region",
                                             shapeName == "Lower Shebelle" ~ "Lower Shabelle region",
                                             shapeName == "Middle Shebelle" ~ "Middle Shabelle region",
                                             shapeName == "Galgaduud" ~ "Galgudud region",
                                             shapeName == "Bay" ~ "Bay region",
                                             shapeName == "Middle Juba" ~ "Middle Juba region",
                                             shapeName == "Lower Juba" ~ "Lower Juba region",
                                             shapeName == "Mudug" ~ "Mudug region",
                                             shapeName == "Bakool" ~ "Bakool region",
                                             shapeName == "Woqooyi Galbeed" ~ "Woqooyi Galbeed region",
                                             shapeName == "Nugaal" ~ "Nugaal region",
                                             shapeName == "Sanaag" ~ "Sanaag region",
                                             shapeName == "Bari" ~ "Bari region",
                                             shapeName == "Sool" ~ "Sool region",
                                             shapeName == TRUE ~ "UNKNOWN"))

# run final formatter
somalia_final <- overall_formatter(raw)



## South Sudan
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/South Sudan/639ba19801db915a1533bf96_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "South Sudan") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)

#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "Western Bahr el Ghazal" ~ "West Bahr-al-Ghazal state",
                                             shapeName == "Jonglei" ~ "Jonglei state",
                                             shapeName == "Upper Nile" ~ "Upper Nile state",
                                             shapeName == "Central Equatoria" ~ "Central Equatoria state",
                                             shapeName == "Unity" ~ "Unity state",
                                             shapeName == "Lakes" ~ "Lakes state",
                                             shapeName == "Warrap" ~ "Warap state",
                                             shapeName == "Northern Bahr el Ghazal" ~ "North Bahr-al-Ghazal state",
                                             shapeName == TRUE ~ "UNKNOWN"))
#due to weirdness of South Sudan recent independence, have to manually remove years in which it wasn't from output3
#otherwise, merger function has many-to-many issues 

output1 <- preprocessor(raw)
output2 <- aiddata_formatter(output1)
output3 <- WDIgrabber(output2)
output3 <- output3[1:4,]
output4 <- alt_merger(output3,output2)
output5 <- alt_cleaner(output4)
output6 <- lumberjack(output5)
south_sudan_final <- output6

## Sudan
raw <- read.csv(paste0("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/",
                       "Post-Midwest v5/Data/Aiddata/Sudan/639b9ecebb8dda51744fb4c5_results.csv"))
# get ADM1 names from GED
#test <- GED1 %>% filter(country == "Sudan") %>% filter(year >= 1995) %>% filter(year < 2015)  %>% filter(where_prec < 5)
#unique(test$adm_1)

#create shapeName2
raw <- raw %>% mutate(shapeName2 = case_when(shapeName == "" ~ "Southern Kordofan state", #drop 123
                                             shapeName == "" ~ "Eastern Equatoria state", #drop 48
                                             shapeName == "" ~ "Bahr-al-Jabal state", #honestly probably dropping most of these
                                             shapeName == "" ~ "Western Equatoria state",
                                             shapeName == "Gedaref" ~ "Gedaref state",
                                             shapeName == "Blue Nile" ~ "Blue Nile state",
                                             shapeName == "Red Sea" ~ "Red Sea state",
                                             shapeName == "Kassala" ~ "Kassala state",
                                             shapeName == "" ~ "Upper Nile state",
                                             shapeName == "" ~ "Jonglei state",
                                             shapeName == "" ~ "Lakes state",
                                             shapeName == "" ~ "Warrap state",
                                             shapeName == "" ~ "Western Bahr-al-Ghazal state",
                                             shapeName == "" ~ "Northern Bahr-al-Ghazal state",
                                             shapeName == "" ~ "Unity state",
                                             shapeName == "Khartoum" ~ "Khartoum state",
                                             shapeName == "" ~ "Western Darfur state",
                                             shapeName == "" ~ "Northern Darfur state",
                                             shapeName == "" ~ "Southern Darfur state",
                                             shapeName == "" ~ "Northern Kordofan state",
                                             shapeName == "West Kordofan" ~ "West Kordofan state",
                                             shapeName == "" ~ "Central Equatoria state",
                                             shapeName == "" ~ "Western Bahr-al-Ghazal state",
                                             shapeName == "East Darfur" ~ "East Darfur state",
                                             shapeName == "Central Darfur" ~ "Central Darfur state",
                                             shapeName == "South Darfur" ~ "South Darfur state",
                                             shapeName == "South Kordofan" ~ "South Kordofan state",
                                             shapeName == "North Kordofan" ~ "North Kordofan state",
                                             shapeName == "North Darfur" ~ "North Darfur state",
                                             shapeName == TRUE ~ "UNKNOWN"))



# run final formatter
sudan_final <- alt_overall_formatter(raw)

## Uganda -- Excluded. 
#Exclusion bc UCDP uses ADM2 subdivisions, which Aiddata has, not ADM1. ADM2 is 135 districts


## Combining all 27 countries into a dataframe
finaldata <- bind_rows(angola_final,burundi_final,cameroon_final,central_africa_republic_final,chad_final,comoros_final,
                       congo_dem_rep_final, congo_rep_final,cote_divoire_final,eritrea_final,ethiopia_final,guinea_bissau_final,
                       guinea_final,kenya_final,lesotho_final,liberia_final,mali_final,mauritania_final,mozambique_final,niger_final,
                       nigeria_final,rwanda_final,senegal_final,sierra_leone_final,somalia_final,south_sudan_final,sudan_final)
#saving it
write.csv(finaldata,"C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/Post-Midwest v6/Data/SSAdata_v6_civwar.csv", row.names = F)

################### End of data formatting 

####data read-in and prep
SSAdata <- read.csv("C:/Users/steve/OneDrive/Documents/Own Research/FDI-Conflict/Paper 1/Post-Midwest drafts/Post-Midwest v6/Data/SSAdata_v6_civwar.csv")

#all violence in GED1 and derivatives is state-based, which means side_a is always the state gov and side_b always rebels/rival gov
#so, deaths_a is deaths experienced by side a (gov), deaths_b deaths xp'd by side b (rebels), and deaths_civilians the civ collateral damage
GED_deaths_a <- GED2 %>% filter(where_prec < 5) %>% filter(year >= 1995) %>% filter(year <= 2014) %>% group_by(country,year,adm_1) %>% summarise(sum(deaths_a)) 
GED_deaths_b <- GED2 %>% filter(where_prec < 5) %>% filter(year >= 1995) %>% filter(year <= 2014) %>% group_by(country,year,adm_1) %>% summarise(sum(deaths_b)) 
GED_deaths_civ <- GED2 %>% filter(where_prec < 5) %>% filter(year >= 1995) %>% filter(year <= 2014) %>% group_by(country,year,adm_1) %>% summarise(sum(deaths_civilians)) 
GED_deaths_total <- GED2 %>% filter(where_prec < 5) %>% filter(year >= 1995) %>% filter(year <= 2014) %>% group_by(country,year,adm_1) %>% summarise(sum(best))

#merge to SSAdata
SSAdata2 <- left_join(SSAdata,GED_deaths_a, by=c("country"="country","year"="year","shapeName2"="adm_1")) 
SSAdata3 <- left_join(SSAdata2,GED_deaths_b, by=c("country"="country","year"="year","shapeName2"="adm_1")) 
SSAdata4 <- left_join(SSAdata3,GED_deaths_civ, by=c("country"="country","year"="year","shapeName2"="adm_1")) 
SSAdatafinal <- left_join(SSAdata4,GED_deaths_total, by=c("country"="country","year"="year","shapeName2"="adm_1")) 

## General cleaning
#name clean-up, NA changes to 0, etc
SSAdatafinal <- rename(SSAdatafinal,deaths_a = "sum(deaths_a)")
SSAdatafinal <- rename(SSAdatafinal,deaths_b = "sum(deaths_b)")
SSAdatafinal <- rename(SSAdatafinal,deaths_civ = "sum(deaths_civilians)")
SSAdatafinal <- rename(SSAdatafinal,deaths_total = "sum(best)")
SSAdatafinal[,c("deaths_a","deaths_b","deaths_civ","deaths_total")][is.na(SSAdatafinal[,c("deaths_a","deaths_b","deaths_civ","deaths_total")])] <- 0

SSAdatafinal$ucdpdeaths <- as.integer(SSAdatafinal$ucdpdeaths) #round deaths to nearest integer bc aiddata has non-integer values
#fix impossibly large population values for year 2000 by making them NA
SSAdatafinal$worldpop <- ifelse(SSAdatafinal$worldpop > 33279608, NA,SSAdatafinal$worldpop)
SSAdatafinal$logworldpop <- ifelse(SSAdatafinal$logworldpop > 17.32046, NA,SSAdatafinal$logworldpop)

## Create change in aid year on year variable, using lagged aid
#uses lead not lag fn bc of data structure
SSAdatafinal <- SSAdatafinal %>% group_by(country) %>% group_by(year) %>% group_by(shapeName) %>% mutate(aidchange = laggedaid - dplyr::lead(laggedaid,n=1))
SSAdatafinal$country <- as.factor(SSAdatafinal$country) #create factor


## Recode aid and gdppc in millions of USD
SSAdatafinal$gdppch <- SSAdatafinal$gdppc/100 #gdppc recoded in hundreds
SSAdatafinal$gdppct <- SSAdatafinal$gdppc/1000 #gdppc recoded in thousands
SSAdatafinal$aidchangemil <- SSAdatafinal$aidchange/1000000 #aid recoded in millions

## Weight aid by population: amt of aid change in millions per million residents
SSAdatafinal$weightedaid <- SSAdatafinal$aidchangemil/(SSAdatafinal$worldpop/1000000)

#### Begin analysis 
##Shape of primary IVs
summary(SSAdatafinal$aidchangemil)
hist(SSAdatafinal$aidchangemil, breaks = 500, xlim = c(-100,100),
     main = "Aid Change Histogram",
     xlab = "Aid Change Aid change (in millions of USD)") #basically centered around 0 with some outliers
#note: x-axis labels exclude some outlier points, but do not affect the overall shape of the distribution.
summary(SSAdatafinal$weightedaid)
hist(SSAdatafinal$weightedaid, breaks = 500, xlim = c(-100,100),
     main = "Weighted Aid Variable Histogram",
     xlab = "Aid Change (in millions of USD) per million residents") #also basically centered around 0 with some outliers
#note: x-axis labels exclude some outlier points, but do not affect the overall shape of the distribution.

## basic descriptive statistics for descriptive table
descstatstable <- data.frame(means=c(meanaid=mean(SSAdatafinal$aid, na.rm = T),
                                     meanpop=mean(SSAdatafinal$worldpop, na.rm = T),
                                     meangovdeaths=mean(SSAdatafinal$deaths_a),
                                     meanrebdeaths=mean(SSAdatafinal$deaths_b),
                                     meancivdeaths=mean(SSAdatafinal$deaths_civ),
                                     meantotaldeaths=mean(SSAdatafinal$deaths_total),
                                     meangdppch=mean(SSAdatafinal$gdppch, na.rm = T),
                                     meancitytime2000=mean(SSAdatafinal$citytime_2000, na.rm = T),
                                     meancitytime2015=mean(SSAdatafinal$citytime_2015, na.rm = T),
                                     meanelectoraldem=mean(SSAdatafinal$v2x_polyarchy),
                                     meanelectoraldem=mean(SSAdatafinal$v2x_libdem),
                                     meanelectoraldem=mean(SSAdatafinal$v2x_partipdem),
                                     meanelectoraldem=mean(SSAdatafinal$v2x_delibdem),
                                     meanelectoraldem=mean(SSAdatafinal$v2x_egaldem)
                                     ), 
                             sds=c(sdaid=sd(SSAdatafinal$aid, na.rm = T),
                                   sdpop=sd(SSAdatafinal$worldpop, na.rm = T),
                                   sdgovdeaths=sd(SSAdatafinal$deaths_a),
                                   sdrebdeaths=sd(SSAdatafinal$deaths_b),
                                   sdcivdeaths=sd(SSAdatafinal$deaths_civ),
                                   sdtotaldeaths=sd(SSAdatafinal$deaths_total),
                                   sdgdppch=sd(SSAdatafinal$gdppch, na.rm = T),
                                   sdcitytime2000=sd(SSAdatafinal$citytime_2000, na.rm = T),
                                   sdcitytime2015=sd(SSAdatafinal$citytime_2015, na.rm = T),
                                   sdelectoraldem=sd(SSAdatafinal$v2x_polyarchy),
                                   sdelectoraldem=sd(SSAdatafinal$v2x_libdem),
                                   sdelectoraldem=sd(SSAdatafinal$v2x_partipdem),
                                   sdelectoraldem=sd(SSAdatafinal$v2x_delibdem),
                                   sdelectoraldem=sd(SSAdatafinal$v2x_egaldem)
                                   )
                            )
rownames(descstatstable) <- c("Aid","Population","State Deaths","Rebel Deaths","Civilian Deaths","Total Deaths",
                              "GDPPC","Travel Time (2000)","Travel Time (2015)","Electoral Democracy","Liberal Democracy",
                              "Participatory Democracy","Deliberative Democracy","Egalitarian Democracy")
descstatstable$means<- round(descstatstable$means,1)
descstatstable$sds<- round(descstatstable$sds,1)

descstatstable %>% 
  kbl(caption="Summary Statistics",
      format="latex",
      col.names = c("Mean","S.D."),
      align="r") %>%
  kable_minimal(full_width = F,)


#### Quasipoisson models, due to overdispersion in deaths


### City travel distances, 2000
## No population variable, citytime_2000
quasi_nopop_a_00 <- glm(deaths_a ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                          v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                        family = "quasipoisson", data = SSAdatafinal)
summary(quasi_nopop_a_00) #int, aid, citytime neg not sig
vif(quasi_nopop_a_00) #gdppc and vdem massive
plot_model(quasi_nopop_a_00, type = "pred", terms = c("aidchangemil [-600,-400]", "citytime_2000"),
           axis.lim = c(0,500000), colors = "bw", title = "Predicted Counts of State Forces Deaths",
           axis.title = c("Aid change (in millions of USD)","State Forces Deaths"),
           legend.title = "2000 Travel Time") #marginal effects plot. 
#weird plot, needs to go to 250k state forces deaths before anything shows up. Travel time predictions are flat.

quasi_nopop_b_00 <- glm(deaths_b ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                          v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, family = "quasipoisson", data = SSAdatafinal)
summary(quasi_nopop_b_00) #int neg, aid and citytime pos, none sig
vif(quasi_nopop_b_00, type = "predictor") #gdppc and vdem massive
plot_model(quasi_nopop_b_00, type = "pred", terms = c("aidchangemil", "citytime_2000"),
            colors = "bw", title = "Predicted Counts of Rebel Forces Deaths",
           axis.title = c("Aid change (in millions of USD)","Rebel Forces Deaths"),
           legend.title = "2000 Travel Time")
#no discernable impact. Plot not weird, at least.

quasi_nopop_tot_00 <- glm(deaths_total ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                            v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, family = "quasipoisson", data = SSAdatafinal)
summary(quasi_nopop_tot_00) #int neg, aid and citytime pos, nothing sig
vif(quasi_nopop_tot_00) #gdppc and vdem massive
plot_model(quasi_nopop_tot_00, type = "pred", terms = c("aidchangemil", "citytime_2000"),
            colors = "bw",title = "Predicted Counts of Total Deaths",
           axis.title = c("Aid change (in millions of USD)","Total Deaths"),
           legend.title = "2000 Travel Time")
#no discernable impact. Plot not weird, at least.

#table 1 is desc stats
#table 2: yearly aid chnage IV, citytime 2000 models, no population variable
stargazer(quasi_nopop_a_00,quasi_nopop_b_00,quasi_nopop_tot_00, 
          title="Quasi-Poisson Regression Results Using 2000 City Travel Times, No Population Variable", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2000)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)

## With population variable, city_time2000
quasi_pop_a_00 <- glm(deaths_a ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                        v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, family = "quasipoisson", data = SSAdatafinal)
summary(quasi_pop_a_00) ##int neg not pos; aid, citytime pos not sig
vif(quasi_pop_a_00) ##gdppc and vdem massive
plot_model(quasi_pop_a_00, type = "pred", terms = c("aidchangemil", "citytime_2000"),
           axis.lim = c(0,100), 
           colors = "bw", title = "Predicted Counts of State Forces Deaths",
           axis.title = c("Aid change (in millions of USD)","State Forces Deaths"),
           legend.title = "2000 Travel Time")
#no impact, like above

quasi_pop_b_00 <- glm(deaths_b ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                        v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, family = "quasipoisson", data = SSAdatafinal)
summary(quasi_pop_b_00) #all but int are pos, none sig
vif(quasi_pop_b_00)#gdppc and vdem massive
plot_model(quasi_pop_b_00, type = "pred", terms = c("aidchangemil", "citytime_2000"),
          axis.lim = c(0,500), 
           colors = "bw", title = "Predicted Counts of Rebel Forces Deaths",
           axis.title = c("Aid change (in millions of USD)","Rebel Forces Deaths"),
           legend.title = "2000 Travel Time")
#same as above

quasi_pop_tot_00 <- glm(deaths_total ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                          v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, family = "quasipoisson", data = SSAdatafinal)
summary(quasi_pop_tot_00) #aid and citytime pos, not sig. int neg not sig
vif(quasi_pop_tot_00) #gdppc and vdem massive
plot_model(quasi_pop_tot_00, type = "pred", terms = c("aidchangemil", "citytime_2000"),
           #axis.lim = c(0,50), 
           colors = "bw", title = "Predicted Counts of Total Deaths",
           axis.title = c("Aid change (in millions of USD)","Total Deaths"),
           legend.title = "2000 Travel Time")

#table 3: citytime 2000 models, with population variable
stargazer(quasi_pop_a_00,quasi_pop_b_00,quasi_pop_tot_00, 
          title="Quasi-Poisson Regression Results Using 2000 City Travel Times, With Population Variable", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2000)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year","Population", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)



### Citytime 2015

## No population variable, citytime_2015
quasi_nopop_a <- glm(deaths_a ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                       v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country,
                     family = "quasipoisson", data = SSAdatafinal)
summary(quasi_nopop_a) #all neg not sig
vif(quasi_nopop_a) #gdppc and vdem massive
plot_model(quasi_nopop_a, type = "pred", terms = c("aidchangemil", "citytime_2015"),
           axis.lim = c(0,1800), 
           colors = "bw", title = "Predicted Counts of State Forces Deaths",
           axis.title = c("Aid change (in millions of USD)","State Forces Deaths"),
           legend.title = "2015 Travel Time")
#weird

quasi_nopop_b <- glm(deaths_b ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                       v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country,
                     family = "quasipoisson", data = SSAdatafinal)
summary(quasi_nopop_b) #int neg not sig; aid and citytime pos not sig
vif(quasi_nopop_b) #gdppc and vdem massive
plot_model(quasi_nopop_b, type = "pred", terms = c("aidchangemil [-400,400]", "citytime_2015"),
           axis.lim = c(0,5), colors = "bw", title = "Predicted Counts of Rebel Forces Deaths",
           axis.title = c("Aid change (in millions of USD)","Rebel Forces Deaths"),
           legend.title = "2015 Travel Time")
#same

quasi_nopop_tot <- glm(deaths_total ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                         v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country,
                       family = "quasipoisson", data = SSAdatafinal)
summary(quasi_nopop_tot) #all pos not sig except int, which is neg not sig
vif(quasi_nopop_tot) ##gdppc and vdem massive
plot_model(quasi_nopop_tot, type = "pred", terms = c("aidchangemil", "citytime_2015"),
           #axis.lim = c(0,250), 
           colors = "bw", title = "Predicted Counts of Total Deaths",
           axis.title = c("Aid change (in millions of USD)","Total Deaths"),
           legend.title = "2015 Travel Time")

#table 4: citytime 2015 models, no population variable
stargazer(quasi_nopop_a,quasi_nopop_b,quasi_nopop_tot,
          title="Quasi-Poisson Regression Results Using 2015 City Travel Times, No Population Variable", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2015)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)



## With population variable, city_time2015
quasi_pop_a <- glm(deaths_a ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                     v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, 
                   family = "quasipoisson", data = SSAdatafinal)
summary(quasi_pop_a) #int neg not sig, aid and citytime pos not sig
vif(quasi_pop_a) #gdppc and vdem massive
plot_model(quasi_pop_a, type = "pred", terms = c("aidchangemil", "citytime_2015"),
           #axis.lim = c(0,10), 
           colors = "bw", title = "Predicted Counts of State Forces Deaths",
           axis.title = c("Aid change","State Forces Deaths"),
           legend.title = "2015 Travel Time")


quasi_pop_b <- glm(deaths_b ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                     v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop,
                   family = "quasipoisson", data = SSAdatafinal)
summary(quasi_pop_b) #int term neg not sig; city and aid pos not sig
vif(quasi_pop_b) ##gdppc and vdem massive
plot_model(quasi_pop_b, type = "pred", terms = c("aidchangemil", "citytime_2015"),
           #axis.lim = c(0,5), 
           colors = "bw", title = "Predicted Counts of Rebel Forces Deaths",
           axis.title = c("Aid change","Rebel Forces Deaths"),
           legend.title = "2015 Travel Time")


quasi_pop_tot <- glm(deaths_total ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                       v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, 
                     family = "quasipoisson", data = SSAdatafinal)
summary(quasi_pop_tot) #int neg, city and aid pos, none sig
vif(quasi_pop_tot) #gdppc and vdem massive
plot_model(quasi_pop_tot, type = "pred", terms = c("aidchangemil", "citytime_2015"),
           #axis.lim = c(0,50), 
           colors = "bw", title = "Predicted Counts of Total Deaths",
           axis.title = c("Aid change","Total Deaths"),
           legend.title = "2015 Travel Time")
#same

#table 5: citytime 2015 models, with population variable
stargazer(quasi_pop_a,quasi_pop_b,quasi_pop_tot,
          title="Quasi-Poisson Regression Results Using 2015 City Travel Times, With Population Variable", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2015)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year", "Population", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)

### Lagged aid change in millions per million residents
## Citytime 2000
#no population variable bc incorporated into the weighted aid variable

weighted_a_00 <- glm(deaths_a ~ weightedaid + citytime_2000 + weightedaid*citytime_2000 + gdppch + v2x_polyarchy + 
                          v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                        family = "quasipoisson", data = SSAdatafinal)
summary(weighted_a_00) #int and city pos, aid neg, none sig
vif(weighted_a_00)
plot_model(weighted_a_00, type = "pred", terms = c("weightedaid", "citytime_2000"),
           axis.lim = c(0,50), 
           colors = "bw", title = "Predicted Counts of State Forces Deaths",
           axis.title = c("Weighted Aid Change","State Forces Deaths"),
           legend.title = "2000 Travel Time")
#same shape as everything else

weighted_b_00 <- glm(deaths_b ~ weightedaid + citytime_2000 + weightedaid*citytime_2000 + gdppch + v2x_polyarchy + 
                       v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                     family = "quasipoisson", data = SSAdatafinal)
summary(weighted_b_00) #int neg, aid and city pos, none sig
vif(weighted_b_00)
plot_model(weighted_b_00, type = "pred", terms = c("weightedaid", "citytime_2000"),
           #axis.lim = c(0,50), 
           colors = "bw", title = "Predicted Counts of Rebel Forces Deaths",
           axis.title = c("Weighted Aid Change","Rebel Forces Deaths"),
           legend.title = "2000 Travel Time")
#weird

weighted_tot_00 <- glm(deaths_total ~ weightedaid + citytime_2000 + weightedaid*citytime_2000 + gdppch + v2x_polyarchy + 
                       v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                     family = "quasipoisson", data = SSAdatafinal)
summary(weighted_tot_00) #all pos none sig
vif(weighted_tot_00)
plot_model(weighted_tot_00, type = "pred", terms = c("weightedaid", "citytime_2000"),
           #axis.lim = c(0,50), 
           colors = "bw", title = "Predicted Counts of Total Deaths",
           axis.title = c("Weighted Aid Change","Total Deaths"),
           legend.title = "2000 Travel Time")
#same as usual

#table 6: weighted aid change, citytime 2000 models
stargazer(weighted_a_00,weighted_b_00,weighted_tot_00,
          title="Quasi-Poisson Regression Results Using 2000 City Travel Times, Weighted Aid IV", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Aid Change per Million Residents (in Millions USD)","City Travel Time (2000)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)

## Citytime 2015
weighted_a_15 <- glm(deaths_a ~ weightedaid + citytime_2015 + weightedaid*citytime_2015 + gdppch + v2x_polyarchy + 
                       v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                     family = "quasipoisson", data = SSAdatafinal)
summary(weighted_a_15) #all pos, none sig
vif(weighted_a_15)
plot_model(weighted_a_15, type = "pred", terms = c("weightedaid", "citytime_2015"),
         #  axis.lim = c(0,50), 
           colors = "bw", title = "Predicted Counts of State Forces Deaths",
           axis.title = c("Weighted Aid Change","State Forces Deaths"),
           legend.title = "2015 Travel Time")
#same shape as everything else

weighted_b_15 <- glm(deaths_b ~ weightedaid + citytime_2015 + weightedaid*citytime_2015 + gdppch + v2x_polyarchy + 
                       v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                     family = "quasipoisson", data = SSAdatafinal)
summary(weighted_b_15) #int neg, aid and city pos, none sig
vif(weighted_b_15)
plot_model(weighted_b_15, type = "pred", terms = c("weightedaid", "citytime_2015"),
           #axis.lim = c(0,50), 
           colors = "bw", title = "Predicted Counts of Rebel Forces Deaths",
           axis.title = c("Weighted Aid Change","Rebel Forces Deaths"),
           legend.title = "2015 Travel Time")
#same

weighted_tot_15 <- glm(deaths_total ~ weightedaid + citytime_2015 + weightedaid*citytime_2015 + gdppch + v2x_polyarchy + 
                         v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                       family = "quasipoisson", data = SSAdatafinal)
summary(weighted_tot_15) #all pos none sig
vif(weighted_tot_15)
plot_model(weighted_tot_15, type = "pred", terms = c("weightedaid", "citytime_2015"),
          axis.lim = c(0,50), 
           colors = "bw", title = "Predicted Counts of Total Deaths",
           axis.title = c("Weighted Aid Change","Total Deaths"),
           legend.title = "2015 Travel Time")
#same as usual

#table 7: weighted aid change, citytime 2015 models
stargazer(weighted_a_15,weighted_b_15,weighted_tot_15,
          title="Quasi-Poisson Regression Results Using 2015 City Travel Times, Weighted Aid IV", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Aid Change per Million Residents (in Millions USD)","City Travel Time (2015)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)



#### Robustness checks
#correlation tables per https://www.statology.org/variance-inflation-factor-r/
data1 <- SSAdatafinal[ , c("aidchangemil", "gdppch", "v2x_polyarchy", "v2x_libdem", "v2x_partipdem", "v2x_delibdem", "v2x_egaldem", "year", "citytime_2000","citytime_2015")]
cor(data1, use = "complete.obs") #should drop any rows with NA, like the regression functions
data2 <- SSAdatafinal[ , c("aidchangemil", "gdppch", "v2x_polyarchy", "v2x_libdem", "v2x_partipdem", "v2x_delibdem", "v2x_egaldem", "year", "citytime_2000","citytime_2015", "logworldpop")]
cor(data2, use = "complete.obs") 
#the problem is v-dem



#### Appendix robustness check models: dropping all v-dem
### City travel distances, 2000
## No population variable, citytime_2000
quasi_nopop_a_00_robust1 <- glm(deaths_a ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + year + country, 
                                family = "quasipoisson", data = SSAdatafinal)
summary(quasi_nopop_a_00_robust1) #Same as original:int neg not sig

quasi_nopop_b_00_robust1 <- glm(deaths_b ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + year + country, family = "quasipoisson", data = SSAdatafinal)
summary(quasi_nopop_b_00_robust1) #Same as original: #int neg, aid and citytime pos, none sig

quasi_nopop_tot_00_robust1 <- glm(deaths_total ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + year + country, family = "quasipoisson", data = SSAdatafinal)
summary(quasi_nopop_tot_00_robust1) #Same as original: int neg, aid and citytime pos, nothing sig

stargazer(quasi_nopop_a_00_robust1,quasi_nopop_b_00_robust1,quasi_nopop_tot_00_robust1, 
          title="Quasi-Poisson Regression Results Using 2000 City Travel Times, No Population Variable, Excluding V-Dem ", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2000)" ,"GDP per capita (in Hundreds USD)", "Year", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F,
          label = "robustyearly2000nopop")


## With population variable, city_time2000
quasi_pop_a_00_robust1 <- glm(deaths_a ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + year + country + logworldpop, family = "quasipoisson", data = SSAdatafinal)
summary(quasi_pop_a_00_robust1) #same as original: int neg not pos; aid, citytime pos not sig

quasi_pop_b_00_robust1 <- glm(deaths_b ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + year + country + logworldpop, family = "quasipoisson", data = SSAdatafinal)
summary(quasi_pop_b_00_robust1) #same as original: int term neg not sig

quasi_pop_tot_00_robust1 <- glm(deaths_total ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + year + country + logworldpop, family = "quasipoisson", data = SSAdatafinal)
summary(quasi_pop_tot_00_robust1) #same as original: int term neg not sig

stargazer(quasi_pop_a_00_robust1,quasi_pop_b_00_robust1,quasi_pop_tot_00_robust1, 
          title="Quasi-Poisson Regression Results Using 2000 City Travel Times, With Population Variable, Excluding V-Dem ", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2000)" ,"GDP per capita (in Hundreds USD)", "Year", "Population", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F,
          label = "robustyearly2000pop")

### Citytime 2015
## No population variable, citytime_2015
quasi_nopop_a_robust1 <- glm(deaths_a ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + year + country,
                             family = "quasipoisson", data = SSAdatafinal)
summary(quasi_nopop_a_robust1) #same as original: int neg not sig

quasi_nopop_b_robust1 <- glm(deaths_b ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + year + country,
                             family = "quasipoisson", data = SSAdatafinal)
summary(quasi_nopop_b_robust1) #same as original: int neg not sig

quasi_nopop_tot_robust1 <- glm(deaths_total ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + year + country,
                               family = "quasipoisson", data = SSAdatafinal)
summary(quasi_nopop_tot_robust1) #same as original: int neg not sig

stargazer(quasi_nopop_a_robust1,quasi_nopop_b_robust1,quasi_nopop_tot_robust1, 
          title="Quasi-Poisson Regression Results Using 2015 City Travel Times, No Population Variable, Excluding V-Dem ", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2015)" ,"GDP per capita (in Hundreds USD)", "Year", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F,
          label = "robustyearly2015nopop")

## With population variable, city_time2015
quasi_pop_a_robust1 <- glm(deaths_a ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + year + country + logworldpop, 
                           family = "quasipoisson", data = SSAdatafinal)
summary(quasi_pop_a_robust1) #Same as original with int neg not sig

quasi_pop_b_robust1 <- glm(deaths_b ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + year + country + logworldpop,
                           family = "quasipoisson", data = SSAdatafinal)
summary(quasi_pop_b_robust1) #Same as original: int term neg not sig

quasi_pop_tot_robust1 <- glm(deaths_total ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + year + country + logworldpop, 
                             family = "quasipoisson", data = SSAdatafinal)
summary(quasi_pop_tot_robust1) #same as original: int neg not sig

stargazer(quasi_pop_a_robust1,quasi_pop_b_robust1,quasi_pop_tot_robust1, 
          title="Quasi-Poisson Regression Results Using 2015 City Travel Times, With Population Variable, Excluding V-Dem ", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2015)" ,"GDP per capita (in Hundreds USD)", "Year", "Population", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F,
          label = "robustyearly2015pop")


### Lagged aid change in millions per million residents
## Citytime 2000
#no population variable bc incorporated into the weighted aid variable

weighted_a_00_robust1 <- glm(deaths_a ~ weightedaid + citytime_2000 + weightedaid*citytime_2000 + gdppch + year + country, 
                             family = "quasipoisson", data = SSAdatafinal)
summary(weighted_a_00_robust1) #same: int pos not sig


weighted_b_00_robust1 <- glm(deaths_b ~ weightedaid + citytime_2000 + weightedaid*citytime_2000 + gdppch + year + country, 
                             family = "quasipoisson", data = SSAdatafinal)
summary(weighted_b_00_robust1) #same: int neg not sig


weighted_tot_00_robust1 <- glm(deaths_total ~ weightedaid + citytime_2000 + weightedaid*citytime_2000 + gdppch + year + country, 
                               family = "quasipoisson", data = SSAdatafinal)
summary(weighted_tot_00_robust1) #same: int pos not sig

stargazer(weighted_a_00_robust1,weighted_b_00_robust1,weighted_tot_00_robust1,
          title="Quasi-Poisson Regression Results Using 2000 City Travel Times, Weighted Aid IV, No V-Dem Variables", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Aid Change per Million Residents (in Millions USD)","City Travel Time (2000)" ,"GDP per capita (in Hundreds USD)","Year", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F,
          label = "robustweighted2000")

## Citytime 2015
weighted_a_15_robust1 <- glm(deaths_a ~ weightedaid + citytime_2015 + weightedaid*citytime_2015 + gdppch + year + country, 
                             family = "quasipoisson", data = SSAdatafinal)
summary(weighted_a_15_robust1) #same: int pos not sig

weighted_b_15_robust1 <- glm(deaths_b ~ weightedaid + citytime_2015 + weightedaid*citytime_2015 + gdppch + year + country, 
                             family = "quasipoisson", data = SSAdatafinal)
summary(weighted_b_15_robust1) #same: int neg not sig

weighted_tot_15_robust1 <- glm(deaths_total ~ weightedaid + citytime_2015 + weightedaid*citytime_2015 + gdppch + year + country, 
                               family = "quasipoisson", data = SSAdatafinal)
summary(weighted_tot_15_robust1) #same: int pos not sig

stargazer(weighted_a_15_robust1,weighted_b_15_robust1,weighted_tot_15_robust1,
          title="Quasi-Poisson Regression Results Using 2000 City Travel Times, Weighted Aid IV, No V-Dem Variables", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Aid Change per Million Residents (in Millions USD)","City Travel Time (2000)" ,"GDP per capita (in Hundreds USD)","Year", "Population","Aid:Travel Interaction"),
          omit = c("country"),
          header = F,
          label = "robustweighted2015")

#### Appendix Democratic vs Autocratic Subsamples
### These will be tables 14-

demsample <- SSAdatafinal %>% filter(v2x_regime >= 2) #997 obs
autosample <- SSAdatafinal %>% filter(v2x_regime < 2) #6663 obs

### Democratic split (tables 14-19)
### City travel distances from year 2000
## No population variable
dem_nopop_a_00 <- glm(deaths_a ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                          v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                        family = "quasipoisson", data = demsample)
summary(dem_nopop_a_00) #no meaningful changes, aside from citytime sig

dem_nopop_b_00 <- glm(deaths_b ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                          v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                        family = "quasipoisson", data = demsample)
summary(dem_nopop_b_00) #same

dem_nopop_tot_00 <- glm(deaths_total ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                            v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                          family = "quasipoisson", data = demsample)
summary(dem_nopop_tot_00) #same

#table 14: yearly aid change IV, citytime 2000 models, no population variable, democratic subsample
stargazer(dem_nopop_a_00,dem_nopop_b_00,dem_nopop_tot_00, 
          title="Quasi-Poisson Regression Results Using 2000 City Travel Times, No Population Variable, with Democratic Subsample", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2000)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)

## With population variable, city_time2000
dem_pop_a_00 <- glm(deaths_a ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                        v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, 
                    family = "quasipoisson", data = demsample)
summary(dem_pop_a_00) ##citytime now sig

dem_pop_b_00 <- glm(deaths_b ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                        v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, 
                    family = "quasipoisson", data = demsample)
summary(dem_pop_b_00) #same

dem_pop_tot_00 <- glm(deaths_total ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                          v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, 
                        family = "quasipoisson", data = demsample)
summary(dem_pop_tot_00) #same

#table 15: yearly aid change IV, citytime 2000 models, with population variable
stargazer(dem_pop_a_00,dem_pop_b_00,dem_pop_tot_00, 
          title="Quasi-Poisson Regression Results Using 2000 City Travel Times, With Population Variable, Using Democratic Subsample", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2000)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year","Population", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)

### City travel distances from year 2015
## No population variable, citytime_2015
dem_nopop_a <- glm(deaths_a ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                       v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country,
                     family = "quasipoisson", data = demsample)
summary(dem_nopop_a) #the usual pattern

dem_nopop_b <- glm(deaths_b ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                       v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country,
                     family = "quasipoisson", data = demsample)
summary(dem_nopop_b) #

dem_nopop_tot <- glm(deaths_total ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                         v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country,
                       family = "quasipoisson", data = demsample)
summary(dem_nopop_tot) #

#table 16: citytime 2015 models, no population variable
stargazer(dem_nopop_a,dem_nopop_b,dem_nopop_tot,
          title="Quasi-Poisson Regression Results Using 2015 City Travel Times, No Population Variable, With Democratic Subsample", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2015)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)

## With population variable, city_time2015
dem_pop_a <- glm(deaths_a ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                     v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, 
                   family = "quasipoisson", data = demsample)
summary(dem_pop_a) #

dem_pop_b <- glm(deaths_b ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                     v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop,
                   family = "quasipoisson", data = demsample)
summary(dem_pop_b) #

dem_pop_tot <- glm(deaths_total ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                       v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, 
                     family = "quasipoisson", data = demsample)
summary(dem_pop_tot) #no meaningful changes


#table 17: citytime 2015 models, with population variable
stargazer(dem_pop_a,dem_pop_b,dem_pop_tot,
          title="Quasi-Poisson Regression Results Using 2015 City Travel Times, With Population Variable, With Democratic Subsample", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2015)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year", "Population", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)


### Lagged aid change in millions per million residents
## Citytime 2000
#no population variable bc incorporated into the weighted aid variable

dem_weighted_a_00 <- glm(deaths_a ~ weightedaid + citytime_2000 + weightedaid*citytime_2000 + gdppch + v2x_polyarchy + 
                       v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                     family = "quasipoisson", data = demsample)
summary(dem_weighted_a_00) #

dem_weighted_b_00 <- glm(deaths_b ~ weightedaid + citytime_2000 + weightedaid*citytime_2000 + gdppch + v2x_polyarchy + 
                       v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                     family = "quasipoisson", data = demsample)
summary(dem_weighted_b_00) #

dem_weighted_tot_00 <- glm(deaths_total ~ weightedaid + citytime_2000 + weightedaid*citytime_2000 + gdppch + v2x_polyarchy + 
                         v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                       family = "quasipoisson", data = demsample)
summary(dem_weighted_tot_00) #

#table 18: weighted aid change, citytime 2000 models
stargazer(dem_weighted_a_00,dem_weighted_b_00,dem_weighted_tot_00,
          title="Quasi-Poisson Regression Results Using 2000 City Travel Times, Weighted Aid IV, With Democratic Subsample", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Aid Change per Million Residents (in Millions USD)","City Travel Time (2000)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)

## Citytime 2015
dem_weighted_a_15 <- glm(deaths_a ~ weightedaid + citytime_2015 + weightedaid*citytime_2015 + gdppch + v2x_polyarchy + 
                       v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                     family = "quasipoisson", data = demsample)
summary(dem_weighted_a_15) #

dem_weighted_b_15 <- glm(deaths_b ~ weightedaid + citytime_2015 + weightedaid*citytime_2015 + gdppch + v2x_polyarchy + 
                       v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                     family = "quasipoisson", data = demsample)
summary(dem_weighted_b_15) #

dem_weighted_tot_15 <- glm(deaths_total ~ weightedaid + citytime_2015 + weightedaid*citytime_2015 + gdppch + v2x_polyarchy + 
                         v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                       family = "quasipoisson", data = demsample)
summary(dem_weighted_tot_15) #

#table 19: weighted aid change, citytime 2015 models
stargazer(dem_weighted_a_15,dem_weighted_b_15,dem_weighted_tot_15,
          title="Quasi-Poisson Regression Results Using 2015 City Travel Times, Weighted Aid IV, With Democratic Subsample", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Aid Change per Million Residents (in Millions USD)","City Travel Time (2015)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)

### Autocratic split (tables 20-25)
### City travel distances from year 2000 auto
## No population variable
auto_nopop_a_00 <- glm(deaths_a ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                         v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                       family = "quasipoisson", data = autosample)
summary(auto_nopop_a_00) #no meaningful changes

auto_nopop_b_00 <- glm(deaths_b ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                         v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                       family = "quasipoisson", data = autosample)
summary(auto_nopop_b_00) #int is neg pos here

auto_nopop_tot_00 <- glm(deaths_total ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                           v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                         family = "quasipoisson", data = autosample)
summary(auto_nopop_tot_00) #back to normal

#table 20: yearly aid change IV, citytime 2000 models, no population variable, autocratic subsample
stargazer(auto_nopop_a_00,auto_nopop_b_00,auto_nopop_tot_00, 
          title="Quasi-Poisson Regression Results Using 2000 City Travel Times, No Population Variable, with Autocratic Subsample", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2000)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)

## With population variable, city_time2000
auto_pop_a_00 <- glm(deaths_a ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                       v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, 
                     family = "quasipoisson", data = autosample)
summary(auto_pop_a_00) #

auto_pop_b_00 <- glm(deaths_b ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                       v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, 
                     family = "quasipoisson", data = autosample)
summary(auto_pop_b_00) #int is neg pos here

auto_pop_tot_00 <- glm(deaths_total ~ aidchangemil + citytime_2000 + aidchangemil*citytime_2000 + gdppch + v2x_polyarchy + 
                         v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, 
                       family = "quasipoisson", data = autosample)
summary(auto_pop_tot_00) #same

#table 21: yearly aid change IV, citytime 2000 models, with population variable
stargazer(auto_pop_a_00,auto_pop_b_00,auto_pop_tot_00, 
          title="Quasi-Poisson Regression Results Using 2000 City Travel Times, With Population Variable, Using Autocratic Subsample", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2000)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year","Population", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)

### City travel distances from year 2015
## No population variable, citytime_2015
auto_nopop_a <- glm(deaths_a ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                      v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country,
                    family = "quasipoisson", data = autosample)
summary(auto_nopop_a) #the usual pattern

auto_nopop_b <- glm(deaths_b ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                      v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country,
                    family = "quasipoisson", data = autosample)
summary(auto_nopop_b) #nothing sig here

auto_nopop_tot <- glm(deaths_total ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                        v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country,
                      family = "quasipoisson", data = autosample)
summary(auto_nopop_tot) #

#table 22: citytime 2015 models, no population variable
stargazer(auto_nopop_a,auto_nopop_b,auto_nopop_tot,
          title="Quasi-Poisson Regression Results Using 2015 City Travel Times, No Population Variable, With Autocratic Subsample", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2015)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)

## With population variable, city_time2015
auto_pop_a <- glm(deaths_a ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                    v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, 
                  family = "quasipoisson", data = autosample)
summary(auto_pop_a) #

auto_pop_b <- glm(deaths_b ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                    v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop,
                  family = "quasipoisson", data = autosample)
summary(auto_pop_b) #

auto_pop_tot <- glm(deaths_total ~ aidchangemil + citytime_2015 + aidchangemil*citytime_2015 + gdppch + v2x_polyarchy + 
                      v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, 
                    family = "quasipoisson", data = autosample)
summary(auto_pop_tot) #no meaningful changes


#table 23: citytime 2015 models, with population variable
stargazer(auto_pop_a,auto_pop_b,auto_pop_tot,
          title="Quasi-Poisson Regression Results Using 2015 City Travel Times, With Population Variable, With Autocratic Subsample", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2015)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year", "Population", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)


### Lagged aid change in millions per million residents
## Citytime 2000
#no population variable bc incorporated into the weighted aid variable

auto_weighted_a_00 <- glm(deaths_a ~ weightedaid + citytime_2000 + weightedaid*citytime_2000 + gdppch + v2x_polyarchy + 
                            v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                          family = "quasipoisson", data = autosample)
summary(auto_weighted_a_00) #

auto_weighted_b_00 <- glm(deaths_b ~ weightedaid + citytime_2000 + weightedaid*citytime_2000 + gdppch + v2x_polyarchy + 
                            v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                          family = "quasipoisson", data = autosample)
summary(auto_weighted_b_00) #

auto_weighted_tot_00 <- glm(deaths_total ~ weightedaid + citytime_2000 + weightedaid*citytime_2000 + gdppch + v2x_polyarchy + 
                              v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                            family = "quasipoisson", data = autosample)
summary(auto_weighted_tot_00) #

#table 24: weighted aid change, citytime 2000 models
stargazer(auto_weighted_a_00,auto_weighted_b_00,auto_weighted_tot_00,
          title="Quasi-Poisson Regression Results Using 2000 City Travel Times, Weighted Aid IV, With Autocratic Subsample", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Aid Change per Million Residents (in Millions USD)","City Travel Time (2000)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)

## Citytime 2015
auto_weighted_a_15 <- glm(deaths_a ~ weightedaid + citytime_2015 + weightedaid*citytime_2015 + gdppch + v2x_polyarchy + 
                            v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                          family = "quasipoisson", data = autosample)
summary(auto_weighted_a_15) #

auto_weighted_b_15 <- glm(deaths_b ~ weightedaid + citytime_2015 + weightedaid*citytime_2015 + gdppch + v2x_polyarchy + 
                            v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                          family = "quasipoisson", data = autosample)
summary(auto_weighted_b_15) #

auto_weighted_tot_15 <- glm(deaths_total ~ weightedaid + citytime_2015 + weightedaid*citytime_2015 + gdppch + v2x_polyarchy + 
                              v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country, 
                            family = "quasipoisson", data = autosample)
summary(auto_weighted_tot_15) #

#table 25: weighted aid change, citytime 2015 models
stargazer(auto_weighted_a_15,auto_weighted_b_15,auto_weighted_tot_15,
          title="Quasi-Poisson Regression Results Using 2015 City Travel Times, Weighted Aid IV, With Autocratic Subsample", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Aid Change per Million Residents (in Millions USD)","City Travel Time (2015)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year", "Aid:Travel Interaction"),
          omit = c("country"),
          header = F)

#### Cameron's Suggested aidchangemil*population models

## City travel distances, 2000
## With population variable, city_time2000
int_quasi_pop_a_00 <- glm(deaths_a ~ aidchangemil + citytime_2000 + aidchangemil*logworldpop + gdppch + v2x_polyarchy + 
                            v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, family = "quasipoisson", data = SSAdatafinal)
summary(int_quasi_pop_a_00) ##


int_quasi_pop_b_00 <- glm(deaths_b ~ aidchangemil + citytime_2000 + aidchangemil*logworldpop + gdppch + v2x_polyarchy + 
                            v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, family = "quasipoisson", data = SSAdatafinal)
summary(int_quasi_pop_b_00) #

int_quasi_pop_tot_00 <- glm(deaths_total ~ aidchangemil + citytime_2000 + aidchangemil*logworldpop + gdppch + v2x_polyarchy + 
                              v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, family = "quasipoisson", data = SSAdatafinal)
summary(int_quasi_pop_tot_00) #

#table 26: citytime 2000 models, with population variable
stargazer(int_quasi_pop_a_00,int_quasi_pop_b_00,int_quasi_pop_tot_00, 
          title="Quasi-Poisson Regression Results Using 2000 City Travel Times, With Population Variable", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2000)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year","Population", "Aid:Population Interaction"),
          omit = c("country"),
          header = F)



### Citytime 2015

## With population variable, city_time2015
int_quasi_pop_a <- glm(deaths_a ~ aidchangemil + citytime_2015 + aidchangemil*logworldpop + gdppch + v2x_polyarchy + 
                         v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, 
                       family = "quasipoisson", data = SSAdatafinal)
summary(int_quasi_pop_a) #int neg not sig, aid and citytime pos not sig


int_quasi_pop_b <- glm(deaths_b ~ aidchangemil + citytime_2015 + aidchangemil*logworldpop + gdppch + v2x_polyarchy + 
                         v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop,
                       family = "quasipoisson", data = SSAdatafinal)
summary(int_quasi_pop_b) #int term neg not sig; city and aid pos not sig


int_quasi_pop_tot <- glm(deaths_total ~ aidchangemil + citytime_2015 + aidchangemil*logworldpop + gdppch + v2x_polyarchy + 
                           v2x_libdem + v2x_partipdem + v2x_delibdem + v2x_egaldem + year + country + logworldpop, 
                         family = "quasipoisson", data = SSAdatafinal)
summary(int_quasi_pop_tot) 

#table 27: citytime 2015 models, with population variable
stargazer(int_quasi_pop_a,int_quasi_pop_b,int_quasi_pop_tot,
          title="Quasi-Poisson Regression Results Using 2015 City Travel Times, With Population Variable", 
          dep.var.labels = c("State Deaths","Rebel Deaths","Total Deaths"), 
          covariate.labels = c("Foreign Aid Change (in Millions USD)","City Travel Time (2015)" ,"GDP per capita (in Hundreds USD)","Electoral Democracy Index", "Liberal Democracy Index", "Participatory Democracy Index", "Deliberative Democracy Index", "Egalitarian Democracy Index","Year", "Population", "Aid:Population Interaction"),
          omit = c("country"),
          header = F)












##### Exploratory for paper 2

### Conflict dyad list
cni <- unique(GED2$conflict_new_id)
x <- GED2 %>% filter(conflict_new_id %in% cni) 
y <- x %>% mutate(dyadname = dyad_name) %>% mutate(id = conflict_new_id) %>% mutate(startyear = min(GED2$year)) %>% mutate(endyear = max(GED2$year)) 
q <- y[!duplicated(y$id), ] #https://www.datanovia.com/en/lessons/identify-and-remove-duplicate-data-in-r/
conlist <- q %>% dplyr::select(conflict_new_id,conflict_name,dyad_name)
#note that list will be longer than contable due to multiple separate rebels fighting same state (ex: Angola vs UNITA and 1 other group for 2 total dyads)

### Conflict dyad total and average deaths and aid
contable <- SSAdatafinal %>% group_by(country) %>% mutate(totalaid = sum(aid, na.rm = T)) %>% mutate(totaldeaths = sum(deaths_total, na.rm = T))
contable <- contable[!duplicated(contable$country), ] #https://www.datanovia.com/en/lessons/identify-and-remove-duplicate-data-in-r/
contable <- contable %>% dplyr::select(country,gdppc,totalaid,totaldeaths)
contable$totalaidmil <- contable$totalaid/1000000 
summary(contable$totaldeaths)
summary(contable$totalaidmil)



# use https://ucdp.uu.se/statebased/895 for dyad descriptions
#Rep of Congo tables
rctable<-SSAdatafinal %>% filter(country == "Congo, Rep.")
View(rctable)
rctableGED<-GED2 %>% filter(country == "Congo")
View(rctableGED)
unique(rctableGED$conflict_new_id)


#Mali tables
mtable<-SSAdatafinal %>% filter(country == "Mali")
View(mtable)
mtableGED<-GED2 %>% filter(country == "Mali")
View(mtableGED)
unique(mtableGED$conflict_new_id)


# Kenya tables
ktable<-SSAdatafinal %>% filter(country == "Kenya")
 View(ktable)
 ktableGED<-GED2 %>% filter(country == "Kenya")
 View(ktableGED)
 unique(ktableGED$conflict_new_id)
 
 
##South Sudan Tables
 sstable<-SSAdatafinal %>% filter(country == "South Sudan")
 View(sstable)
 sstableGED<-GED2 %>% filter(country == "South Sudan")
 View(sstableGED)
unique(sstableGED$conflict_new_id) 
