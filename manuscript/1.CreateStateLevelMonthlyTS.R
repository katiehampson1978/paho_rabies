### SIRVERA DATA: Mexico & Brazil yearly classification from 2005 to 2015
## The script takes SIRVERA data (raw SIRVERA data with attempted name corrections),
## subsets for Mexico and Brazil and produces consolidated monthly time series of cases
## between Jan 1995 and now by country and by state.

rm(list=ls())
setwd("~/Dropbox/PAHO_RShiny")

library(maptools)
library(zoo)
library(lubridate)
source("~/Dropbox/PAHOsurveillance/Analysis/R/states_ts.R")

## data
dogs <- read.csv("~/Dropbox/PAHOsurveillance/Analysis/data/SIRVERA_dogs16(clean_statenames).csv")
countries <- c("Mexico", "Brazil")

## set all dates from Jan 1995 to Dec 2015
current.date <- as.Date("2015-12-01") ## NOTE: this should be automated for each time data is updated
dates <- seq(as.Date("1995-01-01"), current.date, by="months")
dates <- strftime(strptime(dates, format="%Y-%m-%d"),"%Y-%m")

###-------------------------------Subset Cases Data-------------------------------###

names <- countries
for (l in 1:length(countries)){
  ## subset country and yrs of interest
  cn = countries[l]
  yr = 1995

  country <- subset(dogs, Pais == cn & Ano >= yr)
  yrs <- sort(unique(country$Ano))

  country.f <- paste0("data/ShapeFiles/America_Adm_1/", cn, ".shp")
  sp.country <- readShapePoly(country.f)
  states <- sp.country@data$ADM1_NAME

  ## sort out full dates (months included): Converted month 0 to 1 bc do not want to exclude evidence of circulation
  country$Mes[which(country$Mes=="0")] <- 1
  country$date <- as.POSIXct(as.yearmon(paste(country$Ano, country$Mes, sep="-")))
  country$date <- strftime(strptime(country$date, format="%Y-%m-%d"),"%Y-%m")

  ## timeseries: sum all cases for each month and year/ state
  country_ts = states_ts(dates = dates, states = states, data = country)
  fb <- paste0("output/", cn, "_monthly_cases_state.csv")
  write.csv(country_ts,fb,row.names=F)

  print(setdiff(unique(country$UnidMaior),states))
}

