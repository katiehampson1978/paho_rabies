## SIRVERA DATA: Remove wildlife variants BRA & change classification for Chiapas and Mato Grosso do Sul
## Name: Kristyna Rysava
## Date: 5/12/17
## Code: a) Removes wildlife variants from Brazilian case timeseries; 
## b) converst "Absent" to "Absent-Vulnerable" for for Chiapas (Mexico) and ato Grosso do Sul (Brazil)

rm(list=ls())
library(rgdal)

## Data input
my.data <- readRDS("~/Dropbox/PAHO_RShiny/paho_shiny/BrazilOld.RData")
my.mex <- readRDS("~/Dropbox/PAHO_RShiny/paho_shiny/MexicoOld.RData")
wildvars <- read.csv("~/Dropbox/PAHOsurveillance/Analysis/ClassFrameMS/data/Brazil_WildVariants.csv")

## prep
wildvars$Date <- paste0(wildvars$Date, "-01")
setdiff(wildvars$State, my.data$State)
sum(my.data$Value)-nrow(wildvars) # 581

## assign
for(i in 1:nrow(wildvars)){
  ind <- which(my.data$State==wildvars$State[i] & my.data$Date==wildvars$Date[i])
  my.data$Value[ind] <-  my.data$Value[ind]-1
}
sum(my.data$Value) # 581

## Change classification for Chiapas and Mato Grosso do Sul
my.data$Classification[which(my.data$State=="Mato Grosso do Sul" & my.data$Classification=="Absent")] <- "Absent-Vulnerable"
my.mex$Classification[which(my.mex$State=="Chiapas" & my.mex$Classification=="Absent")] <- "Absent-Vulnerable"

## Sao Paolo
my.data$Classification[which(my.data$State=="Sao Paulo" & my.data$Classification=="Intermittent")] <- "Absent-Vulnerable"


## save
saveRDS(my.data, file = "~/Dropbox/PAHO_RShiny/paho_shiny/Brazil.RData")
saveRDS(my.mex, file = "~/Dropbox/PAHO_RShiny/paho_shiny/Mexico.RData")

# test.mex <- readRDS("~/Dropbox/PAHO_RShiny/paho_shiny/Mexico.RData")
# test.bra <- readRDS("~/Dropbox/PAHO_RShiny/paho_shiny/Brazil.RData")

### some testing
old.bra <- readRDS("~/Dropbox/PAHO_RShiny/paho_shiny/BrazilOld.RData")
new.bra <- readRDS("~/Dropbox/PAHO_RShiny/paho_shiny/Brazil.RData")

oldC <- subset(old.bra, State=="Ceara"); tail(oldC, 24)
newC <- subset(new.bra, State=="Ceara"); tail(newC, 24)

oldR <- subset(old.bra, State=="Rio Grande do Norte"); tail(oldR, 24)
newR <- subset(new.bra, State=="Rio Grande do Norte"); tail(newR, 24)


