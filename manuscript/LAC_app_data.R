## LAC DATA: Classification Framework 
## Name: Kristyna Rysava
## Date: 12/1/18
## Code: Prepare Brazil & Mexico classifcation output for the application use

rm(list=ls())
setwd("~/Dropbox/PAHO_RShiny/paho_shiny")

### Look at required format
bra.old <- readRDS("Brazil.RData")
mex.old <- readRDS("Mexico.RData")

### Prepare matching datasets 
namesBra <- unique(bra.old$State)
namesMex <- unique(mex.old$State)

## classification Brazil
classisBra <- read.csv("Brazil_classified_monthly.csv"); head(classisBra)
classisBra$state <- gsub("[.]", " ", classisBra$state)
setdiff(namesBra, classisBra$state)

## classification Mexico
classisMex <- read.csv("Mexico_classified_monthly.csv"); head(classisMex)
classisMex$state <- gsub("[.]", " ", classisMex$state)
setdiff(namesMex, classisMex$state)

## Brazil case data
bra.cases <- read.csv("~/Dropbox/PAHO_RShiny/output/Brazil_monthly_cases_state_WildRemoved.csv"); head(bra.cases, 1) # from 2010-01 to 2016-12
colnames(bra.cases)[1:ncol(bra.cases)-1] <- gsub("[.]", " ", colnames(bra.cases)[1:ncol(bra.cases)-1])
setdiff(namesBra, colnames(bra.cases)[1:ncol(bra.cases)-1])

## Mexico case data
mex.cases <- read.csv("~/Dropbox/PAHO_RShiny/output/Mexico_monthly_cases_state_WildRemoved.csv"); head(bra.cases, 1) # from 2010-01 to 2016-12
colnames(mex.cases)[1:ncol(mex.cases)-1] <- gsub("[.]", " ", colnames(mex.cases)[1:ncol(mex.cases)-1])
setdiff(namesMex, colnames(mex.cases)[1:ncol(mex.cases)-1])

## data frame Brazil
final.bra <- bra.old
final.bra$Value <- 0
final.bra$Classification <- NA

# cases
head(bra.cases, 2)
for(i in 1: length(namesBra)){
  ind <- which(final.bra$State==colnames(bra.cases)[i])
  final.bra$Value[ind] <- bra.cases[,i]
}

head(final.bra); tail(final.bra)

# classification
head(classisBra)
#colnames(classis) <- c("state", paste0("Date", 1:24))
classisBra <- as.matrix(classisBra)

for(i in 1:length(namesBra)){
  ind <- which(final.bra$State==classisBra[i,1])[61:252]
  final.bra$Classification[ind] <- as.character(classisBra[i,2:193])
}

head(final.bra); tail(final.bra)

newC <- subset(final.bra, State=="Ceara"); tail(newC, 24)
newR <- subset(final.bra, State=="Rio Grande do Norte"); tail(newR, 24)

# save
write.csv(final.bra, "~/Dropbox/PAHO_RShiny/paho_shiny/Brazil.csv", row.names=F)
# save(final.bra, file = "~/Dropbox/PAHO_RShiny/paho_shiny/Brazil.Rdata", row.names=F)
saveRDS(final.bra, file = "~/Dropbox/PAHO_RShiny/paho_shiny/Brazil.RData")

## data frame Mexico
final.mex <- mex.old
final.mex$Value <- 0
final.mex$Classification <- NA

# cases
head(mex.cases, 2)
for(i in 1: length(namesMex)){
  ind <- which(final.mex$State==colnames(mex.cases)[i])
  final.mex$Value[ind] <- mex.cases[,i]
}

head(final.mex); tail(final.mex)

# classification
head(classisMex)
classisMex <- as.matrix(classisMex)

for(i in 1:length(namesMex)){
  ind <- which(final.mex$State==classisMex[i,1])[61:252]
  final.mex$Classification[ind] <- as.character(classisMex[i,2:193])
}

head(final.mex); tail(final.mex)

newC <- subset(final.mex, State=="Chiapas"); tail(newC, 24)
newY <- subset(final.mex, State=="Yucatan"); tail(newY, 24)

# save
write.csv(final.mex, "~/Dropbox/PAHO_RShiny/paho_shiny/Mexico.csv", row.names=F)
#save(final.mex, file = "~/Dropbox/PAHO_RShiny/paho_shiny/Mexico.Rdata", row.names=F)
saveRDS(final.mex, file = "~/Dropbox/PAHO_RShiny/paho_shiny/Mexico.RData")





