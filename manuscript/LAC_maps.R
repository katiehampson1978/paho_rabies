## SIRVERA DATA: Classification maps for Mexico and Brazil
## This script produces maps of Mexico and Brazil with states color-shaded depending on
## their classification of epi situation in 2005, 2010 and 2015 (with a separate figure for each year)
## and underlaied with raster data of population density in the region (BW colour scale)

rm(list=ls())
library(maptools)
library(spdep)
library(raster)
library(rgdal)
library(rgeos)
source("R/translate.R")

world <- readOGR("data/ShapeFiles/countries_shp/countries.shp", "countries")
proj4string(world)
LAC <- readOGR("data/ShapeFiles/America_Adm_1/adm1_amro_lat_long.shp","adm1_amro_lat_long")

americas = world[grep("South America|North America", world$CONTINENT),]
class(americas)
writeOGR(americas, dsn="./output/", layer="americas", driver = "ESRI Shapefile", overwrite_layer = T)

test <- readOGR(dsn="output/americas.shp", "americas")
proj4string(test)
head(test@data)

pdf("test.pdf")
plot(test)
dev.off()

###---------------------------------------Mexico---------------------------------------###
## import shapefile
mex <- readOGR("data/ShapeFiles/America_Adm_1/Mexico.shp","Mexico")
proj4string(LAC); proj4string(mex)
proj4string(mex) <- proj4string(LAC)

mex@data$ADM1_NAME <- language(mex@data$ADM1_NAME)
head(mex@data); dim(mex@data) #32 polygons

## import cases info
allstates <- read.csv("output_not_created/Mexico_classified_yearlyTW5yr.csv") # all years

## Create images for each year to combine with a slider
yrs <- c(2005:2015)
for(i in 1:length(yrs)){
  yr <- yrs[i]
  states <- data.frame(state=as.character(allstates$state),
                       phase=as.character(allstates[,colnames(allstates)==paste0("yr",yr)]))
  states$state <- gsub("[.]", " ", states$state)
  head(states)

  ## match
  SA <- world[which(world@data$CONTINENT=="South America" | world@data$CONTINENT=="North America"),]
  (rows <- match(states$state, mex@data$ADM1_NAME))
  states$phase <- as.character(states$phase)
  mex@data$classification <- states$phase
  unique(states$phase)

  ## raster
  pop <- readAsciiGrid("data/Human_pop_highres.asc")
  pop.rast <- raster(pop, layer=1, values=T)

  pop <- aggregate(pop.rast, fact=6, fun=sum) #0.25 x 0.25 degrees per cell
  res(pop.rast); res(pop)

  ## PLOTTING
  pdf(paste0("figs/Mexico/Mex_rabies_classification",yr,".pdf"), width=8, height=6)
  plot(mex)
  plot(SA, add=T, col="lightgrey")
  options(scipen=5)

  ## raster
  colours<- colorRampPalette(c("white", "grey5"))(5)
  brk <- c(0,500,5000,50000,500000,7000000)
  arg <- list(at=c(550000,1900000,3700000,5500000,6800000),
              labels=c("<500","500-5000","5000-50000","50000-500000","<7000000"), cex.axis=0.45)
  plot(pop,add=T,col=colours,breaks=brk,legend=F)

  # LEGEND NEEDS FIXING - CURRENTLY TOO SMALL TO READ PROPERLY
  plot(pop, col=colours, horizontal=TRUE, smallplot=c(.15, .5, .95, .97),
       legend.width=0.5, legend.shrink=0.5, legend.only=TRUE, axis.args=arg)

  # cols: rabies status
  cols <- c("white","#FDFCD6","#FDE4C2","#FAC090","#FA684F") # Absent, Absent-Vulnerable, Intermittent, Declining, Endemic
  episit <-  c("Absent", "Absent-Vulnerable", "Intermittent", "Declining", "Endemic")
  cols.trans <- adjustcolor(cols, alpha.f = 0.6)
  colleg <- cols.trans

  absent = which(mex@data$classification=="Absent")
  absentvul = which(mex@data$classification=="Absent-Vulnerable")
  intermittent = which(mex@data$classification=="Intermittent")
  declining = which(mex@data$classification=="Declining")
  endemic = which(mex@data$classification=="Endemic")

  plot(mex, add=T, col="transparent")
  if(length(absent)>0){plot(mex[absent,], add=T, col=cols.trans[1])}
  if(length(absentvul)>0){plot(mex[absentvul,], add=T, col=cols.trans[2])}
  if(length(intermittent)>0){plot(mex[intermittent,], add=T, col=cols.trans[3])}
  if(length(declining)>0){plot(mex[declining,], add=T, col=cols.trans[4])}
  if(length(endemic)>0){plot(mex[endemic,], add=T, col=cols.trans[5])}

  # boundaries
  plot(SA[which(SA@data$COUNTRY=="Mexico"),], add=T, border="black", lwd=2.5)
  plot(SA, add=T, border="black", lwd=1)

  legend(-113.8122,18.70355,
         legend=c(episit),
         col = c(rep("black",length(colleg))),
         pch=c(rep(22,length(colleg))),pt.cex=1,
         pt.bg = c(colleg),
         cex=0.7, bty="n")
  dev.off()
  print(i)
}


###---------------------------------------Brazil---------------------------------------###
## import shapefile
bra <- readOGR("data/ShapeFiles/America_Adm_1/Brazil.shp","Brazil")
proj4string(LAC); proj4string(bra)
proj4string(bra) <- proj4string(LAC)

# state
bra@data$ADM1_NAME <- language(bra@data$ADM1_NAME)
head(bra@data); dim(bra@data) #27 polygons

## import cases info
allstates <- read.csv("output_not_created/Brazil_classified_yearlyTW5yr.csv") # all years

## run for all by 5yrs periods
yrs <- c(2005:2015)
for(i in 1:length(yrs)){
  yr <- yrs[i]
  states <- data.frame(state=as.character(allstates$state),
                       phase=as.character(allstates[,colnames(allstates)==paste0("yr",yr)]))
  states$state <- gsub("[.]", " ", states$state)
  head(states)

  ## match
  SA <- world[which(world@data$CONTINENT=="South America" | world@data$CONTINENT=="North America"),]
  (rows <- match(states$state, bra@data$ADM1_NAME))
  states$phase <- as.character(states$phase)
  bra@data$classification <- states$phase
  unique(states$phase)

  ## raster
  # pop <- readAsciiGrid("~/Dropbox/PAHOsurveillance/Analysis/data/rasters/Human_pop_highres.asc")
  # pop.rast <- raster(pop, layer=1, values=T)

  pop <- aggregate(pop.rast, fact=6, fun=sum) #0.25 x 0.25 degrees per cell
  res(pop.rast); res(pop)

  ## PLOTTING
  pdf(paste0("figs/Brazil/Bra_rabies_classification", yr,".pdf"), width=8, height=6)
  plot(bra)
  plot(SA, add=T, col="lightgrey")
  options(scipen=5)

  colours<- colorRampPalette(c("white", "grey5"))(5)
  brk <- c(0,500,5000,50000,500000,7000000)
  arg <- list(at=c(550000,1900000,3700000,5500000,6800000),
              labels=c("<500","500-5000","5000-50000","50000-500000","<7000000"), cex.axis=0.45)
  plot(pop,add=T,col=colours,breaks=brk,legend=F)
  plot(pop, col=colours, horizontal=TRUE, smallplot=c(.15, .5, .95, .97),legend.width=0.5,
       legend.shrink=0.5, legend.only=TRUE,axis.args=arg)

  # cols: rabies status
  cols <- c("white","#FDFCD6","#FDE4C2","#FAC090","#FA684F") # Absent, Absent-Vulnerable, Intermittent, Declining, Endemic
  episit <-  c("Absent", "Absent-Vulnerable", "Intermittent", "Declining", "Endemic")
  cols.trans <- adjustcolor(cols, alpha.f = 0.6)
  colleg <- cols.trans

  absent = which(bra@data$classification=="Absent")
  absentvul = which(bra@data$classification=="Absent-Vulnerable")
  intermittent = which(bra@data$classification=="Intermittent")
  declining = which(bra@data$classification=="Declining")
  endemic = which(bra@data$classification=="Endemic")

  plot(bra, add=T, col="transparent")
  if(length(absent)>0){plot(bra[absent,], add=T, col=cols.trans[1])}
  if(length(absentvul)>0){plot(bra[absentvul,], add=T, col=cols.trans[2])}
  if(length(intermittent)>0){plot(bra[intermittent,], add=T, col=cols.trans[3])}
  if(length(declining)>0){plot(bra[declining,], add=T, col=cols.trans[4])}
  if(length(endemic)>0){plot(bra[endemic,], add=T, col=cols.trans[5])}

  # boundaries
  plot(SA[which(SA@data$COUNTRY=="Brazil"),],add=T,border="black",lwd=2.5)
  plot(SA,add=T,border="black",lwd=1)

  legend(-45.59187,-23.96342,
         legend=c(episit),
         col = c(rep("black",length(colleg))),
         pch=c(rep(22,length(colleg))),pt.cex=1,
         pt.bg = c(colleg),
         cex=0.7, bty="n")
  dev.off()
}


###------------------------ MEX monthly maps (no pop underlay) ----------------------------###

## import cases info
msc <- read.csv("output/Mexico_classified_monthly.csv") # monthly state classificiation
mts <- read.csv("output/Mexico_monthly_cases_state_WildRemoved.csv") # monthly state time series
MSC = msc[,-1]
mths <- colnames(msc)[-1]
mths = gsub("X", "", mths)

# Spatial polygon of states in South America
SA <- world[which(world@data$CONTINENT=="South America" | world@data$CONTINENT=="North America"),]
msc$state <- gsub("[.]", " ", msc$state) # match(msc$state, mex@data$ADM1_NAME) # GIS data matches
(rows <- match(msc$state, mex@data$ADM1_NAME))

# cols: rabies status
cols <- c("white","#FDFCD6","#FDE4C2","#FAC090","#FA684F") # Absent, Absent-Vulnerable, Intermittent, Declining, Endemic
episit <-  c("Absent", "Absent-Vulnerable", "Intermittent", "Declining", "Endemic")
cols.trans <- adjustcolor(cols, alpha.f = 0.6)
colleg <- cols.trans

## Create images for each year to combine with a slider
start = 61 # start of monthly timeseries from 2005-01-01 until 2015-12-012

pdf("figs/Mexico/Mex_monthly_classification_map.pdf", width=8, height=6)
for(i in start:ncol(MSC)){
  plot(mex)
  plot(SA, add=T, col="lightgrey")
  plot(mex, add=TRUE, col="white")
  options(scipen=5)

  plot(mex, add=T, col="transparent")
  if(length(absent)>0){plot(mex[which(MSC[,i]=="Absent"),], add=T, col=cols.trans[1])}
  if(length(absentvul)>0){plot(mex[which(MSC[,i]=="Absent-Vulnerable"),], add=T, col=cols.trans[2])}
  if(length(intermittent)>0){plot(mex[which(MSC[,i]=="Intermittent"),], add=T, col=cols.trans[3])}
  if(length(declining)>0){plot(mex[which(MSC[,i]=="Declining"),], add=T, col=cols.trans[4])}
  if(length(endemic)>0){plot(mex[which(MSC[,i]=="Endemic"),], add=T, col=cols.trans[5])}

  # boundaries
  plot(SA[which(SA@data$COUNTRY=="Mexico"),], add=T, border="black", lwd=2.5)
  plot(SA, add=T, border="black", lwd=1)

  legend(-113.8122,18.70355,
         legend=c(episit),
         col = c(rep("black",length(colleg))),
         pch=c(rep(22,length(colleg))),pt.cex=1,
         pt.bg = c(colleg),
         cex=0.7, bty="n")

  text(-113.8122, 19.5, mths[i])
  print(i)
}
dev.off()

###------------------------ MEX time series classified ----------------------------###

# Plot TIME SERIES
msc_y = substr(mths, 1, 4)
msc_m = substr(mths, 6, 7)
mts_y = substr(mts$date, 1, 4)
mts_m = substr(mts$date, 6, 7)
index = (which(mts_y == msc_y[1])[1]): nrow(mts); length(index); ncol(MSC) # make sure indices match

## classification colours
epicols <- data.frame(colours=c("white","#FDFCD6","#FDE4C2","#FAC090","#FA684F"),
                      class=c("Absent", "Absent-Vulnerable", "Intermittent", "Declining", "Endemic"))

# Start in 2000
pdf("figs/Mexico/Mex_monthly_classification_TS.pdf", width=8, height=6)
par(mfrow=c(4,4), mgp=c(1.5, 0.5, 0), mar=c(2,3,2,2))
for(i in 1:length(msc$state)){
  plot(index, mts[index,i], col="red", type="l",
       main = msc$state[i], ylab = "Cases", xlab = "",
       ylim=c(0,10), xlim=c(index[1], nrow(mts)),
       axes=FALSE)
  cols = as.character(epicols$colours[match(as.character(unlist((MSC[i,]))), epicols$class)])
  for(j in 1:length(cols)){
    lines(rep(j+start, 10), 0:9, col=cols[j])
  }
  lines(index, mts[index,i], col="black", type="l")
  axis(2, tck = -0.05, lwd=0.5)
  axis(1, at=index, labels=rep("", length(index)), lwd=0.5, tck=-0.01)
  axis(1, at=seq(start, start+(15*12), 12), labels=2000:2015, lwd=0.5, tck=-0.05)
}
dev.off()

index = (start+60): nrow(mts) # start in 2005
pdf("figs/Mexico/Mex_monthly_classification_TS2.pdf", width=8, height=6)
par(mfrow=c(4,4), mgp=c(1.5, 0.5, 0), mar=c(2,3,2,2))
for(i in 1:length(msc$state)){
  plot(index, mts[index,i], col="red", type="l",
       main = msc$state[i], ylab = "Cases", xlab = "",
       ylim=c(0,10), xlim=c(index[1],nrow(mts)),
       axes=FALSE)
  cols = as.character(epicols$colours[match(as.character(unlist((MSC[i,]))), epicols$class)])
  for(j in 1:length(cols)){
    lines(rep(j+start, 10), 0:9, col=cols[j])
  }
  lines(index, mts[index,i], col="black", type="l")
  axis(2, tck = -0.05, lwd=0.5)
  axis(1, at=index, labels=rep("", length(index)), lwd=0.5, tck=-0.01)
  axis(1, at=seq(start, start+(15*12), 12), labels=2000:2015, lwd=0.5, tck=-0.05)
}
dev.off()


###------------------------ BRA monthly maps (no pop underlay) ----------------------------###

## import cases info
bsc <- read.csv("output/Brazil_classified_monthly.csv") # monthly state classificiation
bts <- read.csv("output/Brazil_monthly_cases_state.csv") # monthly state time series
BSC = bsc[,-1]
mths = gsub("X", "", mths)
bsc$state <- gsub("[.]", " ", bsc$state) # match(msc$state, mex@data$ADM1_NAME) # GIS data matches

# cols: rabies status
cols <- c("white","#FDFCD6","#FDE4C2","#FAC090","#FA684F") # Absent, Absent-Vulnerable, Intermittent, Declining, Endemic
episit <-  c("Absent", "Absent-Vulnerable", "Intermittent", "Declining", "Endemic")
cols.trans <- adjustcolor(cols, alpha.f = 0.6)
colleg <- cols.trans

## Create images for each year to combine with a slider
pdf("figs/Brazil/Brazil_monthly_classification_map.pdf", width=8, height=6)

for(i in start:ncol(BSC)){
  plot(bra)
  plot(SA, add=T, col="lightgrey")
  plot(bra, add=TRUE, col="white")
  options(scipen=5)

  plot(bra, add=T, col="transparent")
  if(length(absent)>0){plot(bra[which(BSC[,i]=="Absent"),], add=T, col=cols.trans[1])}
  if(length(absentvul)>0){plot(bra[which(BSC[,i]=="Absent-Vulnerable"),], add=T, col=cols.trans[2])}
  if(length(intermittent)>0){plot(bra[which(BSC[,i]=="Intermittent"),], add=T, col=cols.trans[3])}
  if(length(declining)>0){plot(bra[which(BSC[,i]=="Declining"),], add=T, col=cols.trans[4])}
  if(length(endemic)>0){plot(bra[which(BSC[,i]=="Endemic"),], add=T, col=cols.trans[5])}

  # boundaries
  plot(SA[which(SA@data$COUNTRY=="Brazil"),], add=T, border="black", lwd=2.5)
  plot(SA, add=T, border="black", lwd=1)

  legend(-45.59187,-23.96342,
         legend=c(episit),
         col = c(rep("black",length(colleg))),
         pch=c(rep(22,length(colleg))),pt.cex=1,
         pt.bg = c(colleg),
         cex=0.7, bty="n")

  # NEED TO FIND BETTER POSITION FOR LEGEND
  text(-45.59187,-22, mths[i])
  print(i)
}
dev.off()

###------------------------ BRA time series classified ----------------------------###
# Plot TIME SERIES
index = (which(mts_y == msc_y[1])[1]): nrow(mts); length(index); ncol(MSC) # make sure indices match

# Start in 2000
pdf("figs/Brazil/Bra_monthly_classification_TS.pdf", width=8, height=6)
par(mfrow=c(4,4), mgp=c(1.5, 0.5, 0), mar=c(2,3,2,2))
for(i in 1:length(bsc$state)){
  plot(index, bts[index,i], col="red", type="l",
       main = bsc$state[i], ylab = "Cases", xlab = "",
       ylim=c(0,10), xlim=c(index[1], nrow(bts)),
       axes=FALSE)
  cols = as.character(epicols$colours[match(as.character(unlist((BSC[i,]))), epicols$class)])
  for(j in 1:length(cols)){
    lines(rep(j+start, 10), 0:9, col=cols[j])
  }
  lines(index, bts[index,i], col="black", type="l")
  axis(2, tck = -0.05, lwd=0.5)
  axis(1, at=index, labels=rep("", length(index)), lwd=0.5, tck=-0.01)
  axis(1, at=seq(start, start+(15*12), 12), labels=2000:2015, lwd=0.5, tck=-0.05)
}
dev.off()

index = (start+60): nrow(mts) # start in 2005
pdf("figs/Brazil/Bra_monthly_classification_TS2.pdf", width=8, height=6)
par(mfrow=c(4,4), mgp=c(1.5, 0.5, 0), mar=c(2,3,2,2))
for(i in 1:length(bsc$state)){
  plot(index, bts[index,i], col="red", type="l",
       main = bsc$state[i], ylab = "Cases", xlab = "",
       ylim=c(0,10), xlim=c(index[1],nrow(mts)),
       axes=FALSE)
  cols = as.character(epicols$colours[match(as.character(unlist((BSC[i,]))), epicols$class)])
  for(j in 1:length(cols)){
    lines(rep(j+start, 10), 0:9, col=cols[j])
  }
  lines(index, bts[index,i], col="black", type="l")
  axis(2, tck = -0.05, lwd=0.5)
  axis(1, at=index, labels=rep("", length(index)), lwd=0.5, tck=-0.01)
  axis(1, at=seq(start, start+(15*12), 12), labels=2000:2015, lwd=0.5, tck=-0.05)
}
dev.off()

