## master R script for creating everything
# prior to this all the output including GB of images were being synced to dropbox. 
# This is horrifically inefficient and should not be the case
# it is also good practice to have an install script which makes sure whoever has the source can actually 
# run it without having to worry about package management
# the directory tree of the output being made should also be created since it should not be synced

### creating the file structure
dirs = c(
  "figs",
  "figs/Brazil",
  "figs/Mexico",
  "output"
)

for(d in dirs){
  if(!dir.exists(d)) dir.create(d)
}

### package management
cran_packs = c(
  "maptools",
  "zoo",
  "lubridate",
  "spdep",
  "raster",
  "rgdal",
  "rgeos",
  "lme4",
  "nlme",
  "MASS",
  "pscl",
  "car",
  "survey",
  "reshape2",
  "magrittr",
  "dplyr"
)


for(p in cran_packs){
  if(!require(p,character.only = TRUE)){
    install.packages(p)
  }
}


### run all the code to generate all of the outputs
source("1.CreateStateLevelMonthlyTS.R")
source("2.CountryMonthlyOutput.R")
source("3.ClassificationAlgorithm.R")
source("4.ClassificationMaps.R")

## create the nice time series data files
source("create_ts_data.R")
