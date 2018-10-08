library("shiny")
library("leaflet") # for the nice maps
library("rgdal") # for readOGR
library("htmltools")
library("dygraphs") #for time series
library("dplyr") #data maniplations
library("DT") #for classification key table
library("shinyjs") # for some extra events
library("magrittr")
library("tidyr")
library("lubridate")

source("utility.R", encoding = "UTF-8")

##################################################################################
## creation of the base map which will have hover info for status and be clickable
##################################################################################

## get relevant shape files

source("www/translate.R") # required for spelling correction of names with foreign letters

world = readOGR("www/data/ShapeFiles/countries_shp/countries.shp", "countries")
LAC = readOGR("www/data/ShapeFiles/America_Adm_1/adm1_amro_lat_long.shp","adm1_amro_lat_long")
## keep only the world country polygons for latin america
world = world[world$NAME %in% LAC$CNTRY_NAME,]
# remove canada, not sure how it's in there
world = world[world$NAME != "Canada",]


## countries for which we have data
mex <- readOGR("www/data/ShapeFiles/America_Adm_1/Mexico.shp","Mexico")
# correct spellings of regions
mex@data$ADM1_NAME <- language(mex@data$ADM1_NAME)
proj4string(mex) <- proj4string(LAC)
bra <- readOGR("www/data/ShapeFiles/America_Adm_1/Brazil.shp","Brazil")
proj4string(bra) <- proj4string(LAC)
# state
bra@data$ADM1_NAME <- language(bra@data$ADM1_NAME)


dat = rbind(mex,bra)
rm(mex,bra)

## leaf1 will give you the country selector, this will then generate a new map
## for regions with the chosen country
### TODO: can we simplify shapefiles to help load times
leaf1 = leaflet() %>%
  addTiles(options = tileOptions(minZoom = 2, maxZoom = 2)) %>%
  addPolygons(data = world[! world$NAME %in% unique(dat$CNTRY_NAME),],
              label = ~htmlEscape(NAME),  # countries polygons, want no fill, basic hover info
              fillOpacity = 0, weight = 2, color = "grey",
              layerId = ~NAME) %>%
  addPolygons(data = world[world$NAME %in% unique(dat$CNTRY_NAME),],
              label = ~htmlEscape(NAME), layerId = ~NAME,
              weight = 3, color = "black",fillOpacity = 0, opacity = 1) %>% # clickable countries polygons, want fill, basic hover info
              setMaxBounds(-26.71875,35.17381,-182.8125,-71.18775) # need to revise the box slightly
country_names = unique(dat$CNTRY_NAME)

poly_list = list()
for(nam in country_names){
  poly_list[[nam]] = dat[dat$CNTRY_NAME == nam,]
}

##################################################################################
## time series data sets list ####################################################
##################################################################################

tsdatalist = list()
for(nam in country_names){
  tsdatalist[[nam]] = readRDS(paste0(nam,".RData"))
}

## time series by year data for default time series map
get_all_yearly = function(dat = tsdatalist){
  df = do.call(rbind, dat)
  df %<>% group_by(Year = year(Date)) %>% summarise(Value = sum(Value))
  rnam = paste0(df$Year, "-01-01")
  df = data.frame("All" = df$Value)
  rownames(df) = rnam
  df
}

default_dygraph = function(){
  df = get_all_yearly()
  colnames(df) = c("Latin America")
  # filter out data before 2005
  id = which(as.Date(rownames(df)) >= "2005-01-01")
  dygraph(df[id,,drop = FALSE], main = paste0("Casos anuales de rabia en América Latina")) %>%
    dyOptions(drawGrid = FALSE, strokeWidth = 2, colors = c("grey","black")) %>%
    # the below specifies a javacsript function for grabbing just the year from the date
    DYRANGE %>%
    dyAxis("y", valueRange = c(0,NULL))
}

get_country_yearly = function(country, dat = tsdatalist){
  df1 = dat[[country]]
  df2 = do.call(rbind, dat)
  df1 %<>% group_by(Year = year(Date)) %>% summarise("Country" = sum(Value))
  df2 %<>% group_by(Year = year(Date)) %>% summarise("All" = sum(Value))
  df = full_join(df2,df1)
  rnam = paste0(df$Year, "-01-01")
  df = df %>% select(-Year) %>% as.data.frame
  rownames(df) = rnam
  colnames(df) = c("Latin America", country)
  df
}

get_country_monthly = function(country, dat = tsdatalist){
  df = dat[[country]]
  df %<>% group_by(Date) %>% summarise(Value = sum(Value))
  rnam = df$Date
  df = data.frame(Cases = df$Value)
  rownames(df) = rnam
  df
}

country_default_dygraph = function(country){
  df = get_country_monthly(country)
  id = which(as.Date(rownames(df)) >= "2005-01-01")
  dygraph(df[id,,drop = FALSE], main = paste0("Casos mensuales de rabia en ", country)) %>%
    dyOptions(drawGrid = FALSE, strokeWidth = 2, colors = c("grey","black")) %>%
    # the below specifies a javacsript function for grabbing just the year from the date
    DYRANGE
}

get_region_monthly = function(country,region,dat = tsdatalist){
  df1 = dat[[country]]
  df2 = df1 %>% filter(State == region)
  rnam = df2$Date
  df1 %<>% group_by(Year = year(Date), Month = month(Date)) %>% summarise("Country" = sum(Value))
  df2 %<>% group_by(Year = year(Date), Month = month(Date)) %>% summarise("Region" = sum(Value))
  df = full_join(df1,df2)
  df %<>% ungroup %>% select(-Year,-Month) %>% as.data.frame
  rownames(df) = rnam
  #edge case where mexico state is in mexico country
  if(country == region) region = paste0(region," ")
  colnames(df) = c(country,region)
  df
}

## list of maps for countries for which we have data,
## doing it this way should make it extendable to having new data

colours = setNames(
  c("white","#FDFCD6","#FDE4C2","#FAC090","#FA684F"),
  c("Absent", "Absent-Vulnerable", "Intermittent", "Declining", "Endemic")
)

colours_df = data.frame("Classification" = factor(names(colours), levels = names(colours)))#,
                                                  # labels = rev(c("ENDEMIC: rabies detected over 2+ consecutive months during past 2 years. No significant increase in the frequency of disease-free months over the past 5 years.",
                                                  #            "DECLINING: >1 case detected in the past 2 years, but an increasing frequency of months with zero cases over the past five years. ",
                                                  #            "INTERMITTENT DETECTION: Cases during past 2 years but not over consecutive months. No trend in the proportion of months with cases during the past 5 years.",
                                                  #            "ABSENT-VULNERABLE: Rabies not detected in past 2 years, but adjacent areas are Endemic or Declining, therefore vulnerable to incursions; or only 1 month with cases during past 2 years, but zero cases detected prior to that month for 2+ years i.e. recent incursion without further spread. ",
                                                  #            "ABSENT: No cases detected during the last 2 years and minimal risk of incursion "))))

## colour palette function for map polygons
colour_pal = colorFactor(colours, names(colours),names(colours))
#   function(region, lookup, colours = setNames(
#   c("white","#FDFCD6","#FDE4C2","#FAC090","#FA684F"),
#   c("Absent", "Absent-Vulnerable", "Intermittent", "Declining", "Endemic")
# )){
#
# }

leaflist = list()
for(nam in country_names){
  tempdat = poly_list[[nam]]
  ## add final classification
  x =  tsdatalist[[nam]] %>% group_by(State) %>% filter(min_rank(desc(Date)) == 1)
  x$State = gsub("[.]", " ",x$State)
  tempdat@data = data.frame(tempdat@data, Classification =
                              x$Classification[match(tempdat@data$ADM1_NAME,x$State)])
  leaflist[[nam]] = leaflet() %>%
    addTiles() %>%
    addPolygons(data = tempdat, label = ~ADM1_NAME, layerId = ~ADM1_NAME,
                color = "black", fillColor = ~colour_pal(Classification), fillOpacity = 0.6,
                weight = 2)
}

###################################################################################
## utility functions for time series plots ########################################
###################################################################################

find_shading = function(x, colours = setNames(
  c("white","#FDFCD6","#FDE4C2","#FAC090","#FA684F"),
  c("Absent", "Absent-Vulnerable", "Intermittent", "Declining", "Endemic")
)){
  # find when the following time is different to current classification
  froms = c(Position(function(x) !is.na(x), x$Classification),
            which(x$Classification[-nrow(x)] != x$Classification[-1])+1
  )
  # tos must be the next from along, c the end of series
  tos = c(froms[-1],nrow(x))
  df = data.frame(from = x$Date[froms], to = x$Date[tos], color = colours[match(x$Classification[froms],names(colours))])
  df
}


tsplot = function(full, region){
  x = filter(full,State == region & Date >= as.Date("2005-01-01", format= "%Y-%m-%d"))
  fillinfo = find_shading(x)
  tsvals = data.frame("Casos mensuales" = x$Value, check.names = FALSE)
  rownames(tsvals) = x$Date
  d = dygraph(tsvals, main = region)
  if(nrow(fillinfo) > 0){
    for(i in 1:nrow(fillinfo)){
      d %<>% dyShading(fillinfo$from[i],fillinfo$to[i], color = fillinfo$color[i])
    }
  }
  d %>%
    #dyRangeSelector() %>%
    dyOptions(drawGrid = FALSE, strokeWidth = 2, colors = "black") %>%
    dyAxis("y", valueRange = c(0, max(20,tsvals$Value))) %>%
    # the below specifies a javacsript function for grabbing just the year from the date
    DYRANGE
}


##### data frame for extra country info
round_percantage = function(x){
  paste0(round(as.numeric(strsplit(x,"%")[[1]][1])),"%")
}

nearest_thousand_formatted = function(x){
  x = round(as.numeric(x) / 1000) * 1000
  format(x,big.mark = ",")
}

country_info_helper = function(){
  cinfo = read.csv("www/data/LACcountries.csv", stringsAsFactors = FALSE)
  return(function(country){ # returns html string for table
    ## in the data file there was a problem with Venezuela, caused because the
    # country name in the shapefile data is different from that in the recorded data
    # we could remove this by changing data file in the long run, but for now:
    if(tolower(country) == "venezuela"){
      country = "Venezuela (Bolivarian Republic of)"
    }

    if(! country %in% cinfo$Country){
      tabdat = data.frame( "No information available")
      tabdat = paste0("<div align='center'></div><table style='width:100%'>",
                      paste0("<tr>",tabdat[1,1],"</tr>", collapse = ""),
                      "</table>")
    }else{
      cinfo = cinfo %>% filter(Country == country)

      tabdat = data.frame(c("País",
                            "Índice de Desarrollo Humano",
                            "Población humana",
                            "Población de perros",
                            "Cobertura de vacunación en perros"
      ),
      as.character(cinfo[,c(3,7,6,14,9)]), stringsAsFactors = FALSE)
      tabdat[5,2] = round_percantage(tabdat[5,2])
      tabdat[4,2] = nearest_thousand_formatted(tabdat[4,2])
      colnames(tabdat) = NULL
      tabdat = as.matrix(tabdat)
      tabdat[,1] = paste0("<td><strong>", tabdat[,1], "</strong></td>")
      tabdat[,2] = paste0("<td>", tabdat[,2], "</td>")
      # tabdat = paste0("<div align='center'><h2>Country Information </h2></div><table style='width:100%'>",
      #                 paste0("<tr>",apply(tabdat, 1, paste0, collapse = ""),"</tr>", collapse = ""),
      #                 "</table>")
      tabdat = paste0("<div align='center'></div><table style='width:100%'>",
                      paste0("<tr>",apply(tabdat, 1, paste0, collapse = ""),"</tr>", collapse = ""),
                      "</table>")
      tabdat
    }


  })
}

country_info = country_info_helper()


########################## some ui defaults ################################


## debouncing function ##
## this is a sort of hack for allowing delayed input registering of shiny bindings
## essentially using this for allowing us to move the date window of the dygraph
## but without triggering the rerendering of the map with every date change.
## This drastically improves the performance at the cost of making the transition slightly less smooth
## some tuning of the delay may be required

debounce = function(expr, millis, env = parent.frame(), quoted = FALSE, domain = getDefaultReactiveDomain()){
  force(millis) # force the time argument to be evaluated, will trigger stop() if no time provided
  f = exprToFunction(expr, env, quoted)
  label = sprintf("debounce%s", paste(deparse(body(f)), collapse = "\n"))
  v = reactiveValues(
    trigger = NULL,
    when = NULL # deadline for timer to fire
  )
  observeEvent(f(),{
    v$when = Sys.time() + millis/1000
  })

  # observation for the timer, rest til when elapses, then touch trigger
  observe({
    if(is.null(v$when)) return()
    now = Sys.time()
    if(now >= v$when){
      v$trigger = runif(1) ## random number so that can use returned event reactive multiple times
      v$when = NULL
    }else{
      invalidateLater((v$when - now)*1000, domain)
    }
  })

  ## event reactive return
  eventReactive(v$trigger, {
    f()
  }, ignoreNULL = FALSE)
}



# html code for the classification table,
# no longer used, image used instead
CLASS_TABLE = "<br/><br/> <table border= '1px splod black'>
<tr>
<th colspan = 5>Classification Key</th>
</tr>
<tr>
<td> Absent </td>
<td> No cases detected during the last 2 years and minimal risk of incursion  </td>
</tr>
<tr>
<td bgcolor = '#FDFCD6'> Absent-Vulnerable </td>
<td> Rabies not detected in past 2 years, but adjacent areas are Endemic or Declining, therefore vulnerable to incursions; or only 1 month with cases during past 2 years, but zero cases detected prior to that month for 2+ years i.e. recent incursion without further spread. </td>
</tr>
<tr>
<td bgcolor = '#FDE4C2'> Intermittent </td>
<td> Cases detected during past 2 years but not over consecutive months. No trend in the proportion of months with cases during the past 5 years. </td>
</tr>
<tr>
<td bgcolor = '#FAC090'> Declining </td>
<td> >1 case detected in the past 2 years, but an increasing frequency of months with zero cases over the past five years.</td>
</tr>
<tr>
<td bgcolor = '#FA684F'> Endemic </td>
<td> rabies detected over 2+ consecutive months during past 2 years. No significant increase in the frequency of disease-free months over the past 5 years.  </td>
</tr>
</table>"

# constant for keeping timeseries axes the same
DYRANGE = function(x){
  # the below specifies a javacsript function for grabbing just the year from the date
  x %>% dyAxis("x", rangePad = 10,
         axisLabelFormatter = "(function(d,gran){
         return d.getFullYear();
         })", # below is a javascript function for specifying the actual tick marks used
         ticker = "(function(a, b, pixels, opts, dygraph, vals) {
         var chosen = 19; // corresponds to annual tick marks

         if (chosen >= 0) {
         return Dygraph.getDateAxis(a, b, chosen, opts, dygraph);
         } else {
         // this can happen if self.width_ is zero.
         return [];
         }
         })") %>% dyLegend(width = 450)
}
