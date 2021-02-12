# Function to take the SIRVERA data for a country and generate timeseries for each admin unit
states_ts = function(dates, states, data){

  cases <- matrix(0, ncol = length(states), nrow = length(dates))

  for(i in 1:length(states)){
    state_data <- data[which(as.character(data$UnidMaior) == states[i]),]
    for(j in 1:length(dates)){
      index <- which(state_data$date == dates[j])
      cases[j,i] <- sum(state_data$TotalCasos[index], na.rm=TRUE)
    }
  }

  # Format dataframe
  cases = data.frame(cases)
  names(cases) <- states
  cases$date = dates
  cases
}

# ## sort out full dates (months included)
# country$Mes[which(country$Mes=="0")] <- 1
# country$date <- as.POSIXct(as.yearmon(paste(country$Ano,country$Mes, sep="-")))
# country$date <- strftime(strptime(country$date, format="%Y-%m-%d"),"%Y-%m")
#
# ## sum all cases for each month and year
# cases <- vector(mode = "numeric", length(dates))
# for(i in 1:length(dates)){
#   index <- which(country$date == dates[i])
#   cases[i] <- sum(country$TotalCasos[index], na.rm=TRUE)
# }
#
# monthly.cases <- cbind(dates, cases)
# if(sum(country$TotalCasos)!=sum(cases)){
#   stop("cases don't match")
# }

