## Takes pre-formatted variant data and removes from the SIRVERA case time series
removeWild = function(cn){
  # import data
  fstates <- paste0("output/",cn, "_monthly_cases_state.csv")
  states <- read.csv(fstates) # monthly detected cases by state

  if(cn=="Mexico"){ ## remove wildlife variants if available
    wildvars <- read.csv("data/Mex_WildVariants.csv") # wildlife variants
    names <- gsub("[.]", " ", colnames(states)[1:(ncol(states)-1)])
    for( i in 1:nrow(wildvars)){
      cind <- which(names==wildvars$State[i])
      rind <- which(states$date==as.character(wildvars$Date[i]))
      states[rind, cind] <-  states[rind, cind]-1
    }
    fstates <- paste0("output/",cn, "_monthly_cases_state_WildRemoved.csv")
    write.csv(states, fstates, row.names = F) # monthly detected cases by state
  }
  
  if(cn=="Brazil"){ ## remove wildlife variants if available
    wildvars <- read.csv("data/Bra_WildVariants.csv") # wildlife variants
    names <- gsub("[.]", " ", colnames(states)[1:(ncol(states)-1)])
    for( i in 1:nrow(wildvars)){
      cind <- which(names==wildvars$State[i])
      rind <- which(states$date==as.character(wildvars$Date[i]))
      states[rind, cind] <-  states[rind, cind]-1
    }
    fstates <- paste0("output/",cn, "_monthly_cases_state_WildRemoved.csv")
    write.csv(states, fstates, row.names = F) # monthly detected cases by state
  }
  
  return(states)
}
