### SIRVERA DATA: Mexico & Brazil yearly classification from 2000 to curent date
## The script takes monthly case timeseries data for Mexico and Brazil,
## processes the data for classification alghorithm monthly from the current date to Jan 2000 via:
## a) case transformation to binary data expanded by using the rolling window method,
## b) output statistics from logistic regression models and patterns detected from case incidence data

rm(list=ls())
library(zoo)
source("R/gap_fc.R")
source("R/RollingWind_fc.R")
source("R/LRmodel_fc.R")
source("R/RemoveWildlifeVar.R")

countries <- c("Mexico", "Brazil")

for (l in 1:length(countries)){
  cn <- countries[l] #country of interest
  states <- removeWild(cn=cn)

  # HARD CODE IMPORTANT VALUES (so not recalculating throughout functions etc)
  study.mths <- length(seq(as.Date("2000-01-01"), as.Date(as.yearmon(rev(states$date)[1])), by="month"))
  mths  <-  nrow(states)
  state_gaps <- list()
  state_names <- colnames(states)[1:(ncol(states)-1)]
  n  <-  length(state_names)
  yrs  <-  length(unique(strftime(strptime(as.Date(as.yearmon(states$date)), format="%Y-%m-%d"),"%Y")))
  Y2  <-  24 # period of two years

  states.fulldata <- states

  ### CASE DETECTION - Logistic Regression
  ## case presence: rolling window method
  RWoutput <- RollingWind(states, mths)
  presence <- RWoutput$presence
  presence.rw <- RWoutput$presence.rw
  presence.rw.fulldata <- presence.rw

  ## working back from current date to the last year period
  ms <- mths:1
  ms.rw <- ms; ms.rw[1] <- ms[1]-1

  ## initiate: output stored in a list of df by 6 month periods
  LRcoefsList <- list() ## store LR coeffs for truncated periods (length(ms)=13)
  length(LRcoefsList) <- study.mths

  ## prep data for class algorithm (LR)
  for (r in 1:study.mths){ #for each month in r
    end.period <- ms[r]-60+1
    states <- states.fulldata[end.period:ms[r],]
    presence.rw <- presence.rw.fulldata[end.period:ms.rw[r],]

    ## set up the frame to store calculations in
    LRcoefs <- data.frame(state = state_names,
                          chsq.prob = rep(NA, n),
                          odds = rep(NA, n),
                          pval = rep(NA, n),
                          cases_yearly=numeric(n),
                          max_cases_monthly=numeric(n),
                          cases_2years=numeric(n),
                          max_gaplength=numeric(n),
                          mean_gaplength=numeric(n),
                          t_since_last_case=numeric(n),
                          t_nocase_prior_lastcase=numeric(n),
                          consecutive_cases_2yr=rep(NA, n))

    ### INTERVALS BETWEEN DETECTED CASES: gaps for each state individually
    for(d in 1:n){
      state_cases <- states[,d]
      gaps <- gap_fc(state_cases, l=length(state_cases))
      state_gaps[[d]] <- gaps
    }
    names(state_gaps) <- state_names

    for (i in 1:n){ #for each state in n
      LRcoefs$cases_yearly[i] <- sum(states[,i])/(nrow(states)/12)
      LRcoefs$max_cases_monthly[i] <- max(states[,i])
      LRcoefs$cases_2years[i] <- ifelse(length(states[,i])>24,sum(states[(nrow(states)-Y2):nrow(states),i]), sum(states[,i]))
      LRcoefs$max_gaplength[i] <- max(state_gaps[[i]]) ## max gap length
      LRcoefs$mean_gaplength[i] <- mean(state_gaps[[i]]) ## mean gap length
      LRcoefs$t_since_last_case[i] <- state_gaps[[i]][length(state_gaps[[i]])] ## time since last case
      LRcoefs$t_nocase_prior_lastcase[i] <- ifelse(length(state_gaps[[i]])>1,
                                                   state_gaps[[i]][(length(state_gaps[[i]])-1)],0) ## with no case prior to last case

      ## are there cases present over consecutive months in last 2 yrs
      end <- length(states[,i])
      bincases <- ifelse(states[(end-Y2):end,i]>0,1,0)
      cons <- rle(bincases)
      ind <- which(cons$length[cons$values == 1]>1)
      LRcoefs$consecutive_cases_2yr[i] <- ifelse(length(ind)>0,"Y","N")

      state <- data.frame(counts=presence.rw[,i], months=1:nrow(presence.rw))

      ## only fit if at least 2 cases and 1 zeros!
      if(sum(state$counts)>1 && sum(state$counts)!=length(state$counts)){
        m = LRmodel(data <- state) ## LR model
        LRcoefs$chsq.prob[i] <- m$chsq.prob ## check whether better than a null model
        LRcoefs$odds[i] <- m$odds  ## check whether increasing or decreasing trend
        LRcoefs$pval[i] <- m$pval
        rm(m)
      }
    }
    LRcoefsList[[r]] <- LRcoefs
    print(r)
  }

  ## loop tru the LRcoefsList and save output for each month separately
  month.names <- rev(seq(as.Date("2000-01-01"), as.Date(as.yearmon(rev(states.fulldata$date)[1])), by="month"))
  for(j in 1:length(LRcoefsList)){
    this.list <- data.frame(LRcoefsList[[j]])
    this.month <- month.names[j]
    write.csv(this.list, paste0("output/", cn, "_MonthlyOutputFor_", this.month, ".csv"), row.names = FALSE)
  }
}


