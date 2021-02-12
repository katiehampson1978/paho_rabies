## SIRVERA DATA: CASE PRESENCE ROLLING WINDOW FUNCTION
## df of cases presence/absence (1/0) using rolling window method - assume case is latent for 1 month
RollingWind = function(states, mths){
  presence <- data.frame(matrix(NA, nrow=length(1:mths), ncol= ncol(states)))
  colnames(presence) <- c(colnames(states))
  presence$date <- 1:(mths)

  presence.rw <- data.frame(matrix(NA, nrow=length(1:(mths-1)), ncol= ncol(states)))
  colnames(presence.rw) <- c(colnames(states))
  presence.rw$date <- 1:(mths-1)

  for (i in 1:(ncol(states)-1)){
    state <- states[,i]
    presence[,i] <- as.numeric(state>0) #assume last one is same as previous
    presence.rw[,i] <- as.numeric((presence[,i][-1] + presence[,i][-mths])>0)
  }
  list(presence=presence, presence.rw=presence.rw)
}
