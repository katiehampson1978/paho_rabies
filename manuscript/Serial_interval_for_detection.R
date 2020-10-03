# Simulation to check Serial Interval
rm(list=ls())

# Serial interval between rabies cases 
# Gamma parameterisation
SIshape <- 1.440691
SIscale <- 21.00193  

# select a day of case detection
# choose secondary cases
# Work out what proportion occur the same month (vs the following month)
prop_SI = function(n, threshold = 31){
  detect = runif(n, 1, 31)
  SI = rgamma(n, shape = SIshape, scale = SIscale)
  p = length(which( (detect + SI) < threshold ))/n
  print(p)
}
prop_SI(1000)


# Lognormal parameterization
SIml <- 3.002046 
SIsdlog <- 0.8620819

prop_SI_lnorm = function(n, threshold = 31){
  detect = runif(n, 1, 31)
  SI = rlnorm(n, meanlog = SIml, sdlog = SIsdlog)
  p = length(which( (detect + SI) < threshold ))/n
  print(p)
}
prop_SI_lnorm(1000)

# Conclude that less than 35% of secondary cases occur the same month 
# i.e. the majority should occur the following month

