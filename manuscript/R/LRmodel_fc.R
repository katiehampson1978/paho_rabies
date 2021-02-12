### SIRVERA DATA: LOGISTIC REGRESSION FUNCTIONS (cases, incursions) ###
library(lme4)
library(nlme)
library(MASS)
library(pscl)
library(car)

### CASES ###
LRmodel  <-  function(data){
  ### FC to generate a univariate logistic regression model and report stats
  ### input: counts = presence/absence time series (response variable),
  ###         months = time in months (explanatory variable)
  ### output: chi-square probability, odds ratio, goodness of fit, p-value of variable,
  ###         confidence interval of variable, pseudo R-squared, aic, fitted values

  model  <-  glm(counts~months, data=data, family=binomial(link="logit"), control = list(maxit = 100))

  ## how well model fits compared to null model
  chi <- model$null.deviance - model$deviance ## check whether this is better than a null model: if <0.05
  df <- model$df.null - model$df.residual
  chsq.prob <- 1 - pchisq(chi, df) # chi-square probability
  goodfit <- pchisq(model$deviance, df=model$df.residual, lower.tail=FALSE) # goodness of fit

  ## coefficients
  odds <- exp(model$coefficients)[2] # determine trend - if > 1 increasing
  # every one unit increase in pred var, the odds of getting 1 in res var increases by a factor of odds no.

  pval <- summary(model)$coefficients[2,4] # p-value
  pseudoR2 <- pR2(model)["McFadden"]  # pseudo R-squared: 'McFadden' btw 0 (no predictive power) and 1

  fitted <- model$fitted # fitted values

  m <- list(goodfit=goodfit, chsq.prob = chsq.prob, odds = odds, pval = pval, fitted = fitted)
}

# ## alternative way how to compare a model with its null model
# mymonths <- 1:131
# model  <-  glm(presence.rw[,7]~mymonths, family=binomial(link="logit"), control = list(maxit = 100))
# modelnul <- glm(presence.rw[,7]~1, family=binomial(link="logit"), control = list(maxit = 100))
# logLik(model); logLik(modelnul)
# AIC(model, modelnul)




### INCURSIONS ###
LRincursions  <-  function(mydata, method="LR", computeCI=F){
  ## FC to generate a univariate regression model* and report stats
  ### method*: LR (simple linear regressio), RE (linear regression with random effect),
  ###           TA (linear regression with random effect and time-autocorrelation)

  ### input: incursion = presence/absence data of incursions (response variable),
  ###         variable = of choice (explanatory variable),
  ###         variable = state (explanatory variable),
  ###         variable = period (time autocorrelation)
  ### output: p-value(s) of variable(s), fitted values,
  ###         LR => chi-square probability, odds ratio, conf int of variable, pseudo R-squared
  ###         RE => chi-square probability, aic, odds ratio, pvalue, max gradient
  ###         TA => phi (coefficient) and pvalue

  ## simple logistic regression
  if(method =="LR"){
    model <- glm(incursion ~ variable, data=mydata, family=binomial(link="logit"), control = list(maxit = 100))

    ## how well model fits compared to null model
    chi <- model$null.deviance - model$deviance ## check whether this is better than a null model: if <0.05
    df <- model$df.null - model$df.residual
    chsq.prob <- 1 - pchisq(chi, df) # chi-square probability

    goodfit <- pchisq(model$deviance, df=model$df.residual, lower.tail=FALSE) # goodnes of fit
    aic <- AIC(model)

    ## coefficients
    odds <- exp(model$coefficients)[2] # determine trend - if > 1 increasing
    if(computeCI==T){
      CI <- confint(model) # confidence intervals
      CIlower <- exp(CI[2,1]); CIupper <- exp(CI[2,2])
    }
    pval <- summary(model)$coefficients[2,4] # p-value
    pseudoR2 <- pR2(model)["McFadden"]  # pseudo R-squared: 'McFadden' btw 0 (no predictive power) and 1

    ## fitted values
    fitted <- model$fitted # fitted values

    ## Wald-Test
    #library(survey); regTermTest(model, "variable") # if p-value < 0.05 the var is significant

    m <- list(chsq.prob = chsq.prob, goodfit=goodfit, aic=aic,odds = odds,
              pval = pval, pseudoR2 = pseudoR2,  fitted = fitted)
  }

  ## logistic regression with random effect
  if(method=="RE"){
    model  <-  glmer(incursion ~ variable + (1|state), data=mydata,
                     family = binomial(link="logit"),
                     control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
                     nAGQ = 100)
#     model  <-  glmer(incursion ~ I(variable/100) + (1|state), data=mydata,
#                      family = binomial(link="logit")) #try to rescale like this - effect of extra 100 cases the probability of getting an incursion

    relgrad <- with(model@optinfo$derivs, solve(Hessian,gradient))
    maxgrad <- max(abs(relgrad)) # gradient of likelihood profile; if <0.001 then it is ok

    ## how well model fits compared to null model
    chsq.prob <- car::Anova(model, type = 3)[2,3] ## Wald test
    aic <- AIC(model)

    ## coefficients
    pval <- summary(model)$coefficients[2,4] # p-value
    odds <- exp(summary(model)$coefficients[2,1]) # determine trend - if > 1 increasing
    if(computeCI==T){
      CI <- confint(model) # confidence intervals
      CIlower <- exp(CI[3,1]); CIupper <- exp(CI[3,2])
      m <- list(chsq.prob = chsq.prob, aic=aic, odds = odds, pval = pval,
                CIlower=CIlower, CIupper=CIupper, maxgrad=maxgrad)
      }

    ## fitted values
    fitted <- fitted(model) # fitted values

    m <- list(chsq.prob = chsq.prob, aic=aic, odds = odds, pval = pval, maxgrad=maxgrad)
    }

  ## logistic regression with random effect and time autocorrelation
  if(method=="TA"){
    model <- glmmPQL(incursion ~ variable, random = ~ 1|state,
                    correlation = corAR1(form = ~period|state),
                    family=binomial, data=mydata)

    pval <- summary(model)$tTable[2,5] # p-value
    phi <- coef(model$modelStruct$corStruct,unconstrained=FALSE) #Phi=<0 no time-autocorrelation
    fitted <- fitted(model)

    m <- list(pval = pval, phi=phi)
  }

  return(m)
}

### Another way how to fit both RE and TA (but no way how to specify family?)
# model <- lme(incursion ~ variable,
#              data=mydata, method="REML",
#              random = ~ 1|state,
#              correlation=corCAR1(form = ~period|state),
#              control=list(maxIter=10000, niterEM=10000))
# lmeObject, resid, coef, fitted

