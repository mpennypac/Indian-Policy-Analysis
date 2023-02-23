

## read American Indian unemployment data in
AI_unemp = read.csv('monthly_AI_AN_unemployment.xlsx - BLS Data Series.csv')
head(AI_unemp)

## read total US unemployment data in
tot_unemp = read.csv('UNRATENSA.csv')
head(tot_unemp)

## What I'd really like to know is how American Indian unemployment rate /alone/
## changes with the implementation of the policy. Would it make sense to just
## subtract total unemployment from American Indian unemployment (i.e. get
## "excess" American Indian unemployment)? It's definitely true that
## American Indian unemployment won't /necessarily/ follow the same movements as
## total unemployment, so I think it makes sense to just subtract and use
## "excess" American Indian unemployment.


## plot the original data just to see if my assumptions are valid
time = c(1:nrow(AI_unemp))
plot(time,AI_unemp$UnemploymentRate,type='l',col='red',xlab='Number of Months since 1/1/2000',ylab='American Indian Unemployment Rate')
plot(time,AI_unemp$UnemploymentRate,type='l',col='red',xlab='Number of Months since 1/1/2000',ylab='Unemployment Rate')
lines(time,tot_unemp$UNRATENSA,col='blue',lty=1)
legend(1,25,legend=c('American Indian','National'),col=c('red','blue'),lty=1:1)
## ehh kinda follows almost the same movement, but I still think it's worthwhile
## to do this transformation:

exc_AI_unemp = AI_unemp$UnemploymentRate - tot_unemp$UNRATENSA
head(exc_AI_unemp)
#lines(time,exc_AI_unemp,col='green')
plot(exc_AI_unemp,type='l',xlab='Number of Months since 1/1/2000',ylab='Excess American Indian Unemployment Rate')

## so now all I have to do is generate the intervention data, let's try a few out:

## 1) We'll start with the simplest, before date of intervention our variable is
## equal to zero and after it's equal to one. (note that Dec2017 is the 216th observation)
intervention1 = c(rep(0,216), rep(1,(276-216)))
plot(intervention1,type='l',xlab='Number of Months since 1/1/2000',ylab='Level of Intervention',main='Jump Intervention') ## nice

## 2) a. Next is the "gradually changing" function, which increases gradually from zero
## to one over time. We'll presumably have to mess around with this one quite a bit
## to get the "right" setting for how long it increases for and subsequently how fast it changes.
## Let's start by saying it takes 2 months to change, and so it increases by 1/3, 2/3, to 1.
intervention2 = c(rep(0,216), 1/3, 2/3, rep(1,(276-216-2)))
plot(intervention2,type='l',xlab='Number of Months since 1/1/2000',ylab='Level of Intervention',main='2 Month Intervention') ## nearly indistinguishable from the first, but still exactly what I want
## b. Increase over 4 months, so 0.2, 0.4, 0.6, 0.8, to 1
intervention3 = c(rep(0,216), 0.2, 0.4, 0.6, 0.8, rep(1,(276-216-4)))
plot(intervention3,type='l',xlab='Number of Months since 1/1/2000',ylab='Level of Intervention',main='4 Month Intervention') ## again, really tough to tell the difference here...
## c. Increase over 1 year, so 1/12 ... 11/12, to 1
intervention4 = c(rep(0,216), 1/12, 2/12, 3/12, 4/12, 5/12, 6/12, 7/12, 8/12, 9/12, 10/12, 11/12, rep(1,(276-216-11)))
plot(intervention4,type='l',xlab='Number of Months since 1/1/2000',ylab='Level of Intervention',main='1 Year Intervention') ## alright we can probably stop there for now
## d. ok, this was made after the 2 year one, but let's try a 1.5 year one
intervention5 = c(rep(0,216), 1/18, 2/18, 3/18, 4/18, 5/18, 6/18, 7/18, 8/18, 9/18, 10/18, 11/18, 12/18, 13/18, 
                  14/18, 15/18, 16/18, 17/18, rep(1,(276-216-17)))
plot(intervention5,type='l',xlab='Number of Months since 1/1/2000',ylab='Level of Intervention',main='1.5 Year Intervention')
## e. changed my mind after the fact, let's do a 2 year one
intervention6 = c(rep(0,216), 1/24,  2/24, 3/24, 4/24, 5/24, 6/24, 7/24, 8/24, 9/24, 10/24, 11/24, 12/24, 13/24, 
                  14/24, 15/24, 16/24, 17/24, 18/24, 19/24, 20/24, 21/24, 22/24, 23/24, rep(1,(276-216-23)))
plot(intervention6,type='l',xlab='Number of Months since 1/1/2000',ylab='Level of Intervention',main='2 Year Intervention')

## 3) The third option here is a function that pulses at the intervention date, then gradually declines.
## Presumably this is for something that isn't intended to have a long-lasting effect, but I would
## imagine the Congressman who proposed this bill didn't want this to be a temporary increase in
## American Indian employment, and would have preferred it to be indefinite, so let's just not
## make use of this kind of intervention function for now.

## just as a note, I expect the second intervention data setup to make much more sense
## in fitting the model, in particular the final setup that spans the intervention over a year,
## since (at least I believe it's the case that) government moves a bit slow, especially
## at the federal level.

## Now I have to make sure the data I care about is stationary, so let's test using Phillips-Perron
PP.test(exc_AI_unemp)
## says it's stationary at the 1% level, so we're good to go (makes sense, plot looked stationary).
## To implement our model, we find the best fitting ARIMA model (based on AIC) for our excess
## American Indian unemployment rate, then use that model with the various intervention data.
library(forecast)
check_model = auto.arima(exc_AI_unemp,stationary=TRUE)#,seasonal=FALSE,max.D =0)#,max.P=0,max.Q=0,max.D=0)
print(check_model[['arma']])
## gets us coefficients of (2,1,0), so now just do that exact model but with intervention data
intervention_model1 = arima(exc_AI_unemp,order=c(2,1,0),xreg=intervention1)
print('----------COEFFICIENTS FOR PURE JUMP----------')
print(intervention_model1[['coef']])
cat('AIC FOR THIS MODEL:', intervention_model1[['aic']])
print('')
print('')
intervention_model2 = arima(exc_AI_unemp,order=c(2,1,0),xreg=intervention2)
print('----------COEFFICIENTS FOR 2 MONTH GRADUAL INCREASE----------')
print(intervention_model2[['coef']])
cat('AIC FOR THIS MODEL:', intervention_model2[['aic']])
print('')
print('')
intervention_model3 = arima(exc_AI_unemp,order=c(2,1,0),xreg=intervention3)
print('----------COEFFICIENTS FOR 4 MONTH GRADUAL INCREASE----------')
print(intervention_model3[['coef']])
cat('AIC FOR THIS MODEL:', intervention_model3[['aic']])
print('')
print('')
intervention_model4 = arima(exc_AI_unemp,order=c(2,1,0),xreg=intervention4)
print('----------COEFFICIENTS FOR 1 YEAR GRADUAL INCREASE----------')
print(intervention_model4[['coef']])
cat('AIC FOR THIS MODEL:', intervention_model4[['aic']])
print('')
print('')
intervention_model5 = arima(exc_AI_unemp,order=c(2,1,0),xreg=intervention5)
print('----------COEFFICIENTS FOR 1.5 YEARS GRADUAL INCREASE----------')
print(intervention_model5[['coef']])
cat('AIC FOR THIS MODEL:', intervention_model5[['aic']])
print('')
print('')
intervention_model6 = arima(exc_AI_unemp,order=c(2,1,0),xreg=intervention6)
print('----------COEFFICIENTS FOR 2 YEARS GRADUAL INCREASE----------')
print(intervention_model6[['coef']])
cat('AIC FOR THIS MODEL:', intervention_model6[['aic']])
print('')
print('')

## check optimal ARIMA before and after Dec 2017
before = exc_AI_unemp[1:216]
after = exc_AI_unemp[216:length(exc_AI_unemp)]
check_model_before = auto.arima(before,stationary=TRUE)
cat('BEFORE ARIMA:', check_model_before[['arma']])
cat('BEFORE AIC:', check_model_before[['aic']])
check_model_after = auto.arima(after,stationary=TRUE)
cat('AFTER ARIMA:', check_model_after[['arma']])
cat('AFTER AIC:', check_model_after[['aic']])
## well... that's odd
## oh goodness, we should be using an ARIMA(1,0,0) model... the steps in Ender's
## say to use the best fit before the intervention if you have enough data available, so let's
## just do that


## gets us coefficients of (2,1,0), so now just do that exact model but with intervention data
intervention_model1 = arima(exc_AI_unemp,order=c(1,0,0),xreg=intervention1)
print('----------COEFFICIENTS FOR PURE JUMP----------')
print(intervention_model1[['coef']])
cat('AIC FOR THIS MODEL:', intervention_model1[['aic']])
print('')
print('')
intervention_model2 = arima(exc_AI_unemp,order=c(1,0,0),xreg=intervention2)
print('----------COEFFICIENTS FOR 2 MONTH GRADUAL INCREASE----------')
print(intervention_model2[['coef']])
cat('AIC FOR THIS MODEL:', intervention_model2[['aic']])
print('')
print('')
intervention_model3 = arima(exc_AI_unemp,order=c(1,0,0),xreg=intervention3)
print('----------COEFFICIENTS FOR 4 MONTH GRADUAL INCREASE----------')
print(intervention_model3[['coef']])
cat('AIC FOR THIS MODEL:', intervention_model3[['aic']])
print('')
print('')
intervention_model4 = arima(exc_AI_unemp,order=c(1,0,0),xreg=intervention4)
print('----------COEFFICIENTS FOR 1 YEAR GRADUAL INCREASE----------')
print(intervention_model4[['coef']])
cat('AIC FOR THIS MODEL:', intervention_model4[['aic']])
print('')
print('')
intervention_model5 = arima(exc_AI_unemp,order=c(1,0,0),xreg=intervention5)
print('----------COEFFICIENTS FOR 1.5 YEARS GRADUAL INCREASE----------')
print(intervention_model5[['coef']])
cat('AIC FOR THIS MODEL:', intervention_model5[['aic']])
print('')
print('')
intervention_model6 = arima(exc_AI_unemp,order=c(1,0,0),xreg=intervention6)
print('----------COEFFICIENTS FOR 2 YEARS GRADUAL INCREASE----------')
print(intervention_model6[['coef']])
cat('AIC FOR THIS MODEL:', intervention_model6[['aic']])
print('')
print('')