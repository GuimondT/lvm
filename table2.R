## TABLE 2 in the paper

## cause specific hazards for the external causes of death
## A Ledberg 2022 01 14

require(dplyr)
require(forcats)
require(survival)
## need jtools for the summ function
require(jtools)
require(broom.mixed)
require(ggstance)

## load the dataset
dat <- readRDS("publicData.rds")

## introduce an id variable 

################################################3
## define the cut points for survsplit
##cuts <- c(15,45,105,225,366)

cuts <- c(14,31,90,182,366)
## use survSplit from survival package to put data in a good shape for
## the time-interval dependent analysis
spdat <- survSplit(Surv(fupTime,as.factor(event))~sex+agroup+ygroup,cut=cuts,episode="epi",zero=-0.01,id="id",data=dat)


## do run the glm (piecewise expontneial model we must have a duration)
spdat <- spdat %>%
    mutate(dur=fupTime-tstart,
           epi=factor(epi,levels=c(1:length(cuts))),
           repi=fct_rev(epi))


## overall regression on the different outcomes
outcome <- spdat$event=="external"
fit.e <- glm(outcome~epi+ygroup+sex + agroup+offset(log(dur)),data=spdat,family=poisson)
##summary(fit.e)
summ(fit.e,confint=1,exp=1)
fit.er <- glm(outcome~repi+ygroup+sex + agroup+offset(log(dur)),data=spdat,family=poisson)
##summary(fit.e)
summ(fit.er,confint=1,exp=1)
plot_summs(fit.e,exp=TRUE)
plot_summs(fit.er,exp=TRUE)

###############################################################################
## check that the results hold if we run the analysis in the sexes separtely

## for men
fit.e.men <- glm(outcome~epi+ygroup+agroup+offset(log(dur)),data=spdat,family=poisson,subset=sex=="men")
summ(fit.e.men,confint=1,exp=1)

## and for women
fit.e.women <- glm(outcome~epi+ygroup+agroup+offset(log(dur)),data=spdat,family=poisson,subset=sex=="women")
summ(fit.e.women,confint=1,exp=1)

###############################################################################
## check if the results hold also in different age groups

## possible values of the age groups are
table(dat$agroup)
fit.e.age <- glm(outcome~epi+ygroup+sex+offset(log(dur)),data=spdat,family=poisson,subset=agroup=="[18,25]")
summ(fit.e.age,confint=1,exp=1)








