## Table 4 in the paper

## A Ledberg 2022 01 16

require(dplyr)
require(survival)

## load the dataset
dat <- readRDS("publicData.rds")

## use survsplit to get a convenient representation of the data
cuts <- c(14,366)

spdat <- survSplit(Surv(fupTime,as.factor(event))~sex+agroup+ygroup,cut=cuts,episode="epi",zero=-0.01,id="id",data=dat) %>%
    mutate(dur=fupTime-tstart,epi=factor(epi,levels=c(1:length(cuts)))) %>% 
    filter( (agroup=="[18,25]"|agroup=="(25,36]") ) %>%
    mutate(interval=ifelse(epi==1,"first two weeks","remaining time")) %>%
    mutate(death=ifelse(event=="external"|event=="dead",1,0))

spdat %>% group_by(interval,sex) %>%
    summarize(N=n(),person_time=sum(dur)/365.25,deaths=sum(death),rate=deaths/person_time)
