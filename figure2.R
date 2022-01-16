###################################################################33
## FIGURE 2 in the paper
## plot cause-specific rates

## A Ledberg, 2022 01 14

## Most of the code below is just a mapping from the output from
## survival package to plot nicely with the ggplot2 package. These
## two packages do all the real work


require(ggplot2)
require(dplyr)
require(tidyr)
require(survival)

dat <- readRDS("publicData.rds")

tdat <- dat %>% mutate(age=ifelse(young==0,"o","y")) %>%
    mutate(state=ifelse(event=="lvm","lvm+prison",
                 ifelse(event=="prison","lvm+prison",event))) %>%
    mutate(state=ifelse(state=="dead","other causes",state)) %>%
    mutate(state=ifelse(state=="external","external causes",state))


fit <- survfit(Surv(fupTime,as.factor(state))~1+sex+age,data=tdat)
sfit <- summary(fit,times=seq(0,365,1))


test1 <- with(sfit,data.frame(time=time,pstate=pstate,strata=strata)) %>%
    pivot_longer(contains("pstate"),names_to="state",values_to="pstate") %>% 
    mutate(state=ifelse(state=="pstate.1",sfit$states[1],
                 ifelse(state=="pstate.2",sfit$states[2],
                 ifelse(state=="pstate.3",sfit$states[3],
                 ifelse(state=="pstate.4",sfit$states[4],
                 ifelse(state=="pstate.5",sfit$states[5],NA)))))) %>%
    filter(state!="(s0)")

test2 <- with(sfit,data.frame(time=time,upper=upper,strata=strata)) %>%
    pivot_longer(contains("upper"),names_to="state",values_to="upper") %>% 
    mutate(state=ifelse(state=="upper.1",sfit$states[1],
                 ifelse(state=="upper.2",sfit$states[2],
                 ifelse(state=="upper.3",sfit$states[3],
                 ifelse(state=="upper.4",sfit$states[4],
                 ifelse(state=="upper.5",sfit$states[5],NA)))))) %>%
    filter(state!="(s0)")

test3 <- with(sfit,data.frame(time=time,lower=lower,strata=strata)) %>%
    pivot_longer(contains("lower"),names_to="state",values_to="lower") %>% 
    mutate(state=ifelse(state=="lower.1",sfit$states[1],
                 ifelse(state=="lower.2",sfit$states[2],
                 ifelse(state=="lower.3",sfit$states[3],
                 ifelse(state=="lower.4",sfit$states[4],
                 ifelse(state=="lower.5",sfit$states[5],NA)))))) %>%
    filter(state!="(s0)")

pdat <- left_join(test1,test2,by=c("time","strata","state")) %>%
    left_join(.,test3,by=c("time","strata","state")) %>% 
    mutate(strata=ifelse(strata=="sex=men, age=o","men > 36",
                  ifelse(strata=="sex=men, age=y","men <= 36",
                  ifelse(strata=="sex=women, age=y","women <= 36",
                  ifelse(strata=="sex=women, age=o","women > 36",NA))))) %>% 
    filter(state!="lvm+prison")

ggplot(data=pdat,aes(x=time,y=pstate,col=state))+geom_line()+geom_point(size=0.6)+
    facet_wrap(~strata)+
    geom_ribbon(aes(ymin=lower,ymax=upper,fill=state),alpha=0.2)+
    theme_bw(base_size=16)+xlab("time from LVM discharge (days)")+
    ylab("marginal probability")


ggsave("pstate_death.pdf",width=10,height=10)




