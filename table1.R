## TABLE 1 in the paper

## A Ledberg 2022 01 14

require(dplyr)

## use epitools package for exact Binomial confidence intervals
require(epitools)

## load the dataset
dat <- readRDS("publicData.rds")

## the variable dead marks all persons who died within one year
## and the intersection of this with "external" will give the external deaths

tab <- dat %>% group_by(agroup,sex) %>%
    summarize(n=n(),nd=sum(dead),ext=sum(dead&external),pois=sum(dead&poison)) %>%
    ungroup() %>% 
    mutate(prob=round(nd/n,3),pext=round(ext/nd,3),ppois=round(pois/nd,3),
           cis=round(binom.exact(nd,n),3))

tab <- bind_cols(dplyr::select(tab %>% dplyr::select(-n),-cis),tab$cis)
print(tab,w=100)

##ofn <- "table1.txt"
##write.table(tab,ofn)















