################################################################################
### ANES 2020

rm(list=ls())

#### package check: this code will make sure that all of the packages we need
#### to use are installed. It will install all the packages that are not 
#### installed yet. Run this first!
pkg <- c("car", "psych", "ggplot2", "AER", "summarytools", "survey", 
         "poliscidata", "tidyverse", "interactions", "mirt", "huxtable", 
         "flextable")
new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
if(length(new.pkg)) install.packages(new.pkg)

#load libraries
library(car)
library(psych)
library(ggplot2)
library(AER)
library(summarytools)
library(survey)
library(poliscidata)
library(tidyverse)
library(interactions)
library(mirt)
library(huxtable)

#import csv
anes20<-read.csv("anes_timeseries_2020_csv_20220210.csv")

#load coded Rdata:
load("anes20.Rdata")

## recode 0-1 function
std01<-function(x){
  min.x<-min(x, na.rm=T)
  max.x<-max(x-min.x, na.rm=T)
  return((x-min.x)/max.x)}

################################################################################
######## coding

## demographics, 2020.
anes20$age1<-anes20$V201507x
anes20$age101<-std01(anes20$V201507x)
anes20$educ1<-ifelse(anes20$V201511x>0, anes20$V201511x, NA)  
anes20$educ101<-std01(anes20$educ1)
anes20$rinc1<-ifelse(anes20$V201617x>0, anes20$V201617x, NA)  
anes20$rinc101<-std01(anes20$rinc1)
anes20$white1<-ifelse(anes20$V201549x==1, 1, ifelse(anes20$V201549x!=1 & anes20$V201549x>0, 0, NA))
anes20$black1<-ifelse(anes20$V201549x==2, 1, ifelse(anes20$V201549x!=2 & anes20$V201549x>0, 0, NA))
anes20$latin1<-ifelse(anes20$V201549x==3, 1, ifelse(anes20$V201549x!=3 & anes20$V201549x>0, 0, NA))
anes20$asian1<-ifelse(anes20$V201549x==4, 1, ifelse(anes20$V201549x!=4 & anes20$V201549x>0, 0, NA))
anes20$male1<-ifelse(anes20$V201600==1, 1, 0)
anes20$college1<-ifelse(anes20$V201511x>3, 1, ifelse(anes20$V201511x<4 & anes20$V201511x>0, 0, NA))
anes20$rhet1<-ifelse(anes20$V201601==1, 1, ifelse(anes20$V201601>0, 0, NA))

## information.
anes20$kn21<-ifelse(anes20$V202217==1 & anes20$V202218==2, 1, 
                  ifelse(anes20$V202217==2 | anes20$V202218==1, 0, NA))
anes20$kn22<-ifelse(anes20$V202138y>-1, anes20$V202138y, NA)
anes20$kn23<-ifelse(anes20$V202139y1>-1, anes20$V202139y1, NA)
anes20$kn24<-ifelse(anes20$V202140y1>-1, anes20$V202140y1, NA)
anes20$kn25<-ifelse(anes20$V202141y1>-1, anes20$V202141y1, NA)
anes20$kn26<-ifelse(anes20$V202142y2>-1, anes20$V202142y2, NA)
anes20$kn27<-ifelse(anes20$V201644==6 & anes20$V201644>0, 1, 
                  ifelse(anes20$V201644!=6 & anes20$V201644>0, 0, NA))   
anes20$kn28<-ifelse(anes20$V201646==1 & anes20$V201646>0, 1, 
                  ifelse(anes20$V201646!=1 & anes20$V201646>0, 0, NA)) 
anes20$kn29<-ifelse(anes20$V201647==2 & anes20$V201647>0, 1, 
                  ifelse(anes20$V201647!=2 & anes20$V201647>0, 0, NA)) 
anes20$kn210<-ifelse(anes20$V201645==1 & anes20$V201645>0, 1, 
                   ifelse(anes20$V201645!=1 & anes20$V201645>0, 0, NA)) 
psych::alpha(with(anes20, cbind(kn21, kn22, kn23, kn24, kn25,
                              kn26, kn27, kn28, kn29, kn210)))
anes20$rknscal<-rowMeans(with(anes20, cbind(kn21, kn22, kn23, 
                                         kn24, kn25, kn26, kn27, 
                                        kn28, kn29, kn210)), na.rm=TRUE)

############making authoritarianism scale 
##anes20$V202266 is independence vs respect for elders
##anes20$V202267 is curiosity vs good manners
##anes20$V202268 is obedience vs. self-reliance
##anes20$V202269 is consideration vs. well-behaved

#making all values that are not 1 or 2 NA
anes20$resp1<-anes20$V202266
anes20$manners1<-anes20$V202267
anes20$obed1<-anes20$V202268
anes20$behave1<-anes20$V202269

table(anes20$resp1)
table(anes20$manners1)
table(anes20$obed1)
table(anes20$behave1)

##making all values that are not 1, 2, or 3 NA
anes20$resp1[anes20$resp1<1]<-NA
anes20$manners1[anes20$manners1<1]<-NA
anes20$obed1[anes20$obed1<1]<-NA
anes20$behave1[anes20$behave1<1]<-NA

anes20$resp1[anes20$resp1>3]<-NA
anes20$manners1[anes20$manners1>3]<-NA
anes20$obed1[anes20$obed1>3]<-NA
anes20$behave1[anes20$behave1>3]<-NA

#making 0-1
anes20$resp1<-ifelse(anes20$resp1==2,1,0) 
anes20$manners1<-ifelse(anes20$manners1==2,1,0) 
anes20$obed1<-ifelse(anes20$obed1==1,1,0) 
anes20$behave1<-ifelse(anes20$behave1==2,1,0) 

anes20$auth<-(anes20$resp1+anes20$manners1+anes20$obed1+anes20$behave1)/4
anes20$rauth<-rowMeans(with(anes20, cbind(resp1, manners1, obed1, behave1)))
auth<-data.frame(with(anes20, cbind(resp1, manners1, obed1, behave1)))

# authoritarianism IRT
# model 
lm1<-mirt(data = with(anes20, cbind(resp1, manners1, obed1, behave1)), 
          model = 1, itemtype = "2PL", verbose = FALSE)
summary(lm1)
coef(lm1)
# latent trait scores
anes20$fsaut <- fscores(lm1, method='EAP')
anes20$rfsaut <- std01(anes20$fsaut)

##variable anes20$auth is 0-1, 0 being fluid and 1 being fixed

# making variable PUBASST 0-1
# 1 --> did not receive public assistance; 0 = did receive assistance.
anes20$pubasst<-anes20$V202563
table(anes20$pubasst)
anes20$pubasst[anes20$pubasst<1]<-NA
anes20$pubasst1<-as.factor(ifelse(anes20$pubasst==1, 0, 1))
table(anes20$pubasst1)

#eco pref; high = right wing
## services and spending
anes20$rserv1<-ifelse(anes20$V201246>0 & anes20$V201246<8,((7-anes20$V201246)/6),NA)
rserv1<-anes20$rserv1
## govt health insurance
anes20$rhel1<-ifelse(anes20$V201252>0 & anes20$V201252<8,((anes20$V201252-1)/6),NA)
rhel1<-anes20$rhel1
## guaranteed jobs
anes20$rjob1<-ifelse(anes20$V201255>0 & anes20$V201255<8,((anes20$V201255-1)/6),NA)
rjob1<-anes20$rjob1
## favor/oppose government trying to reduce income inequality
## this is the correct 'reduce inequality' item
anes20$ineqredstr<-ifelse(anes20$V202259x>0, (anes20$V202259x-1)/6, NA)
ineqredstr<-anes20$ineqredstr

## more vs less government in general
## this is NOT the reduce inequality item, but just asks about general size of
## of government. It might be useful to us, though.
anes20$rgov1<-ifelse(anes20$V202255x>0, (6-anes20$V202255x)/5, NA)
rgov1<-anes20$rgov1

## econ composites
anes20$econ1<-rowMeans(with(anes20, cbind(rserv1, rhel1, rjob1, ineqredstr)), na.rm=T)
anes20$econ2<-rowMeans(with(anes20, cbind(rserv1, rhel1, rjob1, ineqredstr,
                                          rgov1)), na.rm=T)

#### IRT models for economics
## original scales
anes20$ec1<-ifelse(anes20$V201246>0 & anes20$V201246<8, 8-anes20$V201246, NA)
anes20$ec2<-ifelse(anes20$V201252>0 & anes20$V201252<8, anes20$V201252, NA)
anes20$ec3<-ifelse(anes20$V201255>0 & anes20$V201255<8, anes20$V201255, NA)
anes20$ec4<-ifelse(anes20$V202259x>0, anes20$V202259x, NA)
anes20$ec5<-ifelse(anes20$V202255x>0, 8-anes20$V202255x, NA)

## grm model (1)
lm2a<-mirt(data = with(anes20, cbind(ec1, ec2, ec3, ec4)), 
          model = 1, itemtype = "graded", verbose = FALSE)
summary(lm2a)
coef(lm2a)
# latent trait scores
anes20$fsec1 <- fscores(lm2a, method='EAP')
anes20$rfsec1 <- std01(anes20$fsec1)

## grm model (2)
lm2b<-mirt(data = with(anes20, cbind(ec1, ec2, ec3, ec4, ec5)), 
           model = 1, itemtype = "graded", verbose = FALSE)
summary(lm2b)
coef(lm2b)
# latent trait scores
anes20$fsec2 <- fscores(lm2b, method='EAP')
anes20$rfsec2 <- std01(anes20$fsec2)

#### reliabilities -- I've provided some alternate code for getting alphas

## alpha: authoritarianism
psych::alpha(auth)
psych::alpha(with(anes20, cbind(resp1, manners1, obed1, behave1)))

## alpha: econ preferences 1
psych::alpha(ecopref)
psych::alpha(with(anes20, cbind(rserv1, rhel1, rjob1, ineqredstr)))

## alpha: econ preferences 2 (with extra more vs less government in general item)
psych::alpha(with(anes20, cbind(rserv1, rhel1, rjob1, ineqredstr, rgov1)))

## save data in R format
save(anes20, file="anes20.Rdata")


################################################################################
######## regressions -- unweighted

#### simple model
m1<-lm(econ2 ~ rauth*pubasst1, data=anes20)
## for model stats:
summary(m1)
## for SEs and tests:
coeftest(m1, vcovHC(m1, type = "HC3"))
## conditional effects
sim_slopes(m1, pred = rauth, modx = pubasst1, robust=T)

#### with covariates
m2<-lm(econ2 ~ rauth*pubasst1+age101+male1+rinc101+educ101+white1+black1+
                latin1+rknscal, data=anes20)
## for model stats
summary(m2)
## for SEs and tests:
coeftest(m2, vcovHC(m2, type = "HC3"))
## conditional effects
sim_slopes(m2, pred = rauth, modx = pubasst1, robust=T)

#### table
t1<-huxreg(m1, m2, 
            statistics = c("N" = "nobs", 
                           "R squared" = "r.squared"),
            number_format = 2)
quick_docx(t1, file='t1.docx')

################################################################################
######## weighting setup

#### dataframe for weighted, pre/post (drops NAs on weight):
data <- subset(anes20, is.na(V200010b)==F)

#### design object for svy analyses (pre/post):
sdata <-
  svydesign(~V200010c ,               
            strata = ~V200010d ,            
            data = data , 
            weights = ~V200010b ,
            nest = TRUE 
  )

#### subset design object for white respondents if needed.
sdataw <- subset(sdata, white1==1)

################################################################################
######## regressions -- weighted

#### simple model
wm1<-svyglm(econ2 ~ rauth*pubasst1, design=sdata)
summary(wm1)
## get R2 with fit.svyglm from <poliscidata> package (ignore adjusted R2)
fit.svyglm(wm1)
## conditional effects
sim_slopes(wm1, pred = rauth, modx = pubasst1)

#### with covariates
wm2<-svyglm(econ2 ~ rauth*pubasst1+age101+male1+rinc101+educ101+white1+black1+
                latin1+rknscal, design=sdata)
summary(wm2)
## get R2 with fit.svyglm from <poliscidata> package (ignore adjusted R2)
fit.svyglm(wm2)
## conditional effects
sim_slopes(wm2, pred = rauth, modx = pubasst1)

#### table
t2<-huxreg(wm1, wm2, 
           statistics = c("N" = "nobs"),
           number_format = 2)
quick_docx(t2, file='t2.docx')
