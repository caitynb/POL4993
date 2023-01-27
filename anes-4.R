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
library(xtable)
library(expss)

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

## econ composite (RELABELED)
anes20$econ1<-rowMeans(with(anes20, cbind(rserv1, rhel1, rjob1, ineqredstr,
                                          rgov1)), na.rm=T)

#### reliabilities -- I've provided some alternate code for getting alphas

## alpha: authoritarianism
psych::alpha(auth)
psych::alpha(with(anes20, cbind(resp1, manners1, obed1, behave1)))

## alpha: econ preferences (with extra more vs less government in general item)
psych::alpha(with(anes20, cbind(rserv1, rhel1, rjob1, ineqredstr, rgov1)))

#####engagement scale?

anes20$rattpol<-anes20$V201005
anes20$rattpol<-ifelse(anes20$V201005>0 & anes20$V201005<6,((5-anes20$V201005)/4),NA)
table(anes20$rattpol)

anes20$rattcamp<-anes20$V201006
anes20$rattcamp<-ifelse(anes20$V201006>0 & anes20$V201006<4,((3-anes20$V201006)/2),NA)
table(anes20$rattcamp)

##engagement composite
anes20$eng1<-rowMeans(with(anes20,cbind(rattpol,rattcamp,kn21, kn22, kn23, 
                                        kn24, kn25, kn26, kn27, 
                                        kn28, kn29, kn210)),na.rm=T)
table(anes20$eng1)
table(anes20$rknscal)

psych::alpha(with(anes20, cbind(rattpol,rattcamp,kn21, kn22, kn23, 
                                kn24, kn25, kn26, kn27, 
                                kn28, kn29, kn210)))



## save data in R format
save(anes20, file="anes20.Rdata")

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

###### main model
wm1<-svyglm(econ1 ~ age101+male1+rinc101+educ101+white1+black1+
              latin1+rknscal+rauth*pubasst1, design=sdata)
summary(wm1)
## get R2 with fit.svyglm from <poliscidata> package (ignore adjusted R2)
fit.svyglm(wm1)

#### conditional effects:
sim_margins(wm1, pred = rauth, modx = pubasst1)

#### plot, without auth x information interaction
f1 <- interact_plot(wm1, pred = "rauth", 
                    modx = "pubasst1", 
                    interval = TRUE, 
                    legend.main = "Receive Assistance?",
                    modx.values = NULL, 
                    modx.labels=c("Yes", "No"), 
                    colors="CUD Bright") + 
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) + 
  scale_x_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) + 
  theme_bw() + 
  theme(aspect.ratio=1, 
        plot.title = element_text(hjust = 0.5)) +
  theme(legend.position="bottom")+
  labs(title = "Social Spending, Authoritarianism, and Assistance Receipt\n(Without Information Interaction)",
       x = "Authoritarianism (0-1)",
       y = "Opposition for Social Spending (0-1)")
ggsave(file="f1.png", f1, width = 10, height = 8)

###### main model, with information interaction
wm2<-svyglm(econ1 ~ age101+male1+rinc101+educ101+white1+black1+
              latin1+rknscal+rauth*pubasst1+rauth*rknscal, design=sdata)
summary(wm2)
## get R2 with fit.svyglm from <poliscidata> package (ignore adjusted R2)
fit.svyglm(wm2)

#### conditional effects:
## by public assistance receipt:
sim_margins(wm2, pred = rauth, modx = pubasst1)
## by political information:
sim_margins(wm2, pred = rauth, modx = rknscal, 
            modx.values = c(seq(0, 1, by=.2)))

#### plot, with auth x pub asst interaction
f2 <- interact_plot(wm2, pred = "rauth", 
                    modx = "pubasst1", 
                    interval = TRUE, 
                    legend.main = "Receive Assistance?",
                    modx.values = NULL, 
                    modx.labels=c("Yes", "No"), 
                    colors="CUD Bright") + 
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) + 
  scale_x_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) + 
  theme_bw() + 
  theme(legend.position="bottom")+
  theme(aspect.ratio=1, 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Social Spending, Authoritarianism, and Assistance Receipt\n(With Information Interaction)",
       x = "Authoritarianism (0-1)",
       y = "Opposition for Social Spending (0-1)")
ggsave(file="f2.png", f2, width = 10, height = 8)

#### plot of auth x information interaction
f3 <- interact_plot(wm2, pred = "rauth", 
                    modx = "rknscal", 
                    interval = TRUE, 
                    legend.main = "Political Information (0-1)",
                    modx.values = c(seq(0, 1, by=.2)),
                    colors="CUD Bright") + 
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) + 
  scale_x_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) + 
  theme_bw() + 
  theme(legend.position="bottom")+
  theme(aspect.ratio=1, 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Social Spending, Authoritarianism, and Information",
       x = "Authoritarianism (0-1)",
       y = "Opposition for Social Spending (0-1)")
ggsave(file="f3.png", f3, width = 10, height = 8)

#### table (you can manually add the R2)
t1<-huxreg(wm1, wm2,
           statistics = c("N" = "nobs"),
           number_format = 2)
quick_docx(t1, file='t1.docx')
quick_latex(t1,file="t1.tex")



#########################
## main model test with engagement
wm3<-svyglm(econ1 ~ age101+male1+rinc101+educ101+white1+black1+
              latin1+rknscal+rauth*pubasst1+rauth*eng1, design=sdata)
summary(wm3)
## get R2 with fit.svyglm from <poliscidata> package (ignore adjusted R2)
fit.svyglm(wm3)

sim_margins(wm3, pred = rauth, modx = eng1,
            modx.values = c(seq(0, 1, by=.2)))



f4 <- interact_plot(wm3, pred = "rauth", 
                    modx = "eng1", 
                    interval = TRUE, 
                    legend.main = "Political Engagement (0-1)",
                    modx.values = c(seq(0, 1, by=.2)),
                    colors="CUD Bright") + 
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) + 
  scale_x_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) + 
  theme_bw() + 
  theme(legend.position="bottom")+
  theme(aspect.ratio=1, 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Social Spending, Authoritarianism, and Engagement",
       x = "Authoritarianism (0-1)",
       y = "Opposition for Social Spending (0-1)")
ggsave(file="f4.png", f4, width = 10, height = 8)
f4

################################################################################
######## simple models -- for supplemental materials

#### 1
wm1s<-svyglm(econ1 ~ rauth*pubasst1+rknscal, design=sdata)
summary(wm1s)
fit.svyglm(wm1s)

sim_margins(wm1s, pred = rauth, modx = pubasst1)

#### 2
wm2s<-svyglm(econ1 ~ rauth*pubasst1+rauth*rknscal, design=sdata)
summary(wm2s)
fit.svyglm(wm2s)

sim_margins(wm2s, pred = rauth, modx = pubasst1)
sim_margins(wm2s, pred = rauth, modx = rknscal, 
            modx,values = c(seq(0, 1, by=.2)))
wm1<-var_lab(wm1,"Without knowledge")
#### supplemental table
t2<-huxreg(wm1s,wm2s,
           statistics = c("N" = "nobs"),
           number_format = 2)
quick_docx(t2, file='t2.docx')

