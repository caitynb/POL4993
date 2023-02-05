############################################################################
### ESS 2020

# clear environment
rm(list=ls())

# load libraries
library(car)
library(psych)
library(ggplot2)
library(AER)
library(estimatr)
library(summarytools)
library(dotwhisker)
library(dplyr)
library(lavaan)
library(hrbrthemes)
library(survey)
library(tidyverse)
library(interactions)
library(corrplot)
library(Hmisc)
library(haven)
library(poliscidata)
library(lme4)
library(lmerTest)
library(devtools)

# import data
ess8 <- read_dta(file="ESS8e02_2.dta")

# load coded Rdata:
load("ess8.Rdata")

# federico 0-1 recode
std01<-function(x){
  min.x<-min(x, na.rm=T)
  max.x<-max(x-min.x, na.rm=T)
  return((x-min.x)/max.x)}

############################################################################
###### coding

######demographics

## income
ess8$rinc01<-std01(ess8$hinctnta)

## age
ess8$age<-as.numeric(ess8$agea)
ess8$rage<-std01(ess8$age)

## gender
ess8$rsex<-as.numeric(as_factor(ess8$gndr))
ess8$male<-as_factor(Recode(ess8$rsex, "1=1; 2=0; 3=NA"))

## education
ess8$edr1<-as_factor(ess8$eisced)
ess8$edr2<-ifelse(as.numeric(as_factor(ess8$eisced))<9, 
                  as.numeric(as_factor(ess8$eisced))-2, NA)
ess8$edr2r<-ess8$edr2/6

ess8$edr3<-Recode(ess8$edr2, "0=0; 1=0; 2=1; 3=1; 4=2; 5=3; 6=3")
ess8$edr3<-factor(ess8$edr3, labels = c("Lower Secondary or Less", 
                                        "Upper Secondary", "Advanced Vocational",
                                        "College Degree"))

## minority
ess8$rmin<-as.numeric(as_factor(ess8$blgetmg))
ess8$rmin<-as_factor(Recode(ess8$rmin, "1=1; 2=0; 3:5=NA"))

## union member
ess8$union<-ifelse(ess8$mbtru<3, 1, 0)

#### political interest
ess8$int<-as.numeric(as_factor(ess8$polintr))
ess8$int<-Recode(ess8$int, "1=4; 2=3; 3=2; 4=1; 5:7=NA")
ess8$rint<-std01(ess8$int)
ess8$intf<-as.factor(ess8$int)

ess8$intf<-factor(ess8$intf, labels = c("Not at All", "Hardly", "Quite", "Very"))

#####authoritarianism/conservation
#### value recodes.
ess8$rsec1<-(6-ess8$impsafe)
ess8$rsec2<-(6-ess8$ipstrgv)
ess8$rcon1<-(6-ess8$ipfrule)
ess8$rcon2<-(6-ess8$ipbhprp)
ess8$rtrad1<-(6-ess8$ipmodst)
ess8$rtrad2<-(6-ess8$imptrad)
ess8$runi1<-(6-ess8$ipeqopt)
ess8$runi2<-(6-ess8$ipudrst)
ess8$runi3<-(6-ess8$impenv)
ess8$rben1<-(6-ess8$iphlppl)
ess8$rben2<-(6-ess8$iplylfr)
ess8$rselfd1<-(6-ess8$ipcrtiv)
ess8$rselfd2<-(6-ess8$impfree)
ess8$rstim1<-(6-ess8$impdiff)
ess8$rstim2<-(6-ess8$ipadvnt)
ess8$rhed1<-(6-ess8$ipgdtim)
ess8$rhed2<-(6-ess8$impfun)
ess8$rach1<-(6-ess8$ipsuces)
ess8$rach2<-(6-ess8$ipshabt)
ess8$rpow1<-(6-ess8$imprich)
ess8$rpow2<-(6-ess8$iprspot)

## reversed value items.
ess8$rrstim1<-(ess8$impdiff-1)
ess8$rrstim2<-(ess8$ipadvnt-1)
ess8$rrselfd1<-(ess8$ipcrtiv-1)
ess8$rrselfd2<-(ess8$impfree-1)
ess8$rrhed1<-(ess8$ipgdtim-1)
ess8$rrhed2<-(ess8$impfun-1)
ess8$rrach1<-(ess8$ipsuces-1)
ess8$rrach2<-(ess8$ipshabt-1)
ess8$rrpow1<-(ess8$imprich-1)
ess8$rrpow2<-(ess8$iprspot-1)

## security values
ess8$rsec<-std01(rowMeans(with(ess8, cbind(rsec1, rsec2))))

## conformity values
ess8$rcon<-std01(rowMeans(with(ess8, cbind(rcon1, rcon2))))

## tradition values
ess8$rtrad<-std01(rowMeans(with(ess8, cbind(rtrad1, rtrad2))))

## conservation values (high = greater endorsement of CV)
psych::alpha(with(ess8, cbind(rsec1, rsec2, rcon1, rcon2, rtrad1, rtrad2)))
ess8$rcv<-std01(rowMeans(with(ess8, cbind(rsec1, rsec2, rcon1, rcon2, rtrad1, rtrad2))))


#################### benefits received 0-1
# 1 --> did not receive public assistance; 0 = did receive assistance.

table(ess8$hincsrca)

ess8$pubasst1<-ifelse(ess8$hincsrca==6|ess8$hincsrca==5,0,1)
table(ess8$pubasst1)

####################### economic preferences
ess8$rec1<-ifelse(ess8$gincdif<6, (ess8$gincdif-1)/4, NA)
table(ess8$gincdif)
table(ess8$rec1)
ess8$rec2<-ifelse(ess8$gvslvol<11, (10-ess8$gvslvol)/10, NA)
table(ess8$gvslvol)
table(ess8$rec2)
ess8$rec3<-ifelse(ess8$gvslvue<11, (10-ess8$gvslvue)/10, NA)
table(ess8$gvslvue)
table(ess8$rec3)
ess8$rec4<-ifelse(ess8$gvcldcr<11, (10-ess8$gvcldcr)/10, NA)
table(ess8$gvcldcr)
table(ess8$rec4)

## economic preference scale (high = more right-wing/market oriented)
psych::alpha(with(ess8, cbind(rec1, rec2, rec3, rec4)))
ess8$recsc1<-rowMeans(with(ess8, cbind(rec1, rec2, rec3, rec4)))

##check correlations for eco pref items
ecopref<-cbind.data.frame(ess8$rec1, ess8$rec2,ess8$rec3,ess8$rec4)
ecocorr<-rcorr(as.matrix(ecopref), type="pearson")

#### recode nation variable
ess8$nat1<-as_factor(ess8$cntry)
ess8$nat1<-factor(ess8$cntry,
                  labels=c("Austria",
                           "Belgium",
                           "Switzerland",
                           "Czechia",
                           "Germany",
                           "Estonia",
                           "Spain",
                           "Finland",
                           "France",
                           "United Kingdom",
                           "Hungary",
                           "Ireland",
                           "Israel",
                           "Iceland",
                           "Italy",
                           "Lithuania",
                           "Netherlands",
                           "Norway",
                           "Poland",
                           "Portugal",
                           "Russia",
                           "Sweden",
                           "Slovenia"))
ess8$nat2<-as.numeric(ess8$nat1)

## save data in R format
save(ess8, file="ess8.Rdata")

################################################################################
######## weighting setup

### WEIGHTS AVAILABLE: pspwght, pweight, dweight, ***anweight
### NOTE: anweight = pspwght*pweight

### rescale weights for use in lme4
### rescaled weights: pweights_a and pweights_b
### use pweights_a for point estimates (Carle 2009)
library(datawizard)
ess8<-rescale_weights(ess8, "nat1", "anweight", nest = FALSE)

# https://easystats.github.io/datawizard/reference/rescale_weights.html

################################################################################
################################################################################
######## regressions -- mixed models

###### main model
wm1<-lmer(recsc1~1+rinc01+rage+male+edr3+rmin+union+intf+rcv*pubasst1+
            (1+rcv|nat1), weights=pweights_a, data = ess8, REML = F)
summary(wm1)

### conditional effects
sim_margins(wm1, pred = rcv, modx = pubasst1)

#### plot, without auth x information interaction
f4 <- interact_plot(wm1, pred = "rcv", 
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
  labs(title = "Social Spending, Conservation Values, and Assistance Receipt\n(Without Information Interaction)",
       x = "Conservation Values (0-1)",
       y = "Opposition to Social Spending (0-1)")
ggsave(file="f4.png", f4, width = 10, height = 8)

###### main model with interest interaction
wm2<-lmer(recsc1~1+rinc01+rage+male+edr3+rmin+union+intf+rcv*pubasst1+rcv*intf+
            (1+rcv|nat1), weights=pweights_a, data = ess8, REML = F)
summary(wm2)

### conditional effects
### note that with factor variables, sim_margins will sometimes report the
### conditional effects in alphabetical order of the labels rather than the
### underlying values -- be careful with this!
sim_margins(wm2, pred = rcv, modx = pubasst1)
sim_margins(wm2, pred = rcv, modx = intf)

#### plot, with auth x information interaction
f5 <- interact_plot(wm2, pred = "rcv", 
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
  labs(title = "Social Spending, Conservation Values, and Assistance Receipt\n(Without Information Interaction)",
       x = "Conservation Values (0-1)",
       y = "Opposition to Social Spending (0-1)")
ggsave(file="f5.png", f5, width = 10, height = 8)

#### plot of auth x information interaction
f6 <- interact_plot(wm2, pred = "rcv", 
                    modx = "intf", 
                    interval = TRUE, 
                    legend.main = "Interested in Politics?",
                    modx.values = NULL, 
                    #modx.labels=c("Yes", "No"), 
                    colors="CUD Bright") + 
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) + 
  scale_x_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) + 
  theme_bw() + 
  theme(aspect.ratio=1, 
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Social Spending, Conservation Values, and Interest",
       x = "Conservation Values (0-1)",
       y = "Opposition to Social Spending (0-1)")
ggsave(file="f6.png", f6, width = 10, height = 8)

#####make regression table 

devtools::install_github("jacob-long/jtools",force=TRUE)
## getting an error
esst2<-huxreg(wm1, wm2, 
              statistics = c("N" = "nobs"),
              number_format = 2)
quick_docx(esst2, file='esst2.docx')

