rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()
options(mc.cores = parallel::detectCores())

setwd("~/Documents/git/seedpriorities/full_exp")
library(dplyr,quietly = TRUE)
library(xtable,quietly=TRUE)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tidyselect)
library(reshape2)
library(brms)

d<-read.csv("full_data_sheet.csv")

table(d$taxa)
d<-filter(d,pot_type=="Cc.Hm")
sapply(d, class)
cols.num <- 14:40
d[cols.num] <- sapply(d[cols.num],as.numeric)


d<-d%>% pivot_longer(14:40,names_to="date",values_to = "count")
d<-select(d,-date)
d2<-d%>% distinct()

d#2<-d2 %>% tidyr::gather(date,count,14:40) depreciated



d2$count<-ifelse(is.na(d2$count),0,d2$count)
unique(d2$config)
d2$type<-ifelse(d2$config %in% c("monoA","monoB"),"single species","competition") 
class(d2$strat)
d2$stratification<-ifelse(d2$strat==6,"low","high")
#niche<-brm(count~taxa*type*factstrat,data=d2, family=bernoulli(link=logit))

## Does competition reduce germination of C candensis

d3<-filter(d2,taxa=="C.canadensis")
niche<-brm(count~type*stratification,data=d3, family=bernoulli(link=logit))
brms::fixef(niche,probs=c(.25,.75))

c_eff<-conditional_effects(niche,"type:stratification",prob=0.8)
df <- as.data.frame(c_eff$`type:stratification`)
pd=position_dodge(width=0.2)
aplot<-ggplot(df,aes(stratification,estimate__))+geom_point(aes(color=type),size=3,position=pd)+geom_errorbar(aes(ymin=lower__,ymax=upper__,color=type),position=pd,width=0)+
  ggthemes::theme_few()+ylab("Likelihood of Germination")+scale_color_viridis_d(option="turbo")


dat<-read.csv("data_4_invasive.csv")
dat$stratification<-ifelse(dat$strat==6,"low","high")

gruber<-brm(MGT_Cc~type*stratification,data=dat)
summary(gruber)

c_eff2<-conditional_effects(gruber,"type:stratification",prob=.8)
df2 <- as.data.frame(c_eff2$`type:stratification`)
bplot<-ggplot(df2,aes(stratification,estimate__))+geom_point(aes(color=type),size=3,position=pd)+geom_errorbar(aes(ymin=lower__,ymax=upper__,color=type),width=0,position=pd)+
  ggt
hemes::theme_few()+ylab("Mean Germination Time")+scale_color_viridis_d(option="turbo")

dat.comp<-filter(dat, type=="competition")
compan<-brm(MGT_Cc~MGT_Hm*stratification,data=dat.comp)

c_eff3<-conditional_effects(compan,"MGT_Hm:stratification",prob=0.8)


c<-plot(c_eff3, plot = FALSE)[[1]]+scale_color_viridis_d(option="plasma",,begin = 0,end=.7)+
  scale_fill_viridis_d(option="plasma",begin = 0,end=.7)+ggthemes::theme_few()+
  ylab("MGT Honewort")+xlab("MGT Dames Rocket")+theme(legend.position="bottom")

fixef(compan)

plotaa<-ggpubr::ggarrange(aplot,bplot,common.legend=TRUE,labels =c( "a)","b)"))
ggpubr::ggarrange(plotaa,c,nrow=2,labels=c("","c)"))


jpeg("..//figure/nichemodfication.jpeg")
plotaa
dev.off()  
