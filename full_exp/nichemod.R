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
niche<-brm(count~taxa*type*factstrat,data=d2, family=bernoulli(link=logit))
summary(niche1)

conditional_effects(niche)

d2$factstrat<-as.factor(d2$strat)
d3<-filter(d2,taxa=="C.canadensis")
niche3<-brm(count~type*factstrat,data=d3, family=bernoulli(link=logit))
summary(niche3)
exp(fixef(niche3))
plot1<-conditional_effects(niche3,effects="type:factstrat")
plot1a<-plot(plot1)[[1]] + ggplot2::ylim(0, 0.6)+ggthemes::theme_few()+
  ggthemes::theme_few()+scale_color_viridis_d()+ylab("Germination Likelihood")+xlab("")

  
d4<-filter(d2,taxa!="C.canadensis")
niche4<-brm(count~type*factstrat,data=d4, family=bernoulli(link=logit))
plot2<-conditional_effects(niche4,effects = "type:factstrat")
plot2a<-plot(plot2)[[1]] + ggplot2::ylim(0, 0.6)+ggthemes::theme_few()+scale_color_viridis_d()+
ylab("")+xlab("")

jpeg("..//figure/nichemodfication.jpeg")
ggpubr::ggarrange(plot1a,plot2a,common.legend = TRUE,labels=c("  Honewort","Dames Rocket"))
dev.off()  
