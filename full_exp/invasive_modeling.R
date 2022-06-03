
###Clean data comes from generate_xside.R
rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()
options(mc.cores = parallel::detectCores())

setwd("~/Documents/git/seedpriorities/full_exp")
load("invasivemods.Rda")
library(dplyr,quietly = TRUE)
library(xtable,quietly=TRUE)
library(stringr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(tidyselect)
library(reshape2)
library(brms)

d<-read.csv("data_4_invasive.csv")

###1#### competition coefficients

plootI<-ggpubr::ggarrange(ggplot(d,aes(n_Cc,MG_Cc))+geom_point(aes(color=type)),
ggplot(d,aes(n_Hm,MG_Cc))+geom_point(aes(color=type)),
ggplot(d,aes(priority2,MG_Cc))+geom_point(aes(color=type)),common.legend=TRUE,nrow=1)

plootII<-ggpubr::ggarrange(ggplot(d,aes(n_Hm,MG_Hm))+geom_point(aes(color=type)),
ggplot(d,aes(n_Cc,MG_Hm))+geom_point(aes(color=type)),
ggplot(d,aes(priority2,MG_Hm))+geom_point(aes(color=type)),common.legend = TRUE,nrow=1)

jpeg("..//figure/LVraw.jpeg")
ggpubr::ggarrange(plootI,plootII,nrow=2)
dev.off()

jpeg("..//figure/Connoly.jpeg")
ggpubr::ggarrange(ggplot(d,aes(n_Hm,RGRD))+geom_point()+stat_smooth(method="lm"),
ggplot(d,aes(n_Cc,RGRD))+geom_point()+stat_smooth(method="lm"),
ggplot(d,aes(priority,RGRD))+geom_point()+stat_smooth(method="lm"),
ggplot(d,aes(strat,RGRD))+geom_point()+stat_smooth(method="lm"))
dev.off()


d$n_Cc2<-ifelse(d$n_Cc==0,0.00001,d$n_Cc)
d$n_Hm2<-ifelse(d$n_Hm==0,0.00001,d$n_Hm)
d$priorty3<-ifelse(d$priority2==0,0.00001,d$priority2)

mod.cc2<-brms::brm(Cc_percap~n_Cc+n_Hm+priority2,data=d)
mod.hm2<-brms::brm(Hm_percap~n_Cc+n_Hm+priority2,data=d)


mod.cc<-brms::brm(MG_Cc~n_Cc+n_Hm+priority2,data=d)
mod.hm<-brms::brm(MG_Hm~n_Cc+n_Hm+priority2,data=d)

brms::pp_check(mod.cc,ndraws = 100)
jpeg("..//figure/ppchecks.jpeg")
ggpubr::ggarrange(pp_check(mod.cc,ndraws = 100),
pp_check(mod.hm2,ndraws = 100),
pp_check(mod1,ndraws = 100),
pp_check(mod2,ndraws = 100),labels=c("CCpercap","HMpercap","RGRDmgt","RGRDstrat"))
dev.off()
###the problem above is they aren't linear

d.componly<-filter(d,type=="competition")
mod.cc.comp<-brms::brm(Cc_percap~n_Cc+n_Hm+priority2,data=d.componly)
mod.hm.comp<-brms::brm(Hm_percap~n_Cc+n_Hm+priority2,data=d.componly)

fixef(mod.cc)
fixef(mod.hm)
fixef(mod.cc.comp)

fixef(mod.hm2)
fixef(mod.hm.comp)



###deal with the log sclae
d$logn_Cc<-log(d$n_Cc2)
d$logn_Hm<-log(d$n_Hm2)
d$logpriority3<-log(d$priorty3)

ggpubr::ggarrange(ggplot(d,aes(logn_Cc,Cc_percap))+geom_point(),
                  ggplot(d,aes(logn_Hm,Cc_percap))+geom_point(),
                  ggplot(d,aes(logpriority3,Cc_percap))+geom_point())



##1a Make a table of coefficients
xtable(fixef(mod.cc2,probs = c(0.025,.25,.75,.975)))
xtable(fixef(mod.hm2,probs = c(0.025,.25,.75,.975)))


###make a plot of of compeition coeffcient
library(tidybayes)
output.cc<-mod.cc2 %>%
  spread_draws(b_n_Cc,b_n_Hm ,b_priority2)

output.hm<-mod.hm2 %>%
  spread_draws(b_n_Cc,b_n_Hm ,b_priority2)

output.cc$beta<-output.cc$b_n_Hm/output.cc$b_n_Cc
output.cc$beta_priority<-(output.cc$b_n_Hm+output.cc$b_priority2)/output.cc$b_n_Cc

output.hm$beta<-output.hm$b_n_Cc/output.hm$b_n_Hm
output.hm$beta_priority<-(output.hm$b_n_Cc-output.hm$b_priority2)/output.hm$b_n_Hm


##1a Make a table of coefficients
output.cc <-output.cc %>% tidyr::gather("beta","Estimate",7,8)
output.hm <-output.hm %>% tidyr::gather("beta","Estimate",7,8)
output.cc$species<-c("Effects on \non Honewort")
output.hm$species<-c("Effects on \non Dames Rocket")
outputbetas<-rbind(output.cc,output.hm)

xtable(outputbetas %>% group_by(species,beta) %>%summarise(mean(Estimate),sd(Estimate)))

library(bayesplot)
library(tidybayes)
if(FALSE){
jpeg("..//figure/comp_coefficient.jpeg",width = 6,height=4, units="in",res=300)
ggplot(outputbetas,aes(y = beta, x = Estimate))+
  stat_halfeye(aes(color=species,fill=species),alpha=0.6)+ggthemes::theme_few()+scale_fill_viridis_d(begin=0,end=0.5)+
  scale_color_viridis_d(begin=0,end=0.5)+geom_vline(xintercept =1)+
  scale_y_discrete(limits=c("beta_priority","beta"),labels=c("Competition with \npriority effect","Competition no \npriority effect"))
dev.off()
}
### Relative growth rates
###calcuate starting seed mass in order to calculate biomass input
inits<-read.csv("full_data_sheet.csv")
inits<-filter(inits,pot_type=="Cc.Hm")
ini.dens<-inits %>% dplyr::group_by(potnumber,taxa,strat) %>% dplyr::count()


#Cc 2.109 g/ 1000 seed _> 2.109 MG /seed
#Hm 1.9432

init.cc<-filter(ini.dens,taxa=="C.canadensis")
init.cc$start<-init.cc$n*2.109


init.hm<-filter(ini.dens,taxa!="C.canadensis")
init.hm$start<-init.cc$n*1.9432

#ini.dens$start[ini.dens$taxa=="C.canadensis"]<-ini.dens$n*2.109
#ini.dens$start[ini.dens$taxa=="H.matronalis"]<-ini.dens$n*1.9432
####not sure the error here, it seems like above did it right

ini.dens2<-rbind(init.hm,init.cc)
ini.dens2<-dplyr::select(ini.dens2,potnumber,taxa,start)

ini.dens2<-tidyr::spread(ini.dens2, taxa,start)
colnames(ini.dens2)[c(3,4)]<-c("startCc","startHm")

d<-dplyr::left_join(d,ini.dens2)


###RGRD method currently on handles
d$RGRD<-(log(d$MG_Hm/d$startHm)-log(d$MG_Cc/d$startCc))
#hm is species 2

mod1<-brms::brm(RGRD~n_Cc+n_Hm+priority,data=d)
mod2<-brms::brm(RGRD~n_Cc+n_Hm+strat,data=d)

fixef(mod1,probs = c(.05,.95))

xtable(fixef(mod1,probs = c(0.025,.25,.75,0.975)))

get_variables(mod1)
output<-mod1 %>%
  spread_draws(b_n_Cc,b_n_Hm ,b_priority)
colnames(output)
output <-output %>% tidyr::gather("var","Estimate",4:6)
output$winner<-ifelse(output$var=="b_n_Cc","C. canadensis","H. matronalis")
install.packages("ggtext")
plot1<-ggplot(output,aes(y = var, x = Estimate)) +
  stat_halfeye(aes(fill=winner),.width=c(.9),alpha=0.6)+ggthemes::theme_few()+
  geom_vline(xintercept=0,linetype="dashed")+
  scale_y_discrete(limits=c("b_priority","b_n_Cc","b_n_Hm"),labels=c("Priority effect", expression(paste("Influence ", italic(" C. canadensis"))),expression(paste("Influence", italic(" H. matronalis")))))+
  scale_fill_viridis_d(begin=0,end=.5)+theme(legend.position = "none")+ylab("")+xlim(-0.6,0.6)+annotate("text",x=-.15,y=.5,label=expression(paste(italic("C. canadensis") ," favored")))+
  annotate("text",x=.15,y=.5,label=expression(paste(italic("H. matronalis") ," favored")))+
  annotate("segment", x = -0.6, xend = -0.3, y = .5, yend = .5,
           arrow = arrow(ends = "first", length = unit(.2,"cm")))+
  annotate("segment", x = 0.3, xend = 0.6, y = .5, yend = .5,
           arrow = arrow(ends = "last", length = unit(.2,"cm")))

jpeg("..//figure/mu_plots.jpeg",width = 10,height=5, units="in",res=200)
plot1
dev.off()

d.mgt<-dplyr::select(d,MGT_Cc,MGT_Hm,strat)
colnames(d.mgt)[1:2]<-c("C. canandensis","H.matronalis")
d.mgt %>% dplyr::group_by(strat,SPECIES) %>% summarise(meanMGT=mean(MGT,na.rm=TRUE))

d.mgt<-gather(d.mgt,"SPECIES","MGT",1:2)
ggplot(d.mgt,aes(MGT))+geom_density(aes(fill=as.factor(strat),color=SPECIES),alpha=0.6)+scale_fill_grey()+scale_color_viridis_d(begin=0,end=.5)+
  ggthemes::theme_few()+geom_segment(x=2.06,y=1,xend=2.98,yend=1)+geom_segment(x=7.99,y=1.2,xend=5.47,yend=1.2)
7.99-2.98
5.47-2.06
d.pri<-dplyr::select(d,priority,strat)
ggplot(d.pri,aes(priority))+geom_density(aes(fill=as.factor(strat)),alpha=0.7)+#geom_histogram(position = "dodge",aes(fill=as.factor(strat)),alpha=0.6)+
  scale_fill_grey()+
  ggthemes::theme_few()
ggplot(d.pri,aes(as.factor(strat),priority))+geom_boxplot()+ggthemes::theme_few()

gruber<-brm(MGT_Cc~MGT_Hm*as.factor(strat),data=d)
gruberII<-brm(MGT_Cc~type*as.factor(strat),data=d)
gruberIII<-brm(MGT_Cc~as.factor(strat),data=d)
gruberIV<-brm(MGT_Hm~as.factor(strat),data=d)

ruber<-brm(MGT_Cc~MGT_Hm,data=d)

fixef(gruber,probs = c(.25,.75))
fixef(gruberII,probs = c(.25,.75))

fixef(gruberIII,probs = c(.25,.75))
fixef(gruberIV,probs = c(.25,.75))
fixef(ruber,probs = c(.25,.75))
pp_check(gruber,ndraws = 100)

###quick comparison for suppliment
jpeg("..//figure/priority_treat.jpeg")
ggplot(d,aes(as.factor(strat),priority))+geom_boxplot(outlier.shape=NA)+ggthemes::theme_few()+
  xlab("weeks of cold stratification")+ylab("phenological advantage")
dev.off()

save.image("invasivemods.Rda")

