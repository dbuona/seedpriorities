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
d<-read.csv("full_data_sheet.csv")
bio<-read.csv("biomass.csv")
table(d$taxa)

table(bio$potnumber)

colnames(d)
d<-tidyr::gather(d,date,count,14:40)
colnames(d)
d$count<-d$count



d$count<-ifelse(is.na(d$count),0,d$count)
d$count<-ifelse(is.na(d$count),0,d$count)
d$count<-ifelse(d$count=="x",1,d$count)
d$count<-ifelse(d$count==306,1,d$count)
print(xtable(table(d$taxa,d$count)))

table(d$taxa,d$count)
table(d$taxa)

d<-filter(d,pot_type=="Cc.Hm")
d$sp<-ifelse(d$taxa=="C.canadensis","Cryp","Hesp")
colnames(bio)[2]<-"potnumber"




unique(d$date)
d$date<-d$date %>% str_replace(".*X", "")
unique(d$date)

d$date2<-as.Date(d$date, format="%m . %d . %y")

unique(d$date2)

d$day<-yday(d$date2)
start<-yday("2020-12-24")
d$doe<-ifelse(grepl("2020",d$date2),d$day-start,7+d$day)

d$doe<-ifelse(d$doe==0,0.00001,d$doe)


germies<-filter(d,count==1)

dens<-germies %>% group_by(potnumber,sp,strat) %>% count()

time<-germies %>% dplyr::group_by(potnumber,sp,strat) %>% dplyr::summarize(MGT=mean(doe))

###separate by species
CC1<-filter(dens,sp=="Cryp")
CC2<-filter(time,sp=="Cryp")
CC3<-filter(bio,sp=="Cryp")

Cc<-left_join(CC1,CC2)
Cc<-left_join(Cc,CC3)
colnames(Cc)[c(4,5,7)]<-c("n_Cc","MGT_Cc","MG_Cc")

HM1<-filter(dens,sp=="Hesp")
HM2<-filter(time,sp=="Hesp")
HM3<-filter(bio,sp=="Hesp")

Hm<-left_join(HM1,HM2)
Hm<-left_join(Hm,HM3)
colnames(Hm)[c(4,5,7)]<-c("n_Hm","MGT_Hm","MG_Hm")

Hm$sp<-as.numeric(Hm$sp)
Hm<-Hm %>% dplyr::select("potnumber","strat","n_Hm","MGT_Hm","MG_Hm")

Cc$sp<-as.numeric(Cc$sp)
Cc<-Cc %>% dplyr::select("potnumber","strat","n_Cc","MGT_Cc","MG_Cc")

goo<-full_join(Cc,Hm)


goo$priority<-goo$MGT_Cc-goo$MGT_Hm

goo$Cc_percap<-goo$MG_Cc/goo$n_Cc
goo$Hm_percap<-goo$MG_Hm/goo$n_Hm
  
crypplot1<-ggplot(goo,aes(n_Cc,Cc_percap))+geom_point()+geom_smooth(method="lm")
crypplot2<-ggplot(goo,aes(n_Hm,Cc_percap))+geom_point()+geom_smooth(method="lm")+ylim(0,200)
crypplot3<-ggplot(goo,aes(priority,Cc_percap))+geom_point()+geom_smooth(method="lm")+ylim(0,200)

png("..//figure/cryp_plots.png")
ggpubr::ggarrange(crypplot1,crypplot2,crypplot3, nrow=1,ncol=3)
dev.off()


hesp1<-ggplot(goo,aes(n_Hm,Hm_percap))+geom_point()+geom_smooth(method="lm")
hesp2<-ggplot(goo,aes(n_Cc,Hm_percap))+geom_point()+geom_smooth(method="lm")
hesp3<-ggplot(goo,aes(priority,Hm_percap))+geom_point()+geom_smooth(method="lm")
png("..//figure/hesp_plots.png")
ggpubr::ggarrange(hesp1,hesp2,hesp3, nrow=1,ncol=3)
dev.off()

goo$n_Cc<-ifelse(is.na(goo$n_Cc),0,goo$n_Cc)
goo$n_Hm<-ifelse(is.na(goo$n_Hm),0,goo$n_Hm)

#mod<-brms::brm(Cc_percap~n_Cc+n_Hm+priority,data=goo)
summary(mod)
brms::pp_check(mod,nsamples=100)

summary(lm(Cc_percap~n_Cc+n_Hm,data=goo))
summary(lm(Hm_percap~n_Cc+n_Hm,data=goo))


## I feel like I need a dummy varaible for priority
#tryit
goo$priority2<-ifelse(is.na(goo$priority),0,goo$priority)
goo$priority_cent<-goo$priority2-mean(goo$priority2)

summary(lm(Cc_percap~n_Cc+n_Hm+priority_cent,data=goo)) 

summary(lm(Hm_percap~n_Cc+n_Hm+priority_cent,data=goo)) ##



ggplot(goo,aes(priority,Cc_percap))+geom_point()+ylim(0,200)+geom_smooth()

## Do we see a treatmenteffect?
ggplot(goo,aes(strat,MGT_Cc))+geom_point()
ggplot(goo,aes(strat,MGT_Hm))+geom_point()

png("..//figure/priority_treat.png")
ggplot(data=goo,aes(as.factor(strat),priority))+geom_boxplot()+geom_point(alpha=0.5,color="hotpink")
dev.off()

car::Anova(lm(priority~as.factor(strat),data=goo),type=3)

summary(lm(Cc_percap~n_Cc+n_Hm+strat,data=goo)) ## this only includes rows with priority 
summary(lm(Hm_percap~n_Cc+n_Hm+strat,data=goo)) 


summary(lm(Cc_percap~n_Cc+n_Hm*strat,data=goo)) ## this only includes rows with priority 
summary(lm(Hm_percap~n_Cc*strat+n_Hm,data=goo)) 



summary(lm(MG_Cc~n_Cc*n_Hm+priority,data=goo)) ## this only includes rows with priority 
summary(lm(MG_Hm~n_Cc*n_Hm+priority,data=goo)) ##


goo$MG_Cc<-ifelse(is.na(goo$MG_Cc),1,goo$MG_Cc)
goo$MG_Hm<-ifelse(is.na(goo$MG_Hm),1,goo$MG_Hm)

###calcuate starting seed mass
inits<-read.csv("full_data_sheet.csv")
inits<-filter(inits,pot_type=="Cc.Hm")
ini.dens<-inits %>% group_by(potnumber,taxa,strat) %>% count()
ini.dens$Cc_start<-NA

#Cc 2.109 g/ 1000 seed _> 2.109 MG /seed
#Hm 1.9432
ini.dens$start[ini.dens$taxa=="C.canadensis"]<-ini.dens$n*2.109
ini.dens$start[ini.dens$taxa=="H.matronalis"]<-ini.dens$n*1.9432
###TBC
goo$RGRD<-(log(goo$MG_Hm)-log(goo$MG_Cc))
7*11

summary(lm(RGRD~n_Cc+n_Hm+priority2,data=goo)) 

mod1<-brms::brm(RGRD~n_Cc+n_Hm+priority2,data=goo)
gooout<-as.data.frame(brms::fixef(mod1))
gooout$factor<-rownames(gooout)
gooout<-dplyr::filter(gooout,factor!="Intercept")

png("..//figure/RGRD_muplot.png",width=6,height=6, units = "in",res=200)
ggplot(gooout,aes(Estimate,factor))+geom_point(size=3)+geom_errorbarh(aes(xmin=Q2.5,xmax=Q97.5),height=0)+
  geom_vline(xintercept=0,color="red",linetype="dotted")+theme_linedraw()
dev.off()

newdata<-data.frame(n_Cc=c(2,1,2,2),n_Hm= c(1,2,2,2), priority2=c(0,0,1,8))
fitted(mod1,newdata=newdata,probs = c(.25,.75))

#A positive value of bi (for i =1 or 2) means that increasing
#species i in the initial community will enhance the difference
#in average RGRs in favour of species 2 and
#hence will tilt the final composition more towards that
#species. A negative value has the opposite effect. The two
#coefficients b1 and b2 show how changes in initial
#abundance of the species can influence the growth differential
#between species and we call them influence
#coefficients. The influence of a species is the difference
#between its intraspecific effect on its own RGR and its
#interspecific effect on the RGR of the other species. A
#positive value of b3 means that for a particular mixture
#the environmental factor increases the RGRD and hence
#increases the proportion of species 2 in the composition.
#If all three coefficients (b1, b2 and b3) are zero then the
#difference in average RGR is not affected by initial
#composition or applied treatment and will be constant
#(b0).

###
##RGR