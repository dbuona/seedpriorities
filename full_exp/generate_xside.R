rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()

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

time<-germies %>% group_by(potnumber,sp,strat) %>% summarize(MGT=mean(doe))

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
  
ggplot(goo,aes(n_Cc,Cc_percap))+geom_point()+geom_smooth(method="lm")
ggplot(goo,aes(n_Hm,Cc_percap))+geom_point()+geom_smooth(method="lm")

ggplot(goo,aes(n_Hm,Hm_percap))+geom_point()+geom_smooth(method="lm")
ggplot(goo,aes(n_Cc,Hm_percap))+geom_point()+geom_smooth(method="lm")


ggplot(goo,aes(priority,Cc_percap))+geom_point()+geom_smooth(method="lm")+ylim(0,300)

ggplot(goo,aes(priority,Hm_percap))+geom_point()+geom_smooth(method="lm")

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
ggplot(data=goo,aes(as.factor(strat),priority))+geom_boxplot()+geom_point(alpha=0.5,color="pink")

car::Anova(lm(priority~as.factor(strat),data=goo),type=3)

summary(lm(Cc_percap~n_Cc+n_Hm+strat,data=goo)) ## this only includes rows with priority 
summary(lm(Hm_percap~n_Cc+n_Hm+strat,data=goo)) 



summary(lm(MG_Cc~n_Cc*n_Hm+priority,data=goo)) ## this only includes rows with priority 
summary(lm(MG_Hm~n_Cc*n_Hm+priority,data=goo)) ##


goo$MG_Cc<-ifelse(is.na(goo$MG_Cc),1,goo$MG_Cc)
goo$MG_Hm<-ifelse(is.na(goo$MG_Hm),1,goo$MG_Hm)
goo$RGRD<-(log(goo$MG_Hm)-log(goo$MG_Cc))
7*11

summary(lm(RGRD~n_Cc+n_Hm+priority2,data=goo)) 
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