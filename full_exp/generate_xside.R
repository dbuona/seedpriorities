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

d<-read.csv("full_data_sheet.csv")
bio<-read.csv("biomass.csv")
table(d$taxa)
d<-filter(d,pot_type=="Cc.Hm")


sapply(d, class)
cols.num <- 14:40
d[cols.num] <- sapply(d[cols.num],as.numeric)


d<-d%>% pivot_longer(14:40,names_to="date",values_to = "count")
#d<-d %>% tidyr::gather(date,count,14:40) depreciated



d$count<-ifelse(is.na(d$count),0,d$count)
#d$count<-ifelse(is.na(d$count),0,d$count)
#d$count<-ifelse(d$count=="x",1,d$count)
#d$count<-ifelse(d$count==306,1,d$count)
print(xtable(table(d$taxa,d$count)))



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

dens<-germies %>% dplyr::group_by(potnumber,strat,sp) %>% dplyr::count()
time<-germies %>% dplyr::group_by(potnumber,strat,sp) %>% dplyr::summarize(MGT=mean(doe))

###separate by species
CC1<-dplyr::filter(dens,sp=="Cryp")
CC2<-dplyr::filter(time,sp=="Cryp")
CC3<-dplyr::filter(bio,sp=="Cryp")

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

goo$type<-ifelse(!is.na(goo$n_Cc)& !is.na(goo$n_Hm),"competition","single species") 
if (FALSE){ 
crypplot1<-ggplot(goo,aes(n_Cc,Cc_percap))+geom_point(aes(color=type))+geom_smooth(method="lm",aes(color=type))
crypplot2<-ggplot(goo,aes(n_Hm,Cc_percap))+geom_point(aes(color=type))+geom_smooth(method="lm",aes(color=type))+ylim(0,200)
crypplot3<-ggplot(goo,aes(priority,Cc_percap))+geom_point(aes(color=type))+geom_smooth(method="lm",aes(color=type))+ylim(0,200)

png("..//figure/cryp_plots.png")
ggpubr::ggarrange(crypplot1,crypplot2,crypplot3, nrow=1,ncol=3,common.legend = TRUE)
dev.off()


hesp1<-ggplot(goo,aes(n_Hm,Hm_percap))+geom_point(aes(color=type))+geom_smooth(method="lm",aes(color=type))
hesp2<-ggplot(goo,aes(n_Cc,Hm_percap))+geom_point(aes(color=type))+geom_smooth(method="lm",aes(color=type))
hesp3<-ggplot(goo,aes(priority,Hm_percap))+geom_point(aes(color=type))+geom_smooth(method="lm",aes(color=type))
png("..//figure/hesp_plots.png")
ggpubr::ggarrange(hesp1,hesp2,hesp3, nrow=1,ncol=3,common.legend=TRUE)
dev.off()
}
goo$n_Cc<-ifelse(is.na(goo$n_Cc),0,goo$n_Cc)
goo$n_Hm<-ifelse(is.na(goo$n_Hm),0,goo$n_Hm)


write.csv(goo,"data_4_invasive.csv")
#mod.cc<-brms::brm(Cc_percap~n_Cc+n_Hm+priority,data=goo)
#mod.hm<-brms::brm(Hm_percap~n_Cc+n_Hm+priority,data=goo)

brms::fixef(mod.cc)
brms::fixef(mod.hm)

#summary(mod)
#brms::pp_check(mod,nsamples=100)



## I feel like I need a dummy varaible for priority
#tryit
goo$priority2<-ifelse(is.na(goo$priority),0,goo$priority)

mod.cc2<-brms::brm(Cc_percap~n_Cc+n_Hm+priority2,data=goo)
mod.hm2<-brms::brm(Hm_percap~n_Cc+n_Hm+priority2,data=goo)

if (FALSE){
effect<-c("beta_ch","beta_hc","beta_ch","beta_hc")
beta<-c(brms::fixef(mod.cc2)[3]/brms::fixef(mod.cc2)[2], #intra higher than inter
brms::fixef(mod.hm2)[2]/brms::fixef(mod.hm2)[3],
(brms::fixef(mod.cc2)[3]+brms::fixef(mod.cc2)[4])/brms::fixef(mod.cc2)[2] ,
(brms::fixef(mod.hm2)[2]+brms::fixef(mod.hm2)[4])/brms::fixef(mod.hm2)[3]) #intra higher than inter
priority<-c("none","none","one day","one day")
outcome<-c("coexist","coexist","H. matronalis","H. matronis")
data.frame(effect,beta,priority,outcome)
}
#Depending on K they should coexist
library(tidybayes)
output.cc<-mod.cc2 %>%
  spread_draws(b_n_Cc,b_n_Hm ,b_priority2)

output.hm<-mod.hm2 %>%
  spread_draws(b_n_Cc,b_n_Hm ,b_priority2)

output.cc$beta<-output.cc$b_n_Hm/output.cc$b_n_Cc
output.cc$beta_priority<-(output.cc$b_n_Hm+output.cc$b_priority2)/output.cc$b_n_Cc

output.hm$beta<-output.hm$b_n_Cc/output.hm$b_n_Hm
output.hm$beta_priority<-(output.hm$b_n_Cc+output.hm$b_priority2)/output.hm$b_n_Hm



colnames(output.cc)
output.cc <-output.cc %>% tidyr::gather("beta","Estimate",7,8)
output.hm <-output.hm %>% tidyr::gather("beta","Estimate",7,8)
output.cc$species<-c("Competition coefficient \non Honewort")
output.hm$species<-c("Competition coeficient \non Dames Rocket")
outputbetas<-rbind(output.cc,output.hm)

jpeg("..//figure/comp_coefficient.jpeg",width = 6,height=4, units="in",res=300)
ggplot(outputbetas,aes(y = beta, x = Estimate))+
  stat_halfeye(aes(color=species,fill=species),alpha=0.6)+ggthemes::theme_few()+scale_fill_viridis_d(begin=0,end=0.5)+
  scale_color_viridis_d(begin=0,end=0.5)+geom_vline(xintercept =1)+xlim(-1,5)+
  scale_y_discrete(limits=c("beta_priority","beta"),labels=c("betas with \npriority effect","betas no \npriority effect"))
dev.off()


#goo$priority_cent<-goo$priority2-mean(goo$priority2)

summary(lm(Cc_percap~n_Cc+n_Hm+priority2,data=goo))
summary(lm(Hm_percap~n_Cc+n_Hm+priority2,data=goo)) ##
summary(lm(MG_Cc~n_Cc+n_Hm+priority2,data=goo)) 
summary(lm(MG_Hm~n_Cc+n_Hm+priority2,data=goo))


#png("..//figure/priority_treat.png")


ggplot(data=goo,aes(as.factor(strat),priority))+geom_boxplot()+geom_point(alpha=0.5,color="hotpink")


car::Anova(lm(priority~as.factor(strat),data=goo),type=3)


#goo$MG_Cc<-ifelse(is.na(goo$MG_Cc),.0001,goo$MG_Cc)
#goo$MG_Hm<-ifelse(is.na(goo$MG_Hm),.0001,goo$MG_Hm)

###calcuate starting seed mass
inits<-read.csv("full_data_sheet.csv")
inits<-filter(inits,pot_type=="Cc.Hm")
ini.dens<-inits %>% dplyr::group_by(potnumber,taxa,strat) %>% dplyr::count()


#Cc 2.109 g/ 1000 seed _> 2.109 MG /seed
#Hm 1.9432
ini.dens$start<-NA
ini.dens$start[ini.dens$taxa=="C.canadensis"]<-ini.dens$n*2.109
ini.dens$start[ini.dens$taxa=="H.matronalis"]<-ini.dens$n*1.9432
####not sure the error here, it seems like above did it right

ni.dens<-dplyr::select(ini.dens,taxa,potnumber,start)

ini.dens<-tidyr::spread(ini.dens, taxa,start)
colnames(ini.dens)[c(3,4)]<-c("startCc","startHm")

goo<-dplyr::left_join(goo,ini.dens)

#goo$startCc<-ifelse(is.na(goo$startCc),.0001,goo$startCc)
#goo$startHm<-ifelse(is.na(goo$startHm),.0001,goo$startHm)
###TBC
goo$RGRD<-(log(goo$MG_Hm/goo$startHm)-log(goo$MG_Cc/goo$startCc))
#hm is species 2
7*11


coef(lm(RGRD~n_Cc+n_Hm+priority,data=goo)) 
coef(lm(RGRD~n_Cc+n_Hm+strat,data=goo)) 
coef(lm(RGRD~n_Cc+n_Hm+as.factor(strat),data=goo)) 

mod1<-brms::brm(RGRD~n_Cc+n_Hm+priority,data=goo)

mod2<-brms::brm(RGRD~n_Cc+n_Hm+strat,data=goo)
#gooout<-as.data.frame(brms::fixef(mod1))
#gooout$factor<-rownames(gooout)
#gooout<-dplyr::filter(gooout,factor!="Intercept")

library(bayesplot)
library(tidybayes)
get_variables(mod1)
output<-mod1 %>%
  spread_draws(b_n_Cc,b_n_Hm ,b_priority)
colnames(output)
output <-output %>% tidyr::gather("var","Estimate",4:6)
output$winner<-ifelse(output$var=="b_n_Cc","C. canadensis","H. matronalis")

plot1<-ggplot(output,aes(y = var, x = Estimate)) +
  stat_halfeye(aes(fill=winner),alpha=0.6)+ggthemes::theme_few()+
  geom_vline(xintercept=0,linetype="dashed")+
  scale_y_discrete(limits=c("b_priority","b_n_Cc","b_n_Hm"),labels=c("priority effect", expression(paste("Density ", italic(" C. canadensis"))),expression(paste("Density", italic(" H. matronalis")))))+
  scale_fill_viridis_d(begin=0,end=.5)+theme(legend.position = "none")+ylab("")

#png("..//figure/RGRD_muplot.png",width=6,height=6, units = "in",res=200)
#ggplot(gooout,aes(Estimate,factor))+geom_point(size=3)+geom_errorbarh(aes(xmin=Q2.5,xmax=Q97.5),height=0)+
 # geom_vline(xintercept=0,color="red",linetype="dotted")+theme_linedraw()

n_Hm<-rep(c(3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10,10.5),16)
n_Cc<-rep(c(3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10,10.5),each=16)
priority<-rep(c(-1,-0.5,0,.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8),each=256)

#n_Hm<-rep(seq(3,10,by=0.1),71)
#n_Cc<-rep(seq(3,10,by=0.1),each=71)
#priority<-rep(seq(-1,6,by=0.1),each=71)
16^2

newdater<-data.frame(n_Hm,n_Cc,priority)
uber<-fitted(mod1,newdata = newdater,probs = c(.25,.75))

uber<-cbind(uber,newdater)

#pp_check(mod1,ndraws = 100)
library("scatterplot3d")
library("plot3D")
library("plotly")
library(rgl)
uber$winner<-ifelse(uber$Estimate<0,"C. canadensis","H. matronalis")
uber<-as.data.frame(uber)

shapes<-c(15,15)
colors<-c("#440154FF","#1F968BFF")
jpeg("..//figure/threedpred.jpeg",width = 8,height=6, units="in",res=300)
scatterplot3d(uber$n_Cc,uber$n_Hm,  uber$priority,box = FALSE,grid=TRUE,angle=200,pch=shapes[as.factor(uber$winner)],color=colors[as.factor(uber$winner)],xlab='C. canadensis',ylab="H. matronalis", zlab='Advantage')
dev.off()
#with(uber, scatterplot3d(n_Hm, n_Cc, priority, pch = 19,angle=65))

jpeg("..//figure/mu_plots.jpeg",width = 6,height=6, units="in",res=300)
plot1
dev.off()



 
fig <- plot_ly(data=uber, x = ~n_Hm, y = ~n_Cc, z = ~priority,color=uber$winner,colors=c("#440154FF","#1F968BFF"),alpha=0.95,symbol = ~winner, symbols = c('o','o'))
 fig <- fig %>% add_markers()
 
 fig <- fig %>% layout(scene = list(xaxis = list(title = 'H. matronalis'),
                                    yaxis = list(title = 'C. canadensis'),
                                    zaxis = list(title = 'Advantage')))
 fig
 
 
 
 library(plotly)
 
 fig <- plot_ly(type = 'mesh3d',
                x = c(0, 0, 1, 1, 0, 0, 1, 1),
                y = c(0, 1, 1, 0, 0, 1, 1, 0),
                z = c(0, 0, 0, 0, 1, 1, 1, 1),
                i = c(7, 0, 0, 0, 4, 4, 6, 6, 4, 0, 3, 2),
                j = c(3, 4, 1, 2, 5, 6, 5, 2, 0, 1, 6, 3),
                k = c(0, 7, 2, 3, 6, 7, 1, 1, 5, 5, 7, 6)
                )
 )
 
 fig
 
 
 devtools::install_github("AckerDWM/gg3D")
 
 library("gg3D")
 
ggplot(uber, aes(x=n_Hm, y=n_Cc, z=priority, color=winner)) + 
   theme_void() +
  labs_3D()+
   axes_3D() +
   stat_3D()+scale_color_viridis_d(option="inferno")
 
##simple predictiosn
summary(lm(RGRD~n_Cc+n_Hm+priority2,data=goo)) 

fit1 <- brms::brm(mvbind(MG_Hm, MG_Cc) ~ startCc+startHm+priority2,data=goo)
summary(fit1)

fit2 <- brms::brm(mvbind(Hm_percap, Cc_percap) ~ n_Cc+n_Hm+priority2,data=goo)
summary(fit2)


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