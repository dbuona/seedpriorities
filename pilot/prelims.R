rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()
setwd("~/Documents/git/seedpriorities/pilot/")
d<-read.csv(file = "germination_data_sheet.csv",header = TRUE)

library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

colnames(d)
d<-select(d,1:58)
colnames(d)<- c("ID","strat",  "inc" ,"replicate",
 "comp_type",   "pot_type",    "rowcol"  ,    "row"  ,      
 "col",       "seed",    "taxa",    "potnumber",
 "12/4/2019" , "12/6/2019" , "12/8/2019" , "12/9/2019",
"12/10/2019" ,"12/11/2019", "12/12/2019" ,"12/13/2019",
"12/15/2019", "12/16/2019" ,"12/17/2019", "12/18/2019",
"12/19/2019", "12/20/2019", "12/22/2019" ,"12/23/2019",
"12/24/2019","12/25/2019" ,"12/26/2019","12/27/2019",
"12/29/2019","12/30/2019" , "12/31/2019",
"01/01/2020" , "01/03/2020"  ,"01/05/2020",  "01/06/2020",  "01/07/2020" ,
"01/08/2020" , "01/09/2020",  "01/10/2020" ,"01/12/2020" ,"01/13/2020","01/14/2020","01/15/2020",
"01/16/2020","01/17/2020","01/19/2020","01/20/2020","01/21/2020","01/22/2020","01/23/2020","01/24/2020","01/26/2020",
"01/27/2020","01/29/2020")

d<-gather(d,date, germ,13:58)

d$date2<-as.Date(d$date, format="%m / %d / %Y")
unique(d$date2)


#Germ perc
d.sum<-d %>% group_by(ID,comp_type,strat,taxa,inc,pot_type) %>% tally(germ)
d.sum<-filter(d.sum, taxa!="A.petiolaria")
d.sum$tot_seed<-ifelse(d.sum$comp_type=="intra",20,10)
d.sum$germ_perc<-d.sum$n/d.sum$tot_seed

d.sum<-filter(d.sum, !germ_perc==0)
###germ perc
d.sum50<-filter(d.sum,germ_perc>=.50)

fgp<-d.sum50 %>% group_by(taxa,strat,inc) %>% summarise(meanGP=mean(germ_perc),sdGP=sd(germ_perc))

fgp<-filter(fgp,taxa!="E.macrophylla")
colnames(fgp)<-c("taxa"  , "chillweeks" , "force"   , "meanGP", "sdGP" )


#MGT have to change this to accout for the new year

d$day<-yday(d$date2)
start<-yday("2019-12-06")
start2<-yday("2020-01-03")

d.6<-filter(d,strat==6)
d.6$doe<-ifelse(grepl("2019",d.6$date2),d.6$day-start,25+(d.6$day))
d.6<-filter(d.6,!is.na(germ))







#####10 weeks
d.10<-filter(d,strat==10)
d.10<-filter(d.10, !is.na(germ))
d.10$doe<-d.10$day-start2
d.10$doe<-ifelse(d.10$day>=20,0,d.10$doe)


colnames(d.10)
colnames(d.6)

time.data<-rbind(d.10,d.6)


time.dat<-unite(time.data,uneek,ID,taxa, sep = "_", remove = FALSE)
d.sum50<-unite(d.sum50,uneek,ID,taxa, sep = "_", remove = FALSE)
plates<-unique(d.sum50$uneek)

df<-data.frame(uneek=numeric(),ID=numeric(),Taxa=character(),force=numeric(),chillweeks=numeric(),T50=numeric())

for (p in seq_along(plates)){
  dataoneplate <- subset(time.dat, uneek==plates[p])
  t50 <- sum(dataoneplate$doe*dataoneplate$germ)/ifelse(dataoneplate$comp_type=="intra",20,10)
  dfhere<-data.frame(uneek=plates[p],ID=dataoneplate$ID,taxa=dataoneplate$taxa,force=dataoneplate$inc, chillweeks=dataoneplate$strat,T50=t50)
  df <- rbind(df, dfhere)
  }

df<-df %>% distinct()
df<-filter(df,taxa!="E.macrophylla")
ggplot(df,aes(chillweeks,T50))+stat_summary(aes(color=taxa))+facet_wrap(~force)




df2<-data.frame(uneek=numeric(),ID=numeric(),Taxa=character(),force=numeric(),chillweeks=numeric(),mgt=numeric())

for (p in seq_along(plates)){
  dataoneplate <- subset(time.dat, uneek==plates[p])
  mgt <- sum(dataoneplate$doe*dataoneplate$germ)/sum(dataoneplate$germ)
  dfhere<-data.frame(uneek=plates[p],ID=dataoneplate$ID,taxa=dataoneplate$taxa,force=dataoneplate$inc, chillweeks=dataoneplate$strat,mgt=mgt)
  df2 <- rbind(df2, dfhere)
}

df2<-df2 %>% distinct()

df2<-filter(df2,taxa!="E.macrophylla")
df<-filter(df,taxa!="E.macrophylla")

ggplot()+stat_summary(data=df2,aes(as.factor(chillweeks),mgt,color=taxa),shape=1)+facet_wrap(~force)+
stat_summary(data=df,aes(as.factor(chillweeks),T50,color=taxa))+facet_wrap(~force)+ggthemes::theme_base()

ggplot()+stat_summary(data=df,aes(as.factor(chillweeks),T50,color=taxa))+facet_wrap(~force)+ggthemes::theme_base()

ggplot(df,aes(chillweeks,T50))+geom_smooth(method=lm, aes(color=taxa,linetype=force))


recap<-df %>% group_by(taxa,chillweeks,force) %>% summarise(meant50=mean(T50),sdT50=sd(T50))
recap2<-df2 %>% group_by(taxa,chillweeks,force) %>% summarise(meanMGT=mean(mgt),sdMGT=sd(mgt))
cap<-left_join(recap,recap2)
cap<-left_join(cap,fgp)


comps10<-left_join(perc.10,mgt10)

comps<-rbind(comps6,comps10)



gooper<-filter(comps,pot_type %in% c("Cc.Hm"))
gooper2<-filter(comps,pot_type %in% c("Cc","Hm"))


ccpv<-filter(comps10,pot_type %in% c("Cc.Pv"))
a<-ggplot(ccpv,(aes(inc,mean_germ)))+stat_summary(aes(color=taxa))
b<-ggplot(ccpv,(aes(inc,MGT)))+stat_summary(aes(color=taxa))

ggpubr::ggarrange(a,b)
