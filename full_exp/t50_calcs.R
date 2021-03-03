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

table(d$taxa)


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
d.small<-filter(d,count==1)


table(d.small$taxa,d.small$strat)
d.small$count<-as.numeric(d.small$count)
d.small %>% group_by(pot_type,strat,taxa) %>% tally(count)
d.small %>% group_by(pot_type,strat,harvest,taxa) %>% tally(count)


good<-filter(d.small,pot_type=="Cc.Hm")
good %>% group_by(strat,taxa,harvest) %>% tally(count)


unique(d$date)
d$date<-d$date %>% str_replace(".*X", "")
unique(d$date)

d$date2<-as.Date(d$date, format="%m . %d . %y")

unique(d$date2)

d$day<-yday(d$date2)
start<-yday("2020-12-24")
d$doe<-ifelse(grepl("2020",d$date2),d$day-start,7+d$day)

d$doe<-ifelse(d$doe==0,0.00001,d$doe)
d$count<-ifelse(d$count==1,1,0)
table(d$count)
d$censored<-ifelse(d$count==1,0,1)

####descritive stats plots for germ perc
#Q1 what do the overall densities look like?
d.small2<-filter(d,count==1)

d.small2<-filter(d.small2, pot_type=="Cc.Hm")
d.small2 %>% group_by(taxa, strat) %>% summarise(mean.time=mean(doe,na.rm=TRUE),sd.time=sd(doe,na.rm=TRUE))
(7.9-2.94)-
(5.33-2.06)




stop("not an error, below is scratch")

bag<-d.small %>% group_by(taxa,strat,pot_type,config,rep,density,harvest) %>% tally(count)

d.sum<-d %>% group_by(taxa,strat,pot_type,config,rep,density,harvest) %>% tally(count)
ggplot(d.sum,aes(n))+geom_histogram(bins=24)+facet_wrap(~harvest)

d.sum%>% group_by(pot_type,strat,harvest) %>% summarise(sum=sum(n))
d.sum%>% group_by(taxa,strat) %>% summarise(sum=sum(n)) ## stillb ad for temporal

# ger m %
d.small<-filter(d,pot_type=="Cc.Hm")
d.small<-filter(d.small,count==1)
d.sum<-d.small %>% group_by(strat,taxa,pot_type,config,density,rep,harvest,ID) %>% tally(count)

d.sum$planted<-NA

d.sum$planted[which(d.sum$config %in% c("monoA","monoB") & d.sum$density=="high")]<-14
d.sum$planted[which(d.sum$config %in% c("monoA","monoB") & d.sum$density=="low")]<-8

d.sum$planted[which(d.sum$config %in% c("even") & d.sum$density=="high" )]<-7
d.sum$planted[which(d.sum$config %in% c("even") & d.sum$density=="low" )]<-4

d.sum$planted[which(d.sum$config %in% c("quarterA") & d.sum$density=="high" & d.sum$taxa=="C.canadensis")]<-4
d.sum$planted[which(d.sum$config %in% c("quarterA") & d.sum$density=="low" & d.sum$taxa=="C.canadensis")]<-2
d.sum$planted[which(d.sum$config %in% c("quarterA") & d.sum$density=="high" & d.sum$taxa=="H.matronalis")]<-10
d.sum$planted[which(d.sum$config %in% c("quarterA") & d.sum$density=="low" & d.sum$taxa=="H.matronalis")]<-6

d.sum$planted[which(d.sum$config %in% c("quarterB") & d.sum$density=="high" & d.sum$taxa=="H.matronalis")]<-4
d.sum$planted[which(d.sum$config %in% c("quarterB") & d.sum$density=="low" & d.sum$taxa=="H.matronalis")]<-2
d.sum$planted[which(d.sum$config %in% c("quarterB") & d.sum$density=="high" & d.sum$taxa=="C.canadensis")]<-10
d.sum$planted[which(d.sum$config %in% c("quarterB") & d.sum$density=="low" & d.sum$taxa=="C.canadensis")]<-6

d.sum$germ_perc<-d.sum$n/d.sum$planted
d.sum50<-filter(d.sum,germ_perc>=.50)
d.time<-d.small %>% group_by(taxa,ID) %>% dplyr::summarise(doe=mean(doe))
d.sum50<-left_join(d.sum50,d.time)

ggplot(d.sum.high,aes(config,germ_perc))+stat_summary(aes(color=taxa))



d.sum50$uneek<-paste(d.sum50$ID,d.sum50$taxa)
plates<-unique(d.sum50$uneek)
df<-data.frame(ID=character(),Taxa=character(),strat=numeric(),config=character(),potnum=character(),T50=numeric())

for (p in seq_along(plates)){
  dataoneplate <- subset(d.sum50, uneek==plates[p])
  t50 <- sum(dataoneplate$doe*dataoneplate$n)/(dataoneplate$planted)
  dfhere<-data.frame(ID=plates[p],taxa=dataoneplate$taxa,strat=dataoneplate$strat,config=dataoneplate$config,potnum=dataoneplate$ID,T50=t50)
  df <- rbind(df, dfhere)

}
df<-dplyr::select(df, -ID)
df<-tidyr::spread(df,taxa,T50)


df$diff<-df$C.canadensis-df$H.matronalis
df$diff<-ifelse(df$config=="monoA",df$C.canadensis,df$diff)
df$diff<-ifelse(df$config=="monoB",df$H.matronalis,df$diff)

df<-filter(df, !is.na(diff))

df %>% group_by(strat) %>%summarise(meancc=mean(C.canadensis,na.rm=TRUE),meanhm=mean(H.matronalis,na.rm=TRUE))



priorz.wei<-get_prior(doe | cens(censored)~strat*taxa,data=d.small,family= weibull())

fit.wei<- brm(doe | cens(censored)~strat*taxa,data=d.small,family= weibull(),inits=0 ,prior=priorz.wei, chains=2) 

new.data<-data.frame(taxa=rep(c("C.canadensis","H.matronalis"),2),strat=rep(c(6,10),each=2))
out<-as.data.frame(fitted(fit.wei,newdata = new.data,summary = TRUE))



d.germ<-d %>% filter(count==1)
d.germ<-filter(d.germ,pot_type=="Cc.Hm")
goo<-d.germ %>% dplyr::group_by(config,strat,taxa) %>% summarise(meandoe=mean(doe,na.rm=TRUE))

##############################

d<-d %>% filter(!is.na(count))
d$count<-as.numeric(d$count)
d<-d %>% filter(count==1)
d %>% group_by(strat,pot_type,config,taxa) %>% tally()

