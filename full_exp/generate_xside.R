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

d<-filter(d,pot_type=="Cc.Hm")


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

dens<-germies %>% group_by(potnumber,taxa) %>% count()

time<-germies %>% group_by(potnumber,taxa) %>% summarize(MGT=mean(doe))
time<-tidyr::spread(time,taxa,MGT)
time$priority<-time$C.canadensis-time$H.matronalis

colnames(time)[2:3]<-c("MGTcc","MGThm")

#time<-dplyr::select(time,potnumber,priority)


dens<-tidyr::spread(dens,taxa,n)
dens$interaction<-ifelse(!is.na(dens$C.canadensis) &!is.na(dens$H.matronalis),"inter","intra")

dat<-left_join(dens,time)

ggplot(dat,aes(priority))+geom_histogram(color="blue",fill="lightblue")+theme_bw()
quantile(dat$priority,na.rm=TRUE)

dat$C.canadensis<-ifelse(is.na(dat$C.canadensis),0,dat$C.canadensis)
dat$H.matronalis<-ifelse(is.na(dat$H.matronalis),0,dat$H.matronalis)
### more plonts

ggplot()+
geom_histogram(dat=dat,aes(H.matronalis),color="firebrick1",fill="hotpink1",bins=20,alpha=0.8)+
  geom_histogram(data=dat,aes(C.canadensis),color="darkgreen",fill="green",bins=20,alpha=.4)+theme_bw()

ggplot()+
  geom_histogram(dat=dat,aes(MGThm),color="firebrick1",fill="hotpink1",bins=50,alpha=0.8)+
  geom_histogram(data=dat,aes(MGTcc),color="darkgreen",fill="green",bins=50,alpha=.4)+theme_bw()

# Biomass~condens*heterodens (int is overall density effect) + conMGT*heteroMGT (int is priority effect)

inter<-filter(dat,interaction=="inter")
intra<-filter(dat,interaction=="intra")

### fake data
