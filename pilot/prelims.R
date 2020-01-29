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
d<-select(d,1:56)
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
"01/16/2020","01/17/2020","01/19/2020","01/20/2020","01/21/2020","01/22/2020","01/23/2020","01/24/2020","01/26/2020")

d<-gather(d,date, germ,13:56)

d$date2<-as.Date(d$date, format="%m / %d / %Y")
unique(d$date2)


#Germ perc
d.sum<-d %>% group_by(ID,comp_type,strat,taxa,inc,pot_type) %>% tally(germ)
d.sum<-filter(d.sum, taxa!="A.petiolaria")
d.sum$tot_seed<-ifelse(d.sum$comp_type=="intra",20,10)
d.sum$germ_perc<-d.sum$n/d.sum$tot_seed

d.sum<-filter(d.sum, !germ_perc==0)
###germ perc
d.perc<-d.sum %>% group_by(strat,taxa,inc,pot_type) %>%dplyr::summarise(mean_germ=mean(germ_perc),sd_germ=sd(germ_perc))
perc.6<-filter(d.perc,strat==6)
perc.10<-filter(d.perc,strat==10)
#MGT have to change this to accout for the new year

d$day<-yday(d$date2)
start<-yday("2019-12-06")
start2<-yday("2020-01-03")

d.6<-filter(d,strat==6)
d.6$doe<-ifelse(grepl("2019",d.6$date2),d.6$day-start,25+(d.6$day))


d.time.6<-filter(d.6,!is.na(germ))
d.time.6<-d.time.6 %>% group_by(comp_type,taxa,inc,pot_type,doe) %>% count(germ)
d.time.6$tot_seed<-ifelse(d.time.6$comp_type=="intra",20,10)

mgt6<-d.time.6 %>% group_by(taxa,inc,pot_type) %>%summarise(MGT=mean(doe), sdGT=sd(doe))


comps6<-left_join(perc.6,mgt6)



#####10 weeks
d.10<-filter(d,strat==10)
d.10<-filter(d.10, !is.na(germ))
d.10$doe<-d.10$day-start2
d.10$doe<-ifelse(d.10$day>=20,0,d.10$doe)

d.10<-filter(d.10,!is.na(germ))

d.10$tot_seed<-ifelse(d.10$comp_type=="intra",20,10)
mgt10<-d.10 %>% group_by(taxa,inc,pot_type) %>%summarise(MGT=mean(doe), sdGT=sd(doe))



comps10<-left_join(perc.10,mgt10)

comps<-rbind(comps6,comps10)



gooper<-filter(comps,pot_type %in% c("Cc.Hm"))
gooper2<-filter(comps,pot_type %in% c("Cc","Hm"))


ccpv<-filter(comps10,pot_type %in% c("Cc.Pv"))
a<-ggplot(ccpv,(aes(inc,mean_germ)))+stat_summary(aes(color=taxa))
b<-ggplot(ccpv,(aes(inc,MGT)))+stat_summary(aes(color=taxa))

ggpubr::ggarrange(a,b)
