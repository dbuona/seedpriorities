rm(list=ls()) 
options(stringsAsFactors = FALSE)
graphics.off()
setwd("~/Documents/git/seedpriorities/pilot/")
d<-read.csv(file = "germination_data_sheet.csv",header = TRUE)

library(dplyr)
library(tidyr)
library(lubridate)

colnames(d)
d<-select(d,1:45)
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
"01/08/2020" , "01/09/2020",  "01/10/2020" ,"01/12/2020" ,"01/13/2020")
d<-filter(d,strat==6)
d<-gather(d,date, germ,13:45)

d$date2<-as.Date(d$date, format="%m / %d / %Y")
unique(d$date2)


#Germ perc
d.sum<-d %>% group_by(ID,comp_type,taxa,inc,pot_type) %>% tally(germ)
d.sum<-filter(d.sum, taxa!="A.petiolaria")
d.sum$tot_seed<-ifelse(d.sum$comp_type=="intra",20,10)
d.sum$germ_perc<-d.sum$n/d.sum$tot_seed

d.sum<-filter(d.sum, !germ_perc==0)
###germ perc
d.perc<-d.sum %>% group_by(comp_type,taxa,inc) %>%dplyr::summarise(mean_germ=mean(germ_perc),sd_germ=sd(germ_perc))

#MGT have to change this to accout for the new year

d$day<-yday(d$date2)
start<-yday("2019-12-06")

d$doe<-ifelse(grepl("2019",d$date2),d$day-start,25+(d$day))


d.time<-filter(d,!is.na(germ))
d.time<-d.time %>% group_by(ID,comp_type,taxa,inc,pot_type,doe) %>% count(germ)
d.time$tot_seed<-ifelse(d.time$comp_type=="intra",20,10)

plates<-unique(d.time$ID)
df<-data.frame(ID=character(),inc=character(),taxa=character(),comp_type=character(), MGT=numeric())

for (p in seq_along(plates)){
  dataoneplate <- subset(d.time, ID==plates[p])
  MGT<-(sum(dataoneplate$doe*dataoneplate$n)/sum(dataoneplate$n))
  dfhere <- data.frame(ID=plates[p],inc=dataoneplate$inc, taxa=dataoneplate$taxa,comp_type=dataoneplate$comp_type,MGT=MGT)
  df <- rbind(df, dfhere) ## rbind it here for safty
}

df<-df %>% distinct()
df2<-left_join(df,d.sum)

comps<-df2 %>% group_by(taxa,inc) %>%dplyr::summarise(mean_gp=mean(germ_perc),sd_gp=sd(germ_perc),mean_MGT=mean(MGT),sd_MGT=sd(MGT))
