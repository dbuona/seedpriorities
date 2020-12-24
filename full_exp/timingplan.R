rm(list=ls()) 
library(lubridate)


d<-data.frame(Date=c("20/10/12","20/11/09","20/12/24"))
d$Date<-as.Date(d$Date,format =  "%Y-%m-%d")
d$doy<-yday(d$Date)
d
d$doy[3]-d$doy[2]
d$doy[3]-d$doy[1]
