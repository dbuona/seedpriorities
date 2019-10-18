##This file can be used to generate a data sheet that randomly plants seeds into specific grid locations within a pot.
rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("~/Documents/git/seedpriorities/pilot")

replicate<-seq(1:3)
strat<-c(6,10)
inc<-c("H","M","L")


pot_type<- c("Cc.Hm","Cc.Pv","Pv.Ed","Cc.Ap","Em.Ed")
d2<-data.frame(strat=numeric(),inc=numeric(),replicate=numeric(),pot_type=character(), row=numeric(), col=numeric(),seed=numeric())  ##generate fake data

for (l in c(1:length(pot_type))){
for (j in c(1:length(inc))){
  for(i in c(1:length(strat))){
    for(k in c(1:length(replicate))){ 
      
      dfhere2 <- data.frame(strat=rep(strat[i],times=20),inc=rep(inc[j],times=20),replicate=rep(replicate[k],times=20), pot_type=rep(pot_type[l],times=20),row=rep(c("a","b","c","d","e","f","g"),
                                                                                                                                                              times=c(3,3,3,3,3,3,2)),col=c(1,3,5,2,4,6,1,3,5,2,4,6,1,3,5,2,4,6,3,5), seed=sample(rep(c(0,1),time=10)))
      
      d2 <- rbind(d2, dfhere2) ## rbind it here for safty
    }
  }
}
}

pot_type<- c("Cc","Hm","Pv","Ed","Ap","Em")
d3<-data.frame(strat=numeric(),inc=numeric(),replicate=numeric(),pot_type=character(), row=numeric(), col=numeric(),seed=numeric())  ##generate fake data
for (l in c(1:length(pot_type))){
  for (j in c(1:length(inc))){
    for(i in c(1:length(strat))){
      for(k in c(1:length(replicate))){ 
        
        dfhere3 <- data.frame(strat=rep(strat[i],times=20),inc=rep(inc[j],times=20),replicate=rep(replicate[k],times=20), pot_type=rep(pot_type[l],times=20),row=rep(c("a","b","c","d","e","f","g"),
                                                                                                                                                                     times=c(3,3,3,3,3,3,2)),col=c(1,3,5,2,4,6,1,3,5,2,4,6,1,3,5,2,4,6,3,5), seed=sample(rep(c(0,1),time=10)))
        
        d3 <- rbind(d3, dfhere3) ## rbind it here for safty
      }
    }
  }
}

### get rid of row we dont need
#Ie 
unique(d2$pot_type)
d2$keeper=NA
d2<- within(d2,keeper[d2$pot_type %in% c("Cc.Pv","Pv.Ed","Em.Ed") & d2$inc=="L"]<-"no")
d2<- within(d2,keeper[d2$pot_type==c("Cc.Hm") & d2$inc=="H"]<-"no")

d3$keeper=NA
d3<- within(d3,keeper[d3$pot_type %in%c("Pv","Ed","Em") & d3$inc=="L"]<-"no")
d3<- within(d3,keeper[d3$pot_type==c("Hm") & d3$inc=="H"]<-"no")
d<-rbind(d2,d3)


d<-dplyr::filter(d,is.na(keeper))
d$keeper<-"yes"

d<-tidyr::unite(d,ID,strat,inc,replicate,pot_type,sep="_",remove=FALSE)
d<-tidyr::unite(d,rowcol,row,col,sep=".",remove=FALSE)
unique(d$pot_type)

d$taxa<-NA



d$taxa[which(d$pot_type=="Cc.Hm" & d$seed==1)]<-"C.canadensis"
d$taxa[which(d$pot_type=="Cc.Hm" & d$seed==0)]<-"H.matronalis"

d$taxa[which(d$pot_type=="Cc.Pv" & d$seed==1)]<-"C.canadensis"
d$taxa[which(d$pot_type=="Cc.Pv" & d$seed==0)]<-"P.virginiatum"

d$taxa[which(d$pot_type=="Pv.Ed" & d$seed==1)]<-"P.virginiatum"
d$taxa[which(d$pot_type=="Pv.Ed" & d$seed==0)]<-"E.divaricata"

d$taxa[which(d$pot_type=="Cc.Ap" & d$seed==1)]<-"C.canadensis"
d$taxa[which(d$pot_type=="Cc.Ap" & d$seed==0)]<-"A.petiolaria"

d$taxa[which(d$pot_type=="Em.Ed" & d$seed==1)]<-"E.macrophylla"
d$taxa[which(d$pot_type=="Em.Ed" & d$seed==0)]<-"E.divaricata"

d$taxa[which(d$pot_type=="Cc")]<-"C.canadensis"
d$taxa[which(d$pot_type=="Hm")]<-"H.matronalis"
d$taxa[which(d$pot_type=="Pv")]<-"P.virginiatum"
d$taxa[which(d$pot_type=="Ed")]<-"E.diviricata"
d$taxa[which(d$pot_type=="Ap")]<-"A.petiolaria"
d$taxa[which(d$pot_type=="Em")]<-"E.macrophylla"
unique(d$taxa)
d<-dplyr::select(d,-keeper)

d<-d[order(d$strat),]

ids<-unique(d$ID)
d<-transform(d,potnumber=match(ID, unique(ID)))
unique(d$potnumber)

write.csv(d,"germination_data_sheet.csv",row.names = FALSE)
