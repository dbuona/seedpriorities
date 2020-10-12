### This make the full experimental datasheet
rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("~/Documents/git/seedpriorities/full_exp")
set.seed(613)

replicate<-seq(1:3)
strat<-c(6,12)
pot_type<- c("Cc.Hm","Cc.Pv","Hm.Pv")

monoA<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1)
monoB<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0)  
quarterB<-c(1,1,1,1,1,1,1,1,1,1,0,0,0,0)
quarterA<-c(0,0,0,0,0,0,0,0,0,0,1,1,1,1)
even<-c(1,1,1,1,1,1,1,0,0,0,0,0,0,0)

config<-list(monoA=monoA,monoB=monoB,quarterA=quarterA,quarterB=quarterB,even=even)


d<-data.frame(pot_type=character(),strat=numeric(),rep=numeric(),config=character(),row=numeric(), col=numeric(),seed=numeric())  ##generate fake data

for(l in c(1:length(pot_type))){ 
for(j in c(1:length(strat))){ 
for(k in c(1:length(replicate))){ 
for(i in c(1:length(config))){

        dfhere <- data.frame(pot_type=pot_type[l],strat=strat[j],rep=replicate[k],config=names(config[i]),row=rep(c("a","b","c","d","e","f"),times=c(2,2,3,3,2,2))
                             ,col=c(2,4,3,5,2,4,6,1,3,5,2,4,3,5), seed=sample(config[[i]],size=14))
        
        d <- rbind(d, dfhere) ## rbind it here for safty

}
}
}
}




d$density<-"high"

####make low dnesity
monoA2<-c(1,1,1,1,1,1,1,1)
monoB2<-c(0,0,0,0,0,0,0,0)  
quarterB2<-c(1,1,1,1,1,1,0,0)
quarterA2<-c(0,0,0,0,0,0,1,1)
half2<-c(0,0,0,0,1,1,1,1)
config2<-list(monoA=monoA2,monoB=monoB2,quarterA=quarterA2,quarterB=quarterB2,even=half2)

d2<-data.frame(pot_type=character(),strat=numeric(),rep=numeric(),config=character(),row=numeric(), col=numeric(),seed=numeric())


for(l in c(1:length(pot_type))){ 
  for(j in c(1:length(strat))){ 
    for(k in c(1:length(replicate))){ 
      for(i in c(1:length(config2))){
        
        dfhere2 <- data.frame(pot_type=pot_type[l],strat=strat[j],rep=replicate[k],config=names(config2[i]),row=rep(c("a","c","e"),times=c(2,3,3))
                             ,col=c(2,5,1,3,6,1,4,6), seed=sample(config2[[i]],size=8))
        
        d2 <- rbind(d2, dfhere2) ## rbind it here for safty
        
      }
    }
  }
}


d2$density<-"low"
d<-rbind(d,d2)





d$taxa=NA
d$taxa[which(d$pot_type=="Cc.Hm" & d$seed==1)]<-"C.canadensis"
d$taxa[which(d$pot_type=="Cc.Hm" & d$seed==0)]<-"H.matronalis"
d$taxa[which(d$pot_type=="Cc.Pv" & d$seed==1)]<-"C.canadensis"
d$taxa[which(d$pot_type=="Cc.Pv" & d$seed==0)]<-"P.virginiana"
d$taxa[which(d$pot_type=="Hm.Pv" & d$seed==1)]<-"P.virginiana"
d$taxa[which(d$pot_type=="Hm.Pv" & d$seed==0)]<-"H.matronalis"



dearly<-d
dlate<-d

dearly$harvest<-"early"
dlate$harvest<-"late"

d<-rbind(dearly,dlate)

d<-d[(order(d$strat)),]

d<-tidyr::unite(d,ID,strat,density,config,replicate,pot_type,harvest,sep="_",remove=FALSE)
d<-tidyr::unite(d,rowcol,row,col,sep=".",remove=FALSE)
ids<-unique(d$ID)
d<-transform(d,potnumber=match(ID, unique(ID)))
unique(d$potnumber)
write.csv(d,"full_data_sheet.csv",row.names = FALSE)

486/3
162/18

486*2/3

d12<-dplyr::filter(d,strat==12)
table(d12$taxa)
table(d12$density)


d12.small<-filter(d12,density=="high" &pot_type=="Hm.Pv")

pdf("HmPv.high.12.pdf")
ggplot(d12.small,aes(row,col))+geom_point(aes(color=taxa),size=3)+facet_wrap(~potnumber)+
  scale_y_continuous(trans = "reverse",breaks=1:6)+
scale_x_discrete(labels = c("a","b","c","d","e","f"))+
  scale_color_manual(values=c("darkorchid","darkgreen"))+
  theme_bw()
dev.off()

d12.small<-filter(d12,density!="high" &pot_type=="Hm.Pv")

pdf("HmPv.low.12.pdf")
ggplot(d12.small,aes(row,col))+geom_point(aes(color=taxa),size=3)+facet_wrap(~potnumber)+
  scale_y_continuous(trans = "reverse",breaks=1:6)+
  #scale_x_discrete(labels = c("a","b","c","d","e","f"))+
  scale_color_manual(values=c("darkorchid","darkgreen"))+
  theme_bw()
dev.off()
ß