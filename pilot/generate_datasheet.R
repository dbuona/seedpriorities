##This file can be used to generate a data sheet that randomly plants seeds into specific grid locations within a pot.
rm(list=ls()) 
options(stringsAsFactors = FALSE)

setwd("~/Documents/git/seedpriorities/pilot")

replicate<-seq(1:3)
strat<-c(6,10)
inc<-c("W","C")

d<-data.frame(strat=numeric(),inc=numeric(),replicate=numeric(),pot_type=character(), row=numeric(), col=numeric(),seed=numeric())  ##generate fake data


for (j in c(1:length(inc))){
for(i in c(1:length(strat))){
  for(k in c(1:length(replicate))){ 
    
    dfhere <- data.frame(strat=rep(strat[i],times=20),inc=rep(inc[j],times=20),replicate=rep(replicate[k],times=20), pot_type=rep("Cc.Hm",times=20),row=rep(c("a","b","c","d","e","f","g"),
                          times=c(3,3,3,3,3,3,2)),col=c(1,3,5,2,4,6,1,3,5,2,4,6,1,3,5,2,4,6,3,5), seed=sample(rep(c(0,1),time=10)))
    
    d <- rbind(d, dfhere) ## rbind it here for safty
  }
}
}




d2<-data.frame(strat=numeric(),inc=numeric(),replicate=numeric(),pot_type=character(), row=numeric(), col=numeric(),seed=numeric())  ##generate fake data


for (j in c(1:length(inc))){
  for(i in c(1:length(strat))){
    for(k in c(1:length(replicate))){ 
      
      dfhere2 <- data.frame(strat=rep(strat[i],times=20),inc=rep(inc[j],times=20),replicate=rep(replicate[k],times=20), pot_type=rep("Hesp",times=20),row=rep(c("a","b","c","d","e","f","g"),
                times=c(3,3,3,3,3,3,2)),col=c(1,3,5,2,4,6,1,3,5,2,4,6,1,3,5,2,4,6,3,5), seed=sample(rep(c(0),time=20)))
      
      d2 <- rbind(d2, dfhere2) ## rbind it here for safty
    }
  }
}

d3<-data.frame(strat=numeric(),inc=numeric(),replicate=numeric(),pot_type=character(), row=numeric(), col=numeric(),seed=numeric())  ##generate fake data


for (j in c(1:length(inc))){
  for(i in c(1:length(strat))){
    for(k in c(1:length(replicate))){ 
      
      dfhere3 <- data.frame(strat=rep(strat[i],times=20),inc=rep(inc[j],times=20),replicate=rep(replicate[k],times=20), pot_type=rep("Cryp",times=20),row=rep(c("a","b","c","d","e","f","g"),
           times=c(3,3,3,3,3,3,2)),col=c(1,3,5,2,4,6,1,3,5,2,4,6,1,3,5,2,4,6,3,5), seed=sample(rep(c(1),time=20)))
      
      d3 <- rbind(d3, dfhere3) ## rbind it here for safty
    }
  }
}


d<-rbind(d,d2,d3)
d<-tidyr::unite(d,ID,strat,inc,replicate,pot_type,sep="_",remove=FALSE)
d<-tidyr::unite(d,rowcol,row,col,sep=".",remove=FALSE)
unique(d$ID)
d$taxa<-ifelse(d$seed==1,"C.canadensis","H.matronalis")
