###prepare data for knb
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
library(brms)

d<-read.csv("data_4_invasive.csv")
colnames(d)
d<-dplyr::select(d,-X,-sp)
d<-dplyr::select(d,-Cc_percap,-Hm_percap,-priority2)
write.csv(d,"comp_trial_knb.csv",row.names = FALSE)
