
setwd("/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches/")
dat<-read.csv("ExpPatch-Data/ExpPatch_TrainingSetComparisons.csv",skip=1,header=TRUE)

datb<-dat[,c(2:9)]
datm<-dat[,c(2,10:ncol(dat))]

quartz()
pairs(datb[,-1],pch=16)

quartz()
pairs(datm[,-1],pch=16)


quartz()
pairs(datb[,-1],panel = function(x,y) text(x,y, labels=datb[,1]))

quartz()
pairs(datm[,-1],panel = function(x,y) text(x,y, labels=datm[,1]))

