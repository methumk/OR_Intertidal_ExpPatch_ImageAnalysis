# Summarize and plot species dynamics from photographed and counted experimental patch plots
##############
rm(list=ls()) # clears workspace
options(stringsAsFactors=F)

#Install packages
require(gdata)
require(lubridate)
require(RCurl)
require(reshape2)

#############
source('/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches/ExpPatch-Scripts/ExpPatch_PhotoCountParser.r')
setwd('/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches/')

###############################
# Preliminary time-series stats
###############################
StatsFun<-function(x){x<-na.omit(x);c(Mean=mean(x),SD=sd(x), SE=sd(x)/sqrt(length(x)),n=length(x))}
# Patch-specific
StatsPatch<-aggregate(Density~Date+Patch+Species, data=dat,StatsFun)

# Site-level across patch-averages
StatsSite<-aggregate(list(Density=StatsPatch$Dens[,'Mean']), by=list(Date=StatsPatch$Date,Species=StatsPatch$Species), StatsFun)

# Insert zeroes for single-observation sd and se
StatsPatch$Density[,c('SD','SE')][is.na(StatsPatch$Density[,c('SD','SE')])]<-0
StatsSite$Density[,c('SD','SE')][is.na(StatsSite$Density[,c('SD','SE')])]<-0

#Create single graph for Pollicipes polymerus (or whatever species you want)
par(mfrow=c(1,1)
    policip<-subset(StatsSite,Species=='Pollicipes polymerus')
    policip
    maxy<-1.2*max(policip$Density[,'Mean']+policip$Density[,'SE'],na.rm=TRUE)
    plot(policip$Date,(policip$Density[,'Mean']+policip$Density[,'SE']), 
    #Not sure what the units on density are...     
    xlab="Date",ylab='Density', type='n', main='Pollicipes polymerus', xaxt='n',ylim=c(0,maxy))
    xat<-seq(min(policip$Date), max(policip$Date), "months")
    axis.Date(1,at=xat)
    polygon(c(policip$Date,rev(policip$Date)), c(policip$Density[,'Mean']+policip$Density[,'SE'], rev(policip$Density[,'Mean']-policip$Density[,'SE'])), col='grey90')
    points(policip$Date,policip$Density[,'Mean'],type='o',pch=21,bg='grey')
    text(policip$Date,maxy*0.98,policip$Density[,'n'],cex=0.6)




