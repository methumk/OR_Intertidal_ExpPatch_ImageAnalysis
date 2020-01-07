############################################################
############################################################
# Analyses/plots relating to Whelk dynamics, size-frequency
# distributions and feeding observations.
############################################################
############################################################
rm(list=ls()) # clears workspace
options(stringsAsFactors=F)

library(lubridate)
library(dplyr)
library(gam)
###########################################################
# Update data files (assuming db tables have been exported)
###########################################################
source('/Volumes/NovakLab/Projects/OR_Intertidal/FeedingObs_db/FeedingObs_db-Scripts/FeedingObs_db-Parse.r')

#######################
setwd('/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches/')
#######################
# Convenience functions
#######################
# standard error
se<-function(x,na.rm=TRUE){sd(na.omit(x))/sqrt(length(na.omit(x)))}

##########################################################################################
############################  Whelk Densities   ##########################################
##########################################################################################

##############
# Import data
##############
dat<-read.csv('ExpPatch-Data/ExpPatch_WhelkCounts.csv')

##################
# Data preparation
##################
# remove missed dates
dat<-subset(dat,Date!='NA')

# Counts of quadrats
Quads<-aggregate(list(Count=dat$N.ostrina),by=list(Date=dat$Date,Patch=dat$Patch),length)
if(any(Quads$Count!=9)){
	warning("Problem: too many or few quads reported.")
	Quads[which(Quads$Count!=9),]}

dat$Date<-as.Date(dat$Date,format='%m.%d.%y')
Wks<-dat$Week<-floor_date(dat$Date,"week")

# Convert to densities
Area<-0.25*0.35
dat$N.ostrina<-dat$N.ostrina/Area
dat$N.canaliculata<-dat$N.canaliculata/Area

############
# Statistics
############
# Patch-level (averaged across quadrats)
No.Patch<-merge(aggregate(list(Mean=dat$N.ostrina),by=list(Week=dat$Week,Patch=dat$Patch),mean,na.rm=TRUE),aggregate(list(SE=dat$N.ostrina),by=list(Week=dat$Week,Patch=dat$Patch),se,na.rm=TRUE))
Nc.Patch<-merge(aggregate(list(Mean=dat$N.canaliculata),by=list(Week=dat$Week,Patch=dat$Patch),mean,na.rm=TRUE),aggregate(list(SE=dat$N.canaliculata),by=list(Week=dat$Week,Patch=dat$Patch),se,na.rm=TRUE))

# Site-level (averaged across patch-means, not at quadrat scale)
No.Site<-merge(aggregate(list(Mean=No.Patch$Mean),by=list(Week=No.Patch$Week),mean,na.rm=TRUE),aggregate(list(SE=No.Patch$Mean),by=list(Week=No.Patch$Week),se,na.rm=TRUE))
Nc.Site<-merge(aggregate(list(Mean=Nc.Patch$Mean),by=list(Week=Nc.Patch$Week),mean,na.rm=TRUE),aggregate(list(SE=Nc.Patch$Mean),by=list(Week=Nc.Patch$Week),se,na.rm=TRUE))

##########
# Plotting
##########
pdf('ExpPatch-Output/ExpPatch_Nucella_PopnDynamics.pdf',height=5,width=3.5)
par(mfrow=c(2,1),las=1,mar=c(4,4,1,0.5),mgp=c(2.5,0.4,0),tcl=-0.3,cex.axis=0.8,cex=0.9,cex.lab=1.1)
ymax=400
Patches<-unique(No.Patch$Patch)
plot(Mean~Week,data=No.Patch,type='n',ylim=c(0,ymax),las=1, xlab='', ylab=expression(paste('Density ',(m^{-2}))),main='N. ostrina',xaxt='n')
	axis.Date(1,at=seq(as.Date('2013/6/1'),max(Wks),'months'),format='%b',las=2)
	polygon(c(No.Site$Week,rev(No.Site$Week)), c(No.Site$Mean+No.Site$SE,rev(No.Site$Mean-No.Site$SE)), col='grey90',border=NA)
	for(p in Patches){
		tdat<-subset(No.Patch,Patch==p)
		points(tdat$Week,tdat$Mean,type='l',col='grey60')
		}
	points(No.Site$Week,No.Site$Mean,type='o',pch=19,lwd=2)

plot(Mean~Week,data=Nc.Patch,type='n',ylim=c(0,ymax),las=1, xlab='', ylab=expression(paste('Density ',(m^{-2}))),main=' N. canaliculata',xaxt='n')
	axis.Date(1,at=seq(as.Date('2013/6/1'),max(Wks),'months'),format='%b',las=2)
	polygon(c(Nc.Site$Week,rev(Nc.Site$Week)), c(Nc.Site$Mean+Nc.Site$SE,rev(Nc.Site$Mean-Nc.Site$SE)), col='grey90',border=NA)
	for(p in Patches){
		tdat<-subset(Nc.Patch,Patch==p)
		points(tdat$Week,tdat$Mean,type='l',col='grey60')
		}
	points(Nc.Site$Week,Nc.Site$Mean,type='o',pch=19,lwd=2)
dev.off()

##########################################################################################
#########################  Size distributions   ##########################################
##########################################################################################
Obs<-read.csv('ExpPatch-Data/ExpPatch_FeedingObs-Obs.csv')
Cens<-read.csv('ExpPatch-Data/ExpPatch_FeedingObs-Census.csv')

Obs$Date<-ymd(Obs$Date)
SizeStats<-summarise(group_by(Obs,Date,Pred), Size.mean=mean(PredSize),Size.sd=sd(PredSize),Size.median=median(PredSize),Count=n())

Dates<-unique(Obs$Date)
Preds<-unique(Obs$Pred)
Patches<-unique(Cens[,c('Area','Height_rel_MLLW_cm')])
Patches<-Patches[order(Patches$Height_rel_MLLW_cm),]

nDates<-length(Dates)
mxSize<-max(Obs$PredSize)+1

pdf('ExpPatch-Output/ExpPatch_Nucella_Size_Nostrina.pdf',height=4,width=8)
par(oma=c(5,4,3,2.4),mar=c(0,0,0,0),yaxs='i')
	p=2
	mxFreqs<-c(50,250,530,675,1000) # see hist of mxFreqsObs below to set
	mxFreqsObs<-dim(0)
	nf<-layout(matrix(seq(1,nDates),1,nDates),widths=1)
	for(d in 1:length(Dates)){
		temp<-subset(Obs,Date==ymd(Dates[d])&Pred==Preds[p])
		tab<-table(factor(temp$Patch,levels=Patches$Area), factor(temp$PredSize,levels=seq(0,mxSize)))
		tab[tab==0]<-NA
# 		none<-apply(tab,1,sum,na.rm=TRUE)
# 		tab[none==0,]<-0
		maxFreq<-max(apply(tab,2,sum,na.rm=TRUE))
		mxFreqsObs<-c(mxFreqsObs,maxFreq)
# 		mxFreq<-mxFreqs[mxFreqs>maxFreq][1] # uncomment to scale to common xlim values
		mxFreq<-maxFreq+1 # uncomment to scale to individual xlim values
		barplot(tab,axes=FALSE,xlim=c(0,mxFreq),ylim=c(0,mxSize), horiz=TRUE,axisnames=FALSE,border=NA)
		text(mxFreq/2, 0, labels = Dates[d], srt = 45, adj = c(1.1,1.1), xpd = NA, cex=1)
		axis(3,at=c(mxFreq),label=c(mxFreq), cex.axis=.8,tcl=-0.2,mgp=c(0,0.3,0),las=2)
		if(d==1){
			axis(2,las=2,at=seq(0,mxSize,5))
			mtext('Shell length (mm)',2,line=2.5)		}
		if(d==length(Dates)){axis(4,las=2,at=seq(0,mxSize,5))	}
	}
	box("inner", col="black")
dev.off()

# hist(mxFreqsObs,breaks=20,axes=FALSE)
# axis(1,at=seq(0,max(mxFreqs),25),las=2,cex.axis=.6)
# axis(2)


pdf('ExpPatch-Output/ExpPatch_Nucella_Size_canalic.pdf',height=4,width=8)
par(oma=c(5,4,3,2.4),mar=c(0,0,0,0),yaxs='i')
	p=1
	mxFreqs<-c(25,70,150,1000) # see hist of mxFreqsObs below to set
	mxFreqsObs<-dim(0)
	nf<-layout(matrix(seq(1,nDates),1,nDates),widths=1)
	for(d in 1:length(Dates)){
		temp<-subset(Obs,Date==ymd(Dates[d])&Pred==Preds[p])
		tab<-table(factor(temp$Patch,levels=Patches$Area), factor(temp$PredSize,levels=seq(0,mxSize)))
		tab[tab==0]<-NA
# 		none<-apply(tab,1,sum,na.rm=TRUE)
# 		tab[none==0,]<-0
		maxFreq<-max(apply(tab,2,sum,na.rm=TRUE))
		mxFreqsObs<-c(mxFreqsObs,maxFreq)
# 		mxFreq<-mxFreqs[mxFreqs>maxFreq][1] # uncomment to scale to common xlim values
		mxFreq<-maxFreq+1 # uncomment to scale to individual xlim values
		barplot(tab,axes=FALSE,xlim=c(0,mxFreq),ylim=c(0,mxSize), horiz=TRUE,axisnames=FALSE,border=NA)
		text(mxFreq/2, 0, labels = Dates[d], srt = 45, adj = c(1.1,1.1), xpd = NA, cex=1)
		axis(3,at=c(mxFreq),label=c(mxFreq), cex.axis=.8,tcl=-0.2,mgp=c(0,0.3,0),las=2)
		if(d==1){
			axis(2,las=2,at=seq(0,mxSize,5))
			mtext('Shell length (mm)',2,line=2.5)		}
		if(d==length(Dates)){axis(4,las=2,at=seq(0,mxSize,5))	}
	}
	box("inner", col="black")
dev.off()

# hist(mxFreqsObs,breaks=20,axes=FALSE)
# axis(1,at=seq(0,max(mxFreqs),25),las=2,cex.axis=.6)
# axis(2)


##########################################################################################
##############################  Fraction Feeding #########################################
##########################################################################################
# Data imported above.
FFP<-summarise(group_by(Obs,Date,Pred,Patch),NF=sum(Prey=='Not_Feeding',na.rm=TRUE),All=n(), F=All-NF,fracF=(All-NF)/All, fracFbg=sum(Prey=='Balanus_glandula',na.rm=TRUE)/n(), fracFmt=sum(Prey=='Mytilus_trossulus',na.rm=TRUE)/n())
FF<-summarise(group_by(FFP,Date,Pred),meanFF=mean(fracF),seFF=se(fracF),meanFFbg=mean(fracFbg),seFFbg=se(fracFbg),meanFFmt=mean(fracFmt),seFFmt=se(fracFmt))

pdf('ExpPatch-Output/ExpPatch_Nucella_FracFeed.pdf',height=5,width=3.5)
par(mfrow=c(2,1),las=1,mar=c(4,4,1,0.5),mgp=c(2.5,0.4,0),tcl=-0.3,cex.axis=0.8,cex=0.9, cex.lab=1.1)
ymax=0.5
temp<-filter(FF,Pred=='Nucella_ostrina')
ttemp<-filter(FFP,Pred=='Nucella_ostrina')
plot(meanFF~as.Date(Date),data=temp,type='n',ylim=c(0,ymax),las=1, xlab='', ylab='Fraction feeding',main='N. ostrina',xaxt='n')
	axis.Date(1,at=seq(as.Date('2013/6/1'),as.Date(max(ymd(Dates))),'months'), format='%b',las=2)
	polygon(as.Date(c(temp$Date,rev(temp$Date))), c(temp$meanFF+temp$seFF,rev(temp$meanFF-temp$seFF)), col='grey90',border=NA)
	for(p in Patches$Area){
		tttemp<-filter(ttemp,Area==p)
		points(as.Date(tttemp$Date),tttemp$fracF,type='l',col='grey60')
		}
	points(as.Date(temp$Date),temp$meanFF,type='o',pch=19,lwd=2)

temp<-filter(FF,Pred=='Nucella_canaliculata')
ttemp<-filter(FFP,Pred=='Nucella_canaliculata')
plot(meanFF~as.Date(Date),data=temp,type='n',ylim=c(0,ymax),las=1, xlab='', ylab='Fraction feeding',main='N. canaliculata',xaxt='n')
	axis.Date(1,at=seq(as.Date('2013/6/1'),as.Date(max(ymd(Dates))),'months'), format='%b',las=2)
	polygon(as.Date(c(temp$Date,rev(temp$Date))), c(temp$meanFF+temp$seFF,rev(temp$meanFF-temp$seFF)), col='grey90',border=NA)
	for(p in Patches$Area){
		tttemp<-filter(ttemp,Area==p)
		points(as.Date(tttemp$Date),tttemp$fracF,type='l',col='grey60')
		}
	points(as.Date(temp$Date),temp$meanFF,type='o',pch=19,lwd=2)
dev.off()

############################################
# on Balanus glandula and Mytilus trossulus
############################################

pdf('ExpPatch-Output/ExpPatch_Nucella_FracFeed_BgMt.pdf',height=5,width=7)
par(mfcol=c(2,2),las=1,mar=c(4,4,1,0.5),mgp=c(2.5,0.4,0),tcl=-0.3,cex.axis=0.8,cex=0.9,cex.lab=1.1,cex.main=0.8)
ymax=0.3
temp<-filter(FF,Pred=='Nucella_ostrina')
ttemp<-filter(FFP,Pred=='Nucella_ostrina')
plot(meanFFbg~as.Date(Date),data=temp,type='n',ylim=c(0,ymax),las=1, xlab='', ylab='Fraction feeding',main='N. ostrina - Balanus glandula',xaxt='n')
	axis.Date(1,at=seq(as.Date('2013/6/1'),as.Date(max(ymd(Dates))),'months'), format='%b',las=2)
	polygon(as.Date(c(temp$Date,rev(temp$Date))), c(temp$meanFFbg+temp$seFFbg,rev(temp$meanFFbg-temp$seFFbg)), col='grey90',border=NA)
	for(p in Patches$Area){
		tttemp<-filter(ttemp,Area==p)
		points(as.Date(tttemp$Date),tttemp$fracFbg,type='l',col='grey60')
		}
	points(as.Date(temp$Date),temp$meanFFbg,type='o',pch=19,lwd=2)

plot(meanFFmt~as.Date(Date),data=temp,type='n',ylim=c(0,ymax),las=1, xlab='', ylab='Fraction feeding',main='N. ostrina - Mytilus trossulus',xaxt='n')
	axis.Date(1,at=seq(as.Date('2013/6/1'),as.Date(max(ymd(Dates))),'months'), format='%b',las=2)
	polygon(as.Date(c(temp$Date,rev(temp$Date))), c(temp$meanFFmt+temp$seFFmt,rev(temp$meanFFmt-temp$seFFmt)), col='grey90',border=NA)
	for(p in Patches$Area){
		tttemp<-filter(ttemp,Area==p)
		points(as.Date(tttemp$Date),tttemp$fracFmt,type='l',col='grey60')
		}
	points(as.Date(temp$Date),temp$meanFFmt,type='o',pch=19,lwd=2)


temp<-filter(FF,Pred=='Nucella_canaliculata')
ttemp<-filter(FFP,Pred=='Nucella_canaliculata')
plot(meanFFbg~as.Date(Date),data=temp,type='n',ylim=c(0,ymax),las=1, xlab='', ylab='Fraction feeding',main='N. canaliculata - Balanus glandula',xaxt='n')
	axis.Date(1,at=seq(as.Date('2013/6/1'),as.Date(max(ymd(Dates))),'months'), format='%b',las=2)
	polygon(as.Date(c(temp$Date,rev(temp$Date))), c(temp$meanFFbg+temp$seFFbg,rev(temp$meanFFbg-temp$seFFbg)), col='grey90',border=NA)
	for(p in Patches$Area){
		tttemp<-filter(ttemp,Area==p)
		points(as.Date(tttemp$Date),tttemp$fracFbg,type='l',col='grey60')
		}
	points(as.Date(temp$Date),temp$meanFFbg,type='o',pch=19,lwd=2)

plot(meanFFmt~as.Date(Date),data=temp,type='n',ylim=c(0,ymax),las=1, xlab='', ylab='Fraction feeding',main='N. canaliculata - Mytilus trossulus',xaxt='n')
	axis.Date(1,at=seq(as.Date('2013/6/1'),as.Date(max(ymd(Dates))),'months'), format='%b',las=2)
	polygon(as.Date(c(temp$Date,rev(temp$Date))), c(temp$meanFFmt+temp$seFFmt,rev(temp$meanFFmt-temp$seFFmt)), col='grey90',border=NA)
	for(p in Patches$Area){
		tttemp<-filter(ttemp,Area==p)
		points(as.Date(tttemp$Date),tttemp$fracFmt,type='l',col='grey60')
		}
	points(as.Date(temp$Date),temp$meanFFmt,type='o',pch=19,lwd=2)

dev.off()




##########################################################################################
##########################################################################################
##########################################################################################

