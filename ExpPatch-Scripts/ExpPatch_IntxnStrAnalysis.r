#############################################################
rm(list=ls()) # clears workspace
options(stringsAsFactors=F)
if("RCurl" %in% loadedNamespaces()){detach("package:RCurl", unload=TRUE)}# Interferes with "complete" from tidyr
library(plyr)
library(tidyr)
library(sfsmisc)
library(scales) # for color scales
library(mgcv) # for generalized additive models


# Load convenience functions
source('/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches/ExpPatch-Scripts/ExpPatch_MiscFunctions.r')

###########################################################
# Update data files (assuming db tables have been exported)
###########################################################
# source('/Volumes/NovakLab/Projects/OR_Intertidal/FeedingObs_db/FeedingObs_db-Scripts/FeedingObs_db-Parse.r')
# source('/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches/ExpPatch-Scripts/ExpPatch_PhotoCountParser.r')
# source('/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches/ExpPatch-Scripts/ExpPatch_IntxnStr.r')

#############
# Import data
#############
setwd('/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches/')
dat<-read.csv('ExpPatch-Data/ExpPatch_IntxnStr.csv')

###################
# Data preparations
###################
dat$Date<-as.Date(dat$Date)
preySp<-sort(unique(dat$Prey))
predSp<-sort(unique(dat$Pred))
patches<-sort(unique(dat$Patch))[c(1,8,14:18,2:7,9:13)]
sDates<-sort(unique(dat$Date))

# These should go away with more prey pictures being processed.
# They're due to prey for which feeding observations were made but
# which were not (yet) observed in the picture quad counts.
dat$att[is.infinite(dat$att)]<-0
dat$frate[is.infinite(dat$frate)]<-0

# Turn NA's (not observed) into "true" zeros.
dat$att[is.na(dat$att)]<-0
dat$frate[is.na(dat$frate)]<-0

# Site-level summary stats.
# Diet-related means *weighted* by number of feeding observations in that patch.
site<-ddply(dat,.(Date,Pred,Prey),summarise,
	attM=weighted.mean(att,(fcnt+1), na.rm=TRUE),
	frateM=weighted.mean(frate,(fcnt+1), na.rm=TRUE),
	attVar=var.wtd.mean.cochran(att,(fcnt+1), na.rm=TRUE),
	frateVar=var.wtd.mean.cochran(frate,(fcnt+1), na.rm=TRUE),
	htimeM=weighted.mean(htime,(fcnt+1), na.rm=TRUE),
	fcnt=sum(fcnt,na.rm=TRUE),
	tObs=sum(tObs,na.rm=TRUE),
	DensityM=mean(Density,na.rm=TRUE),
	DensityVar=var(Density,na.rm=TRUE),
	DensitySE=sd(Density,na.rm=TRUE)/sqrt(length(Density[!is.na(Density)])),
	avgTemp=mean(avgTemp,na.rm=TRUE))

# Color-scale by temperature (NOTE: Could end up being different across different plots)
colfoo<-col_numeric(colorRamp(c("Blue", "Yellow","Red"), interpolate="spline"),site$avgTemp)

# Baseline data
Dat<-dat
Site<-site

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#############
# Time-series
#############
#~~~~~~~~~~~~
# Temperature
#~~~~~~~~~~~~
Temp<-unique(site[,c('Date','avgTemp')])
pdf('ExpPatch-Output/ExpPatch_Dynamics_Temperature-Site.pdf',height=3,width=4)
par(las=2,cex=0.8,mar=c(4,3,1,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.7,cex.main=0.5)
	plot(Temp$Date,Temp$avgTemp,pch=21,type='o',bg=colfoo(Temp$avgTemp), xlab='Average of preceeding 3 weeks', ylab='Temp. C',axes=FALSE)
	ax1.at<-pretty(Temp$Date,6)
	axis(1,at=ax1.at,labels=format(ax1.at,"%b"),las=2)
	axis(2)
	box(lwd=1)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Site and patch-specific attack rates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf('ExpPatch-Output/ExpPatch_Nucella_Dynamics_AttRate-Site.pdf',height=8,width=11)
par(mfcol=c(4,3),las=2,cex=0.8,mar=c(4,3,1,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.7,cex.main=0.5)
	ymin<-min(dat$att[dat$att!=0],na.rm=TRUE)
	for(j in length(predSp):1){
	for(i in 1:length(preySp)){
		mdat<-subset(site,Pred==predSp[j] & Prey==preySp[i])
		sdat<-subset(dat,Pred==predSp[j] & Prey==preySp[i])
		ylim<-c(min(mdat$attM,na.rm=TRUE),1.1*max(ymin, max(mdat$attM,na.rm=TRUE))) # ylim by site max
		plot(mdat$Date,mdat$attM,type='n',ylim=ylim,xlab='',ylab='', main=paste(predSp[j],preySp[i]),axes=FALSE)
		ax1.at<-pretty(mdat$Date,6)
		axis(1,at=ax1.at,labels=format(ax1.at,"%b"),las=2)
		eaxis(2)
		box(lwd=1)
		for(p in 1:length(patches)){
			tdat<-subset(dat,Pred==predSp[j] & Prey==preySp[i] & Patch==patches[p])
			points(tdat$Date,tdat$att,type='l',col='grey')
		}
		points(mdat$Date,mdat$attM,type='o',pch=21,bg=colfoo(mdat$avgTemp), cex=ptscale(mdat$fcnt))
	}}
dev.off()


pdf('ExpPatch-Output/ExpPatch_Nucella_Dynamics_AttRate-Site_log.pdf',height=8,width=11)
par(mfcol=c(4,3),las=2,cex=0.8,mar=c(4,3,1,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.7,cex.main=0.5)
	for(j in length(predSp):1){
	for(i in 1:length(preySp)){
		mdat<-subset(site,Pred==predSp[j] & Prey==preySp[i])
		sdat<-subset(dat,Pred==predSp[j] & Prey==preySp[i])
	if(sum(sdat$att)==0){
		plot(mdat$Date,mdat$attM,type='n',ylim=ylim,xlab='',ylab='', main=paste(predSp[j],preySp[i]),axes=FALSE)
	}
	if(sum(sdat$att)!=0){
		ymin<-min(sdat$att[sdat$att!=0],na.rm=TRUE)
		logOffset<-ymin
		ylim<-c(min(mdat$attM,na.rm=TRUE),1.1*max(ymin,max(mdat$attM,na.rm=TRUE))) + logOffset
		plot(mdat$Date,mdat$attM+logOffset,type='n',ylim=ylim,xlab='',ylab='', main=paste(predSp[j],preySp[i]),axes=FALSE,log='y')
		ax1.at<-pretty(mdat$Date,6)
		axis(1,at=ax1.at,labels=format(ax1.at,"%b"),las=2)
		eaxis(2)
		box(lwd=1)
		for(p in 1:length(patches)){
			tdat<-subset(dat,Pred==predSp[j] & Prey==preySp[i] & Patch==patches[p])
			points(tdat$Date,tdat$att+logOffset,type='l',col='grey')
		}
		points(mdat$Date,mdat$attM+logOffset,type='o',pch=21,bg=colfoo(mdat$avgTemp), cex=ptscale(mdat$fcnt))
	}}}
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Site and patch-specific feeding rates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf('ExpPatch-Output/ExpPatch_Nucella_Dynamics_FeedRate-Site.pdf',height=8,width=11)
par(mfcol=c(4,3),las=2,cex=0.8,mar=c(4,3,1,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.7,cex.main=0.5)
	ymin<-min(dat$frate[dat$frate!=0],na.rm=TRUE)
	for(j in length(predSp):1){
	for(i in 1:length(preySp)){
		mdat<-subset(site,Pred==predSp[j] & Prey==preySp[i])
		sdat<-subset(dat,Pred==predSp[j] & Prey==preySp[i])
		ylim<-c(min(mdat$frateM,na.rm=TRUE),1.1*max(ymin, max(mdat$frateM,na.rm=TRUE))) # ylim by site max
		plot(mdat$Date,mdat$frateM,type='n',ylim=ylim,xlab='',ylab='', main=paste(predSp[j],preySp[i]),axes=FALSE)
		ax1.at<-pretty(mdat$Date,6)
		axis(1,at=ax1.at,labels=format(ax1.at,"%b"),las=2)
		eaxis(2)
		box(lwd=1)
		for(p in 1:length(patches)){
			tdat<-subset(dat,Pred==predSp[j] & Prey==preySp[i] & Patch==patches[p])
			points(tdat$Date,tdat$frate,type='l',col='grey')
		}
		points(mdat$Date,mdat$frateM,type='o',pch=21,bg=colfoo(mdat$avgTemp), cex=ptscale(mdat$fcnt))
	}}
dev.off()

pdf('ExpPatch-Output/ExpPatch_Nucella_Dynamics_FeedRate-Site_log.pdf',height=8,width=11)
par(mfcol=c(4,3),las=2,cex=0.8,mar=c(4,3,1,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.7,cex.main=0.5)
	for(j in length(predSp):1){
	for(i in 1:length(preySp)){
		mdat<-subset(site,Pred==predSp[j] & Prey==preySp[i])
		sdat<-subset(dat,Pred==predSp[j] & Prey==preySp[i])
	if(sum(sdat$frate)==0){
		plot(mdat$Date,mdat$frateM,type='n',ylim=ylim,xlab='',ylab='', main=paste(predSp[j],preySp[i]),axes=FALSE)
	}
	if(sum(sdat$frate)!=0){
		ymin<-min(sdat$frate[sdat$frate!=0],na.rm=TRUE)
		logOffset<-ymin
		ylim<-c(min(mdat$frateM,na.rm=TRUE),1.1*max(ymin,max(mdat$frateM,na.rm=TRUE))) + logOffset
		plot(mdat$Date,mdat$frateM+logOffset,type='n',ylim=ylim,xlab='',ylab='', main=paste(predSp[j],preySp[i]),axes=FALSE,log='y')
		ax1.at<-pretty(mdat$Date,6)
		axis(1,at=ax1.at,labels=format(ax1.at,"%b"),las=2)
		eaxis(2)
		box(lwd=1)
		for(p in 1:length(patches)){
			tdat<-subset(dat,Pred==predSp[j] & Prey==preySp[i] & Patch==patches[p])
			points(tdat$Date,tdat$frate+logOffset,type='l',col='grey')
		}
		points(mdat$Date,mdat$frateM+logOffset,type='o',pch=21, bg=colfoo(mdat$avgTemp), cex=ptscale(mdat$fcnt))
	}}}
dev.off()


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
################################
# Att & frate vs. prey abundance
################################
pdf('ExpPatch-Output/ExpPatch_Nucella_AttRate_v_Abund-Site.pdf',height=8,width=11)
par(mfcol=c(4,3),las=2,cex=0.8,mar=c(4,3,1,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.7,cex.main=0.5)
	ymin<-min(dat$att[dat$frate!=0],na.rm=TRUE)
	for(j in length(predSp):1){
	for(i in 1:length(preySp)){
		mdat<-subset(site,Pred==predSp[j] & Prey==preySp[i])
		sdat<-subset(dat,Pred==predSp[j] & Prey==preySp[i])
		ylim<-c(min(mdat$attM,na.rm=TRUE),1.01*max(ymin,max(mdat$attM,na.rm=TRUE))) # ylim by site max
		plot(mdat$DensityM,mdat$attM,type='n',ylim=ylim, xlab='',ylab='', main=paste(predSp[j],preySp[i]),axes=FALSE)
		eaxis(1)
		eaxis(2)
		box(lwd=1)
		for(p in 1:length(patches)){
			tdat<-subset(dat,Pred==predSp[j] & Prey==preySp[i] & Patch==patches[p])
			points(tdat$Density,tdat$att,pch=19,col='grey')
		}
		points(mdat$DensityM,mdat$attM,pch=21,bg=colfoo(mdat$avgTemp), cex=ptscale(mdat$fcnt))
	}}
dev.off()

pdf('ExpPatch-Output/ExpPatch_Nucella_FeedRate_v_Abund-Site.pdf',height=8,width=11)
par(mfcol=c(4,3),las=2,cex=0.8,mar=c(4,3,1,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.7,cex.main=0.5)
	ymin<-min(dat$frate[dat$frate!=0],na.rm=TRUE)
	for(j in length(predSp):1){
	for(i in 1:length(preySp)){
		mdat<-subset(site,Pred==predSp[j] & Prey==preySp[i])
		sdat<-subset(dat,Pred==predSp[j] & Prey==preySp[i])
		ylim<-c(min(mdat$frateM,na.rm=TRUE),1.1*max(ymin,max(mdat$frateM,na.rm=TRUE))) # ylim by site max
		plot(mdat$DensityM,mdat$frateM,type='n',ylim=ylim,xlab='',ylab='', main=paste(predSp[j],preySp[i]),axes=FALSE)
		eaxis(1)
		eaxis(2)
		box(lwd=1)
		for(p in 1:length(patches)){
			tdat<-subset(dat,Pred==predSp[j] & Prey==preySp[i] & Patch==patches[p])
			points(tdat$Density,tdat$frate,pch=19,col='grey')
		}
		points(mdat$DensityM,mdat$frateM,pch=21,bg=colfoo(mdat$avgTemp), cex=ptscale(mdat$fcnt))
	}}
dev.off()

###########################################################################
# Comparisons of Mytilus trossulus and Balanus glandula for Nucella ostrina
###########################################################################
Dat<-subset(Dat,Pred=='Nucella_ostrina' & Prey=='Mytilus_trossulus' | Pred=='Nucella_ostrina' & Prey=='Balanus_glandula')
Dat$Prey<-substr(Dat$Prey,1,2)
# Site<-subset(Site,Pred=='Nucella_ostrina' & Prey=='Mytilus_trossulus' | Pred=='Nucella_ostrina' & Prey=='Balanus_glandula')
Site<-subset(Site,Pred=='Nucella_ostrina' & Prey=='Mytilus_trossulus' | Pred=='Nucella_ostrina' & Prey=='Balanus_glandula' | Pred=='Nucella_ostrina' & Prey=='Pollicipes_polymerus' )
Site$Prey<-substr(Site$Prey,1,2)

# Set zero has half minimum non-zero patch-specific value
# so that zeros can get plotted on log axes
f<-2
minatt<-min(Dat$att[Dat$att!=0],na.rm=TRUE)/f
minfrate<-min(Dat$frate[Dat$frate!=0],na.rm=TRUE)/f
Site$attM[Site$attM==0]<-minatt
Site$frateM[Site$frateM==0]<-minfrate
Dat$att[Dat$att==0]<-minatt
Dat$frate[Dat$frate==0]<-minfrate

site<-Site %>%
  gather(variable, value, -(Date:Prey)) %>%
  unite(temp, Prey, variable) %>%
  spread(temp, value)

dat<-Dat %>%
  gather(variable, value, -(Date:Prey)) %>%
  unite(temp, Prey, variable) %>%
  spread(temp, value)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Feeding & attack rates superimposed on abundance
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf('ExpPatch-Output/ExpPatch_Dynamics_Abund_frate-att-MyBaPo.pdf',height=8,width=11)
par(mfcol=c(4,3),las=2,cex=0.8,mar=c(4,3,1,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.7,cex.main=0.5)
	ylim<-c(0,max(site$My_DensityM+site$My_DensitySE,na.rm=TRUE)*1.2)
	plot(site$Date,site$My_DensityM,type='n',xlab='',ylab='',axes=FALSE, main='Mytilus trossulus', ylim=ylim)
	polygon(c(site$Date,rev(site$Date)), c(site$My_DensityM+site$My_DensitySE,rev(site$My_DensityM-site$My_DensitySE)),col='grey90',border=FALSE)
	points(site$Date,site$My_DensityM,type='o',pch=21, bg='grey',col='grey80',xlab='',ylab='Density')
	ax1.at<-pretty(site$Date,6)
	axis(1,at=ax1.at,labels=format(ax1.at,"%b"),las=2)
	axis(2)
	box(lwd=1)
	par(new=TRUE)
	plot(site$Date,site$My_frateM,type='o',pch=21, bg=colfoo(site$My_avgTemp), cex=ptscale(site$My_fcnt),axes=FALSE,xlab='',ylab='')
	axis(4)

	ylim<-c(0,max(site$Ba_DensityM+site$Ba_DensitySE,na.rm=TRUE)*1.2)
	plot(site$Date,site$Ba_DensityM,type='n',xlab='',ylab='',axes=FALSE, main='Balanus glandula', ylim=ylim)
	polygon(c(site$Date,rev(site$Date)), c(site$Ba_DensityM+site$Ba_DensitySE,rev(site$Ba_DensityM-site$Ba_DensitySE)),col='grey90',border=FALSE)
	points(site$Date,site$Ba_DensityM,type='o',pch=21, bg='grey',col='grey80',xlab='',ylab='Density')
	ax1.at<-pretty(site$Date,6)
	axis(1,at=ax1.at,labels=format(ax1.at,"%b"),las=2)
	axis(2)
	box(lwd=1)
	par(new=TRUE)
	plot(site$Date,site$Ba_frateM,type='o',pch=21, bg=colfoo(site$Ba_avgTemp), cex=ptscale(site$Ba_fcnt),axes=FALSE,xlab='',ylab='')
	axis(4)

	ylim<-c(0,max(site$Po_DensityM+site$Po_DensitySE,na.rm=TRUE)*1.2)
	plot(site$Date,site$Po_DensityM,type='n',xlab='',ylab='',axes=FALSE, main='Pollicipes polymerus', ylim=ylim)
	polygon(c(site$Date,rev(site$Date)), c(site$Po_DensityM+site$Po_DensitySE,rev(site$Po_DensityM-site$Po_DensitySE)),col='grey90',border=FALSE)
	points(site$Date,site$Po_DensityM,type='o',pch=21, bg='grey',col='grey80',xlab='',ylab='Density')
	ax1.at<-pretty(site$Date,6)
	axis(1,at=ax1.at,labels=format(ax1.at,"%b"),las=2)
	axis(2)
	box(lwd=1)
	par(new=TRUE)
	plot(site$Date,site$Po_frateM,type='o',pch=21, bg=colfoo(site$Po_avgTemp), cex=ptscale(site$Po_fcnt),axes=FALSE,xlab='',ylab='')
	axis(4)

	#~~~~~~~~~~~~
	# Attack rate
	#~~~~~~~~~~~~
	ylim<-c(0,max(site$My_DensityM+site$My_DensitySE,na.rm=TRUE)*1.2)
	plot(site$Date,site$My_DensityM,type='n',xlab='',ylab='',axes=FALSE, main='Mytilus trossulus', ylim=ylim)
	polygon(c(site$Date,rev(site$Date)), c(site$My_DensityM+site$My_DensitySE,rev(site$My_DensityM-site$My_DensitySE)),col='grey90',border=FALSE)
	points(site$Date,site$My_DensityM,type='o',pch=21, bg='grey',col='grey80',xlab='',ylab='Density')
	ax1.at<-pretty(site$Date,6)
	axis(1,at=ax1.at,labels=format(ax1.at,"%b"),las=2)
	axis(2)
	box(lwd=1)
	par(new=TRUE)
	plot(site$Date,site$My_attM,type='o',pch=21, bg=colfoo(site$My_avgTemp), cex=ptscale(site$My_fcnt),axes=FALSE,xlab='',ylab='',log='y')
	ax4.at<-pretty(site$My_attM,6)
	eaxis(4,max.at=5)

	ylim<-c(0,max(site$Ba_DensityM+site$Ba_DensitySE,na.rm=TRUE)*1.2)
	plot(site$Date,site$Ba_DensityM,type='n',xlab='',ylab='',axes=FALSE, main='Balanus glandula', ylim=ylim)
	polygon(c(site$Date,rev(site$Date)), c(site$Ba_DensityM+site$Ba_DensitySE,rev(site$Ba_DensityM-site$Ba_DensitySE)),col='grey90',border=FALSE)
	points(site$Date,site$Ba_DensityM,type='o',pch=21, bg='grey',col='grey80',xlab='',ylab='Density')
	ax1.at<-pretty(site$Date,6)
	axis(1,at=ax1.at,labels=format(ax1.at,"%b"),las=2)
	axis(2)
	box(lwd=1)
	par(new=TRUE)
	plot(site$Date,site$Ba_attM,type='o',pch=21, bg=colfoo(site$Ba_avgTemp), cex=ptscale(site$Ba_fcnt),axes=FALSE,xlab='',ylab='',log='y')
	ax4.at<-pretty(site$Ba_attM,4)
	eaxis(4,max.at=5)

	ylim<-c(0,max(site$Po_DensityM+site$Po_DensitySE,na.rm=TRUE)*1.2)
	plot(site$Date,site$Po_DensityM,type='n',xlab='',ylab='',axes=FALSE, main='Pollicipes polymerus', ylim=ylim)
	polygon(c(site$Date,rev(site$Date)), c(site$Po_DensityM+site$Po_DensitySE,rev(site$Po_DensityM-site$Po_DensitySE)),col='grey90',border=FALSE)
	points(site$Date,site$Po_DensityM,type='o',pch=21, bg='grey',col='grey80',xlab='',ylab='Density')
	ax1.at<-pretty(site$Date,6)
	axis(1,at=ax1.at,labels=format(ax1.at,"%b"),las=2)
	axis(2)
	box(lwd=1)
	par(new=TRUE)
	plot(site$Date,site$Po_attM,type='o',pch=21, bg=colfoo(site$Po_avgTemp), cex=ptscale(site$Po_fcnt),axes=FALSE,xlab='',ylab='',log='y')
	eaxis(4,max.at=5)
dev.off()

#~~~~~~~~~~~~~
# Attack rates
#~~~~~~~~~~~~~
pdf('ExpPatch-Output/ExpPatch_No_Mt_vs_Bg_att.pdf',height=3,width=4)
par(pty='s',las=2,cex=0.8,mar=c(4,4,1,1),tcl=-0.2, mgp=c(3,0.3,0), cex.axis=0.7,cex.main=0.5)
	plot(dat$Ba_att,dat$My_att,xlab='Balanus glandula',ylab='Mytilus trossulus',log='xy',pch=19, col='grey', cex=0.5, axes=FALSE)
	points(site$Ba_attM,site$My_attM,pch=21, bg=colfoo(site$Ba_avgTemp),cex=1.25)
	eaxis(1,drop.1=TRUE,sub10=TRUE,max.at=3)
	eaxis(2,drop.1=TRUE,sub10=TRUE,max.at=3)
	box(lwd=1)
dev.off()

#~~~~~~~~~~~~
# Feeding rate
#~~~~~~~~~~~~
pdf('ExpPatch-Output/ExpPatch_No_Mt_vs_Bg_frate.pdf',height=3,width=4)
par(pty='s',las=2,cex=0.8,mar=c(4,4,1,1),tcl=-0.2, mgp=c(3,0.3,0), cex.axis=0.7,cex.main=0.5)
	plot(dat$Ba_frate,dat$My_frate,xlab='Balanus glandula',ylab='Mytilus trossulus',log='xy',pch=19, col='grey', cex=0.5, axes=FALSE)
	points(site$Ba_frateM,site$My_frateM,pch=21, bg=colfoo(site$Ba_avgTemp),cex=1.25)
	eaxis(1,drop.1=TRUE,sub10=TRUE,max.at=3)
	eaxis(2,drop.1=TRUE,sub10=TRUE,max.at=3)
	box(lwd=1)
dev.off()

###############################################################
# Ratio and proportion of feeding, attack rates and abundances
###############################################################
# Remove minatt and minfrate values again
Site$attM[Site$attM==minatt]<-0
Site$frateM[Site$frateM==minfrate]<-0
Dat$att[Dat$att==minatt]<-0
Dat$frate[Dat$frate==minfrate]<-0

site<-Site %>%
  gather(variable, value, -(Date:Prey)) %>%
  unite(temp, Prey, variable) %>%
  spread(temp, value)

dat<-Dat %>%
  gather(variable, value, -(Date:Prey)) %>%
  unite(temp, Prey, variable) %>%
  spread(temp, value)

dat$frate.rat<-as.numeric(dat$My_frate)/as.numeric(dat$Ba_frate)
dat$att.rat<-as.numeric(dat$My_att)/as.numeric(dat$Ba_att)
dat$fcnt.rat<-as.numeric(dat$My_fcnt)/as.numeric(dat$Ba_fcnt)
dat$Dens.rat<-as.numeric(dat$My_Density)/as.numeric(dat$Ba_Density)
dat$htime.rat<-as.numeric(dat$My_htime)/as.numeric(dat$Ba_htime)

dat$frate.prop<-as.numeric(dat$My_frate)/ (as.numeric(dat$Ba_frate)+as.numeric(dat$My_frate))
dat$fcnt.prop<-as.numeric(dat$My_fcnt)/ (as.numeric(dat$Ba_fcnt)+as.numeric(dat$My_fcnt))
dat$att.prop<-as.numeric(dat$My_att)/ (as.numeric(dat$Ba_att)+as.numeric(dat$My_att))
dat$Dens.prop<-as.numeric(dat$My_Density)/ (as.numeric(dat$Ba_Density)+as.numeric(dat$My_Density))

site$frate.rat<-as.numeric(site$My_frateM)/as.numeric(site$Ba_frateM)
site$att.rat<-as.numeric(site$My_attM)/as.numeric(site$Ba_attM)
site$fcnt.rat<-as.numeric(site$My_fcnt)/as.numeric(site$Ba_fcnt)
site$htime.rat<-as.numeric(site$My_htimeM)/as.numeric(site$Ba_htimeM)
site$Dens.rat<-as.numeric(site$My_DensityM)/as.numeric(site$Ba_DensityM)

site$frate.prop<-as.numeric(site$My_frateM)/ (as.numeric(site$Ba_frateM)+as.numeric(site$My_frateM))
site$att.prop<-as.numeric(site$My_attM)/ (as.numeric(site$Ba_attM)+as.numeric(site$My_attM))
site$fcnt.prop<-as.numeric(site$My_fcnt)/ (as.numeric(site$Ba_fcnt)+as.numeric(site$My_fcnt))
site$Dens.prop<-as.numeric(site$My_DensityM)/ (as.numeric(site$Ba_DensityM)+as.numeric(site$My_DensityM))

site[is.na(site)]<-0

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Feeding rate proportions vs. abund proportions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to plot isobars of equal preference for Sp. A
# p = preference,  A = % of A available
Fprop<-function(A){p*A/((1-A)+p*A)}

pref<-function(Fprop,densProp){(Fprop*(densProp-1))/(densProp*(Fprop-1))}
site$Pref<-pref(site$frate.prop,site$Dens.prop)
dat$Pref<-pref(dat$frate.prop,dat$Dens.prop)
meanPref<-weighted.mean(site$Pref,site$My_tObs)

ax.at<-seq(0,1,0.2)
pdf('ExpPatch-Output/ExpPatch_No_Mt_vs_Bg_frateProp_vs_densProps.pdf',height=3,width=4)
par(pty='s',cex=0.8,mar=c(4,4,1.5,1),tcl=-0.2, mgp=c(1.5,0.35,0), cex.axis=0.7,cex.main=0.5,las=1)
	plot(dat$Dens.prop,dat$frate.prop,xlab='Available (% Mytilus)',ylab='Feeding rate (% Mytilus)',type='n', axes=FALSE, xlim=c(0,1),ylim=c(0,1))
	axis(1,at=ax.at,labels=100*ax.at,mgp=c(0,0.1,0)); axis(2,at=ax.at,labels=100*ax.at);box(lwd=1)
	abline(0,1,col='grey40',lty=2)
	points(dat$Dens.prop,dat$frate.prop,pch=19, col='grey', cex=0.5)
	points(site$Dens.prop,site$frate.prop,pch=21, bg=colfoo(site$Ba_avgTemp),cex=ptscale(site$My_tObs))
dev.off()

pdf('ExpPatch-Output/ExpPatch_No_Mt_vs_Bg_frateProp_vs_densProps_fittedIsoBar.pdf',height=3,width=4)
par(pty='s',cex=0.8,mar=c(4,4,1.5,1),tcl=-0.2, mgp=c(1.5,0.35,0), cex.axis=0.7,cex.main=0.5,las=1)
	plot(dat$Dens.prop,dat$frate.prop,xlab='Available (% Mytilus)',ylab='Feeding rate (% Mytilus)',type='n', axes=FALSE, xlim=c(0,1),ylim=c(0,1))
	axis(1,at=ax.at,labels=100*ax.at,mgp=c(0,0.1,0)); axis(2,at=ax.at,labels=100*ax.at);box(lwd=1)
	abline(0,1,col='grey40',lty=2)
	p=meanPref
	points(dat$Dens.prop,dat$frate.prop,pch=19, col='grey', cex=0.5)
	curve(Fprop,0,1,add=TRUE,col='black',n=200,lty=1,lwd=2)
	points(site$Dens.prop,site$frate.prop,pch=21, bg=colfoo(site$Ba_avgTemp), cex=ptscale(site$My_tObs))
dev.off()

pdf('ExpPatch-Output/ExpPatch_No_Mt_vs_Bg_frateProp_vs_densProps_Isobars.pdf',height=3,width=4)
par(pty='s',cex=0.8,mar=c(4,4,1.5,1),tcl=-0.2, mgp=c(1.5,0.35,0), cex.axis=0.7,cex.main=0.5,las=1)
	plot(dat$Dens.prop,dat$frate.prop,xlab='Available (% Mytilus)',ylab='Feeding rate (% Mytilus)',type='n', axes=FALSE, xlim=c(0,1),ylim=c(0,1))
	axis(1,at=ax.at,labels=100*ax.at,mgp=c(0,0.1,0)); axis(2,at=ax.at,labels=100*ax.at);box(lwd=1)
	# Isobars of constant attack rate (preference) for Mytilus
		Seq<-c(1,2,5,20,100,500)
			for(i in 1:length(Seq)){
				p=Seq[i]
				curve(Fprop,0,1,add=TRUE,col='grey80',n=200,lty=1)
				xat<-c(0.5,0.41,0.31,0.19,0.12,0.07)
				yat<-c(0.5,0.59,0.7,0.83,0.94,0.99)
				text(xat,yat,Seq,srt=45,col='grey',cex=0.7,pos=2,offset=0.3)
			}
	p=meanPref
	curve(Fprop,0,1,add=TRUE,col='black',n=200,lty=1,lwd=2)
	points(site$Dens.prop,site$frate.prop,pch=21, bg=colfoo(site$Ba_avgTemp), cex=ptscale(site$My_tObs))
dev.off()

pdf('ExpPatch-Output/ExpPatch_No_Mt_vs_Bg_frateProp_vs_densProps_IsobarsOnly.pdf',height=3,width=4)
par(pty='s',cex=0.8,mar=c(4,4,1.5,1),tcl=-0.2, mgp=c(1.5,0.35,0), cex.axis=0.7,cex.main=0.5,las=1)
	plot(dat$Dens.prop,dat$frate.prop,xlab='Available (% Mytilus)',ylab='Feeding rate (% Mytilus)',type='n', axes=FALSE, xlim=c(0,1),ylim=c(0,1))
	axis(1,at=ax.at,labels=100*ax.at,mgp=c(0,0.1,0)); axis(2,at=ax.at,labels=100*ax.at);box(lwd=1)
	for(i in 1:length(Seq)){
		p=Seq[i]
		curve(Fprop,0,1,add=TRUE,col='grey50',n=200,lty=1)
		xat<-c(0.5,0.41,0.31,0.19,0.12,0.07)
		yat<-c(0.5,0.59,0.7,0.83,0.94,0.99)
		text(xat,yat,Seq,srt=45,col='grey',cex=0.7,pos=2,offset=0.3)
	}
dev.off()


# tsite<-site[site$Pref!=0,]
# tsite$wt<-tsite$Ba_fcnt+tsite$My_fcnt
# fit1<-nls(log(Pref)~P0*Dens.prop^b,data=tsite,list(P0=5,b=1),weights=tsite$wt)
# fit2<-nls(log(Pref)~(a+b*Dens.prop)/(1+c*Dens.prop),data=tsite,list(a=5,b=1,c=1),weights=tsite$wt)
# fit3<-nls(log(Pref)~(1+a*Dens.prop)/(b+c*Dens.prop),data=tsite,list(a=5,b=1,c=10),weights=tsite$wt)
# fit<-fit1
# parms<-coef(fit)
#
# varP1<-function(A){exp(parms[1]*A^parms[2])}
# varP2<-function(A){exp((parms[1]+parms[2]*A)/(1+parms[3]*A))}
# varP3<-function(A){exp((1+parms[1]*A)/(parms[2]+parms[3]*A))}
# varP<-varP1
#
# Fprop.varP<-function(A){varP(A)*A/((1-A)+varP(A)*A)}
#

# Polynomial
fit<-lm(frate.prop~poly(Dens.prop,2),data=site,weights=My_tObs)

r <- range(site$Dens.prop)
xNew <- seq(r[1],r[2],length.out = 200)
yNew <- predict(fit,list(Dens.prop = xNew))

pdf('ExpPatch-Output/ExpPatch_No_Mt_vs_Bg_frateProp_vs_densProps_Isobars_polyfit.pdf',height=3,width=4)
par(pty='s',las=1,cex=0.8,mar=c(4,4,1.5,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.7,cex.main=0.5)
	plot(dat$Dens.prop,dat$frate.prop,xlab='Available (% Mytilus)',ylab='Feeding rate (% Mytilus)',type='n', axes=FALSE, xlim=c(0,1),ylim=c(0,1))
	axis(1,at=ax.at,labels=100*ax.at,mgp=c(0,0.1,0)); axis(2,at=ax.at,labels=100*ax.at);box(lwd=1)

	# Isobars of constant attack rate (preference) for Mytilus
		Seq<-c(1,2,5,20,100,500)
			for(i in 1:length(Seq)){
				p=Seq[i]
				curve(Fprop,0,1,add=TRUE,col='grey80',n=200,lty=1)
				xat<-c(0.5,0.41,0.31,0.19,0.12,0.07)
				yat<-c(0.5,0.59,0.7,0.83,0.94,0.99)
				text(xat,yat,Seq,srt=45,col='grey',cex=0.7,pos=2,offset=0.3)
			}
	p=meanPref
# 	curve(Fprop,0,1,add=TRUE,col='grey',n=200,lty=2,lwd=1)
# 	curve(Fprop.varP,0,1,add=TRUE,col='black',n=250,lty=1,lwd=2)
	points(site$Dens.prop,site$frate.prop,pch=21, bg=colfoo(site$Ba_avgTemp), cex=ptscale(site$My_tObs))
	lines(xNew,yNew,lwd=2)
dev.off()

ax.at=seq(0,0.4,0.1)
pdf('ExpPatch-Output/ExpPatch_No_Mt_vs_Bg_Prefcalc_vs_densProps-site.pdf',height=3,width=4)
par(las=1,cex=0.8,mar=c(4,4,1.5,1),tcl=-0.2, mgp=c(2,0.3,0), cex.axis=0.7,cex.main=0.5)
	plot(site$Dens.prop,site$Pref,type='n',xlab='Available (% Mytilus)',ylab='Preference for Mytilus',axes=FALSE)#,ylim=c(0.8,800))
# 	lines(xNew,exp(yNew),lwd=2)
# 	points(dat$Dens.prop,dat$Pref,pch=19, col='grey', cex=0.5)
	points(site$Dens.prop,site$Pref,pch=21,bg=colfoo(site$Ba_avgTemp), cex=ptscale(site$My_tObs))
	axis(1,at=ax.at,labels=100*ax.at,mgp=c(0,0.1,0))
	axis(2)
	box(lwd=1)
dev.off()

ax.at=seq(0,1,0.2)
pdf('ExpPatch-Output/ExpPatch_No_Mt_vs_Bg_Prefcalc_vs_densProps.pdf',height=3,width=4)
par(las=1,cex=0.8,mar=c(4,4,1.5,1),tcl=-0.2, mgp=c(2,0.3,0), cex.axis=0.7,cex.main=0.5)
	plot(dat$Dens.prop,dat$Pref,type='n',xlab='Available (% Mytilus)',ylab='Preference for Mytilus',axes=FALSE,log='y',xlim=c(0,1),ylim=c(1E-2,2E2))
	points(dat$Dens.prop,dat$Pref,pch=19, col='grey', cex=0.5)
	points(site$Dens.prop,site$Pref,pch=21,bg=colfoo(site$Ba_avgTemp), cex=ptscale(site$My_tObs))
	axis(1,at=ax.at,labels=100*ax.at,mgp=c(0,0.1,0))
	eaxis(2,max.at=5)
	box(lwd=1)
dev.off()

################################################
# Set zero has half minimum non-zero patch-specific value
# so that zeros can get plotted on log axes
f<-2
minDensRat<-min(dat$Dens.rat[dat$Dens.rat!=0],na.rm=TRUE)
minFrateRat<-min(dat$frate.rat[dat$frate.rat!=0],na.rm=TRUE)/f
minattRat<-min(dat$att.rat[dat$att.rat!=0],na.rm=TRUE)/f
dat$Dens.rat[dat$Dens.rat==0]<-minDensRat
dat$frate.rat[dat$frate.rat==0]<-minFrateRat
dat$att.rat[dat$att.rat==0]<-minattRat
site$Dens.rat[site$Dens.rat==0]<-minDensRat
site$frate.rat[site$frate.rat==0]<-minFrateRat
site$att.rat[site$att.rat==0]<-minattRat

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Feeding rate ratios vs. abund ratios
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf('ExpPatch-Output/ExpPatch_No_Mt_vs_Bg_frateRatio_vs_densRatio.pdf',height=3,width=4)
par(pty='s',las=2,cex=0.8,mar=c(4,4,1.5,1),tcl=-0.2, mgp=c(3,0.3,0), cex.axis=0.7,cex.main=0.5)
	plot(dat$Dens.rat,dat$frate.rat,xlab='Available (Mytilus/Balanus)',ylab='Feeding rate (Mytilus/Balanus)',log='xy',type='n', axes=FALSE,ylim=c(1E-2,2E1), xlim=c(1E-3,2E1))
	abline(0,1,col='grey40',lty=2)
	points(dat$Dens.rat,dat$frate.rat,pch=19, col='grey', cex=0.5)
	points(site$Dens.rat,site$frate.rat,pch=21, bg=colfoo(site$Ba_avgTemp), cex=ptscale(site$My_tObs))
	eaxis(1,drop.1=TRUE,sub10=TRUE,max.at=3)
	eaxis(2,drop.1=TRUE,sub10=TRUE,max.at=4)
	eaxis(3,drop.1=TRUE,sub10=TRUE,max.at=3)
	eaxis(4,drop.1=TRUE,sub10=TRUE,max.at=4)
	box(lwd=1)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Attack rate proportions vs. abund proportions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf('ExpPatch-Output/ExpPatch_No_Mt_vs_Bg_attProp_vs_densProps.pdf',height=3,width=4)
par(pty='s',las=2,cex=0.8,mar=c(4,4,1.5,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.7,cex.main=0.5)
	plot(dat$Dens.prop,dat$att.prop,xlab='Available (% Mytilus)',ylab='Attack rate (% Mytilus)',type='n', axes=TRUE, xlim=c(0,1),ylim=c(0,1))
	abline(0,1,col='grey40',lty=2)
	points(dat$Dens.prop,dat$att.prop,pch=19, col='grey', cex=0.5)
	points(site$Dens.prop,site$att.prop,pch=21, bg=colfoo(site$Ba_avgTemp), cex=ptscale(site$My_tObs))
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Attack rate ratios vs. abund ratios
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# tdat<-dat[-which(is.na(dat$att.rat) | is.infinite(dat$att.rat) | is.na(dat$Dens.rat) | is.infinite(dat$Dens.rat)| dat$Dens.rat==minDensRat | dat$att.rat==minattRat),]
# fit<-lm(log10(tdat$att.rat)~log10(tdat$Dens.rat))

pdf('ExpPatch-Output/ExpPatch_No_Mt_vs_Bg_attRatio_vs_densRatio.pdf',height=3,width=4)
par(pty='s',las=2,cex=0.8,mar=c(4,4,1.5,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.7,cex.main=0.5)
	plot(dat$Dens.rat,dat$att.rat,xlab='Available (Mytilus/Balanus)',ylab='Attack rate (Mytilus/Balanus)',log='xy',type='n', axes=FALSE,xlim=c(1E-3,2E1), ylim=c(1E-2,2E2))
	abline(0,1,col='grey40',lty=2)
# 	abline(fit)
	points(dat$Dens.rat,dat$att.rat,pch=19, col='grey', cex=0.5)
	points(site$Dens.rat,site$att.rat,pch=21, bg=colfoo(site$Ba_avgTemp), cex=ptscale(site$My_tObs))
	eaxis(1,drop.1=TRUE,sub10=TRUE,max.at=3)
	eaxis(2,drop.1=TRUE,sub10=TRUE,max.at=3)
	eaxis(3,drop.1=TRUE,sub10=TRUE,max.at=3)
	eaxis(4,drop.1=TRUE,sub10=TRUE,max.at=3)
	box(lwd=1)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Feeding count proportions vs. abund proportions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf('ExpPatch-Output/ExpPatch_No_Mt_vs_Bg_fcntProp_vs_densProps.pdf',height=3,width=4)
par(pty='s',las=2,cex=0.8,mar=c(4,4,1.5,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.7,cex.main=0.5)
	plot(dat$Dens.prop,dat$fcnt.prop,xlab='Available (% Mytilus)',ylab='Feeding count (% Mytilus)',type='n', axes=TRUE, xlim=c(0,1),ylim=c(0,1))
	abline(0,1,col='grey40',lty=2)
	points(dat$Dens.prop,dat$fcnt.prop,pch=19, col='grey', cex=0.5)
	points(site$Dens.prop,site$fcnt.prop,pch=21, bg=colfoo(site$Ba_avgTemp), cex=ptscale(site$My_tObs))
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Feeding count ratios vs. abund ratios
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf('ExpPatch-Output/ExpPatch_No_Mt_vs_Bg_fcntRatio_vs_densRatio.pdf',height=3,width=4)
par(pty='s',las=2,cex=0.8,mar=c(4,4,1.5,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.7,cex.main=0.5)
	plot(dat$Dens.rat,dat$fcnt.rat,xlab='Available (Mytilus/Balanus)',ylab='Feeding count (Mytilus/Balanus)',log='xy',type='n', axes=FALSE)
	abline(0,1,col='grey40',lty=2)
	points(dat$Dens.rat,dat$fcnt.rat,pch=19, col='grey', cex=0.5)
	points(site$Dens.rat,site$fcnt.rat,pch=21, bg=colfoo(site$Ba_avgTemp), cex=ptscale(site$My_tObs))
	eaxis(1,drop.1=TRUE,sub10=TRUE,max.at=5)
	eaxis(2,drop.1=TRUE,sub10=TRUE,max.at=5)
	eaxis(3,drop.1=TRUE,sub10=TRUE,max.at=5)
	eaxis(4,drop.1=TRUE,sub10=TRUE,max.at=5)
	box(lwd=1)
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Handling time ratios vs. abund ratios
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf('ExpPatch-Output/ExpPatch_No_Mt_vs_Bg_htimeRatio_vs_densRatio.pdf',height=3,width=4)
par(pty='s',las=2,cex=0.8,mar=c(4,4,1.5,1),tcl=-0.2, mgp=c(2.5,0.3,0), cex.axis=0.7,cex.main=0.5)
	plot(dat$Dens.rat,dat$htime.rat,xlab='Available (Mytilus/Balanus)',ylab='Handling time (Mytilus/Balanus)',log='xy',type='n', axes=FALSE)
	abline(0,1,col='grey40',lty=2)
	points(dat$Dens.rat,dat$htime.rat,pch=19, col='grey', cex=0.5)
	points(site$Dens.rat,site$htime.rat,pch=21, bg=colfoo(site$Ba_avgTemp), cex=ptscale(site$My_tObs))
	eaxis(1,drop.1=TRUE,sub10=TRUE,max.at=5)
	eaxis(2,drop.1=TRUE,sub10=TRUE,max.at=5)
	eaxis(3,drop.1=TRUE,sub10=TRUE,max.at=5)
	eaxis(4,drop.1=TRUE,sub10=TRUE,max.at=5)
	box(lwd=1)
dev.off()



##########################################################################################
##########################################################################################
##########################################################################################
