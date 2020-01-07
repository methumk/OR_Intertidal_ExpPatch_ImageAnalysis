########################################################
########################################################
# Summarize and plot species dynamics from photographed
# and counted experimental patch plots
########################################################
########################################################
rm(list=ls()) # clears workspace
options(stringsAsFactors=F)

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

###############################
# Preliminary time-series plots
###############################
spp<-sort(unique(StatsSite$Species))

pdf('ExpPatch-Output/ExpPatch_PhotoCounts_SiteLevel.pdf',height=8,width=11)
par(mfcol=c(4,3),las=2,cex=0.8,mar=c(4,3,1,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.7,cex.main=0.5)
for(s in 1:length(spp)){
	tSpp<-subset(StatsSite,Species==spp[s])
	maxy<-1.2*max(tSpp$Density[,'Mean']+tSpp$Density[,'SE'],na.rm=TRUE)
	plot(tSpp$Date,(tSpp$Density[,'Mean']+tSpp$Density[,'SE']), xlab='',ylab='', type='n',main=spp[s], xaxt='n',ylim=c(0,maxy))
	ax1.at<-pretty(tSpp$Date,6)
	axis(1,at=ax1.at,labels=format(ax1.at,"%b"),las=2)
# 	axis.Date(1,at=xat)
	polygon(c(tSpp$Date,rev(tSpp$Date)), c(tSpp$Density[,'Mean']+tSpp$Density[,'SE'], rev(tSpp$Density[,'Mean']-tSpp$Density[,'SE'])), col='grey90')
	points(tSpp$Date,tSpp$Density[,'Mean'],type='o',pch=21,bg='grey')
# 	text(tSpp$Date,maxy*0.98,tSpp$Density[,'n'],cex=0.6)
}
dev.off()

xlim<-range(StatsPatch$Date)
pdf('ExpPatch-Output/ExpPatch_PhotoCounts_ByPatchSpecies.pdf',height=8,width=11)
par(mfrow=c(6,5),las=2,cex=0.8,mar=c(4,3,1,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.8,cex.main=0.8)
	for(p in 1:length(Patches)){
		tPatch<-subset(StatsPatch,Patch==Patches[p])
	for(s in 1:length(spp)){
		tSpp<-subset(tPatch,Species==spp[s])
		if(nrow(tSpp)==0){plot(1,1,axes=FALSE,xlab='',ylab='',type='n')}
		if(nrow(tSpp)>0){
		maxy<-max(tSpp$Density[,'Mean']+tSpp$Density[,'SE'])
		plot(tSpp$Date,(tSpp$Density[,'Mean']+tSpp$Density[,'SE']), xlab='',ylab='', type='n', main=paste(Patches[p],spp[s],sep='.'), xlim=xlim)
		polygon(c(tSpp$Date,rev(tSpp$Date)), c(tSpp$Density[,'Mean']+tSpp$Density[,'SE'], rev(tSpp$Density[,'Mean']-tSpp$Density[,'SE'])), col='grey90')
		points(tSpp$Date,tSpp$Density[,'Mean'],type='o',pch=21,bg='grey')
		text(tSpp$Date,maxy*0.98,tSpp$Density[,'n'],cex=0.6)
	}}}
dev.off()


xlim<-range(StatsPatch$Date)
pdf('ExpPatch-Output/ExpPatch_PhotoCounts_BySpeciesPatch.pdf',height=5,width=11)
par(mfrow=c(3,6),las=2,cex=0.8,mar=c(4,3,1,1),tcl=-0.2, mgp=c(1.5,0.3,0), cex.axis=0.8,cex.main=0.8)
	for(s in 1:length(spp)){
		tSpp<-subset(StatsPatch,Species==spp[s])
	for(p in 1:length(Patches)){
		tPatch<-subset(tSpp,Patch==Patches[p])
		if(nrow(tPatch)==0){plot(1,1,axes=FALSE,xlab='',ylab='',type='n')}
		if(nrow(tPatch)>0){
		maxy<-max(tPatch$Density[,'Mean']+tPatch$Density[,'SE'])
		plot(tPatch$Date,(tPatch$Density[,'Mean']+tPatch$Density[,'SE']), xlab='',ylab='', type='n', main=paste(Patches[p],spp[s],sep='.'), xlim=xlim)
		polygon(c(tPatch$Date,rev(tPatch$Date)), c(tPatch$Density[,'Mean']+tPatch$Density[,'SE'], rev(tPatch$Density[,'Mean']-tPatch$Density[,'SE'])), col='grey90')
		points(tPatch$Date,tPatch$Density[,'Mean'],type='o',pch=21,bg='grey')
		text(tPatch$Date,maxy*0.98,tPatch$Density[,'n'],cex=0.6)
	}}}
dev.off()

##################################
# Effort and data completion plots
##################################
# PQD and DQD come from PhotoCountParser script
# ###### Quads done
target<-3 # target quads per patch
mat<-t(PQD)
T<-ncol(mat)
nP<-nrow(mat)
mat<-mat[sort(rownames(mat)),]
pmat<-mat<-mat[nP:1,]
pmat[which(pmat>target)]<-target

ymx<-length(Patches)*target
series<-yats<-c(0,cumsum(rep(3,nP-1)))

pdf('ExpPatch-Output/ExpPatch_PhotoCounts_PatchQuadsDone.pdf',height=6,width=8)
	par(mar=c(5.5,4,1,1),mgp=c(2,0.4,0),tcl=-0.2)
	plot(1,1,ylim=c(0,ymx),xlim=c(0,T),type='n',xlab='',ylab='Quads completed',axes=FALSE,yaxs='i',xaxs='i')
	axis(1,at=1:ncol(mat),labels=colnames(mat),las=2)
	axis(2,yats+1.5,labels=rownames(mat),las=1,tcl=0,mgp=c(0,0.2,0))
	tcks<-rep(c(1,2,3),length(Patches))
	axis(4,at=1:ymx,labels=rep(c(1,2,3),length(Patches)),cex.axis=0.5,las=2)
	abline(v=1:T,col='grey',lty=3,lwd=0.5)
	box(lwd=2)
	abline(h=series)

	zT<-rep(0,T)
	for(i in 1:nP){
		polygon(c(1:T,T:1),c(series[i]+pmat[i,],zT+series[i]),col='grey')
		m<-which(mat[i,]>target)
		if(length(m)>0){text(m,rep(series[i]+1.5,length(m)),mat[i,m],cex=0.8)}
	}
dev.off()


###############
# Person effort
###############
# pdf('ExpPatch-Output/ExpPatch_PersonEffort.pdf',height=5,width=8)
# par(mfrow=c(2,3),cex=0.8,mar=c(4,4.5,1,1),mgp=c(3.2,0.2,0),tcl=-0.1)
# 	pe<-table(inf$Counter)
# 	oa<-order(pe,decreasing=TRUE)
# 	pe<-pe[oa]
# 	nams<-sapply(strsplit(names(pe),' '), '[', 1)
# 	barplot(pe,las=2,ylab='Quadrats counted (full & crop)',names=nams)
#
# 	hrs<-data.frame(Name=c('Beatriz','Julia','Stephanie','Isaac','Unknown','Unknown'),  Hours=c(576.5,409,673.5,NA,NA,NA)) # Data from 4/3/2014-1/29/2015
# 	eff<-pe/hrs$Hours
# 	barplot(eff,las=2,ylab='Quadrats/hr',names=nams)
#
# 	inveff<-1/eff
# 	barplot(inveff,las=2,ylab='Hours/quad',names=nams)
#
# 	pspe<-aggregate(list(Count=dat$Count), by=list(Counter=dat$Counter,Species=dat$Species),sum)
# 	# Balanus
# 	B<-subset(pspe,Species=='Balanus glandula')
# 	barplot(B[oa,]$Count,las=2,ylab='Balanus glandula counted',names=nams)
# 	# Mussels
# 	M<-pspe[grep('Mytilus',pspe$Species),]
# 	M<-aggregate(list(Count=M$Count),by=list(Counter=M$Counter),sum)
# 	barplot(M[oa,]$Count,las=2,ylab='Mytilus counted',names=nams)
# dev.off()

######################################################################
######################################################################
######################################################################
