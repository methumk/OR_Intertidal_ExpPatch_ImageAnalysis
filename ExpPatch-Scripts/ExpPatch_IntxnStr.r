##############################################################################
##############################################################################
# Estimate point estimates (means) for feeding and attack rates of
# Nucella ostrina in the experimental patches
##############################################################################
##############################################################################
rm(list=ls()) # clears workspace
options(stringsAsFactors=F)
if("RCurl" %in% loadedNamespaces()){detach("package:RCurl", unload=TRUE)}# Interferes with "complete" from tidyr
library(plyr)
library(tidyr)

# Load convenience functions
source('/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches/ExpPatch-Scripts/ExpPatch_MiscFunctions.r')

###########################################################
# Update data files (assuming db tables have been exported)
###########################################################
# source('/Volumes/NovakLab/Projects/OR_Intertidal/FeedingObs_db/FeedingObs_db-Scripts/FeedingObs_db-Parse.r')
# source('/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches/ExpPatch-Scripts/ExpPatch_PhotoCountParser.r')
# source('/Volumes/NovakLab/Projects/OR_Intertidal/Temperature/Temp-Scripts/Temp-ProcessRaw.r')

#############
# Import data
#############
setwd('/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches/')
abunds<-read.csv('ExpPatch-Data/ExpPatch_PhotoCounts.csv')
fobs<-read.csv('ExpPatch-Data/ExpPatch_FeedingObs-Obs.csv')
htime_match<-read.csv('ExpPatch-Info/ExpPatch_NZ-OR_HtimeMatch.txt')
htime_reg<-read.csv('ExpPatch-Data/NZ-HTimeRegns/NZ-HandlingTimes-MRegnCoeff-MeasuredANDMatched.csv',skip=3)
tempC<-read.csv('../Temperature/Temp-Data/TempsCleaned_YB_Site_Day.csv')

################################################
# Data subsets, species combinations & matching
################################################
# Drop unnecessary columns
abunds<-abunds[,c('Date','Patch','Quad','Species','Density')]
fobs<-fobs[,c('Date','Patch','Pred','Prey', 'PredSize','PreySize','Drilling')]
abunds$Date<-as.Date(abunds$Date)
fobs$Date<-as.Date(fobs$Date)

# Rename "Mytilus trossulus 2" (the combination of Mytilus spp. and Mytilus trossulus)
abunds$Species[grep("^Mytilus_trossulus$",abunds$Species)]<-"Mytilus_trossulus1"
abunds$Species[grep("^Mytilus_trossulus2$",abunds$Species)]<-"Mytilus_trossulus"

fobs$Prey[grep('Lottia',fobs$Prey)]<-'Limpets'
abunds<-abunds[which(abunds$Species %in% unique(fobs$Prey)),]
fobsSp<-sort(unique(fobs$Prey))
abundSp<-sort(unique(abunds$Species))
preySp<-unique(c(fobsSp,abundSp))
predSp<-unique(fobs$Pred)

patches<-sort(unique(fobs$Patch))

# Match up and limit survey dates
# abund<-subset(abund,Date!=atimes[1])
fobs<-MatchDates(abunds,fobs) # see 'ExpPatch_MiscFunctions.r' for details

# Drop dates not present in both data sets
abunds<-abunds[which(abunds$Date %in% unique(fobs$Date)),]
fobs<-fobs[which(fobs$Date %in% unique(abunds$Date)),]

sDates<-as.Date(unique(c(abunds$Date,fobs$Date)))

#####################################################
# Match up temperature data with feeding observations
#####################################################
tempC$Date<-as.Date(tempC$Date)
tempC$avgTemp<-NA
# Average temperatures of preceeding D days
D=14
warning(paste0("Survey-specific temperatures averaged over the preceeding ",D," days."))
TempC<-dim(0)
for(s in 1:length(sDates[sDates%in%tempC$Date])){
	dloc<-which(tempC$Date%in%sDates[s]==TRUE)
	dtempC<-tempC[max(1,dloc-D):dloc,]
	tempC$avgTemp[dloc]<-round(mean(dtempC$Temp,na.rm=TRUE),2)
}

lastTemp<-round(mean(tail(tempC$Temp,D),na.rm=TRUE),2) # mean temperature of last D measured days
addDates<-sDates[sDates %in% tempC$Date==FALSE]
tempC<-tempC[tempC$Date %in% sDates ,c('Date','avgTemp')]
tempC<-rbind(tempC,data.frame(Date=addDates,avgTemp=rep(lastTemp,length(addDates))))
if(length(addDates>0)){
	warning("Some dates don't have measured temperatures and have been assigned the temperature of the last measured date.");	sort(addDates) }
fobs<-merge(fobs,tempC,all.x=TRUE)

#######################################################
# Match handling times regn coeff for NZ and OR species
#######################################################
htime_reg<-rbind(subset(htime_reg,Pred=='Haustrum haustorium' & Prey=='Haustrum scobina' & ConLevel==0.1 & Window==0.1 & Type=='Weighted'), subset(htime_reg,Pred=='Haustrum scobina' & ConLevel==0.1 & Window==0.1 & Type=='Weighted') )
htime_reg<-htime_reg[which(htime_reg$Prey %in% unique(htime_match$NZ)),]
htime_reg<-merge(htime_reg,htime_match,by.x='Prey',by.y='NZ')[,-c(1,2)]
fobs<-merge(fobs,htime_reg,by.x='Prey',by.y='OR',all.x=TRUE)
if(sum(preySp[-grep("Not_Feeding",preySp)] %in% unique(htime_reg$OR)==FALSE)!=0){warning('Not all OR prey species have been matched to NZ handling time regression coefficients.')}

############################################
# Estimate observation-specific handling times
fobs$htime<-exp(fobs$logIntC + fobs$logPredSizeC*log(fobs$PredSize) + fobs$logPreySizeC*log(fobs$PreySize)  + fobs$logTempC*log(fobs$avgTemp))

#####################
# Patch-Time averages
#####################
abunds$Species<-factor(abunds$Species,levels=preySp[-grep("Not_Feeding",preySp)])
abunds$Patch<-factor(abunds$Patch,levels=patches)
abund<-ddply(abunds,.(Date,Patch,Species),summarize,Density=mean(Density))
abund<-spread(abund,Species,Density,drop=FALSE,fill=0)
abund<-gather(abund,Prey,Density,-Date,-Patch)

fobs$Prey<-factor(fobs$Prey,levels=preySp)
fobs$Pred<-factor(fobs$Pred,levels=predSp)
fobs$Patch<-factor(fobs$Patch,levels=patches)

htime<-ddply(fobs,.(Date,Patch,Pred,Prey),summarize,htime=mean(htime))
htime<-complete(htime,Date,Patch,Pred,Prey)
htime<-subset(htime,Prey!='Not_Feeding')
# htime<-spread(htime,Prey,htime)
# htime<-gather(htime,Prey,htime,-Date,-Patch,-Pred)

# Feeding counts and ratios
fCnt<-ddply(fobs,.(Date,Patch,Pred),summarise,tObs=length(Prey))
fCnt<-complete(fCnt,Date,Patch,Pred,fill=list(tObs=0))

frat<-fcnt<-ddply(fobs,.(Date,Patch,Pred),function(x) table(x$Prey))
frat[,-c(1:3)]<-frat[,-c(1:3)]/frat$Not_Feeding
fcnt<-fcnt[-grep("Not_Feeding",colnames(fcnt))]
frat<-frat[-grep("Not_Feeding",colnames(frat))]
fcnt<-gather(fcnt,Prey,fcnt,-Date,-Patch,-Pred)
frat<-gather(frat,Prey,frat,-Date,-Patch,-Pred)
fcnt<-complete(fcnt,Date,Patch,Pred,Prey,fill=list(fcnt=0))
frat<-complete(frat,Date,Patch,Pred,Prey,fill=list(frat=0))

########################
# Combine into one table
########################
dat<-merge(frat,fcnt,all=TRUE)
dat<-merge(dat,fCnt,all=TRUE)
dat<-merge(dat,htime,all=TRUE)
dat<-merge(dat,abund,all=TRUE)
dat<-merge(dat,tempC,all.x=TRUE)

####################################
# Calculate attack and feeding rates
####################################
dat$att<-dat$frat/(dat$htime*dat$Density)
dat$frate<-dat$frat/dat$htime

# Metric is undefined when all indivudals are feeding
# but feeding rate may nonentheless be estimated as
# the inverse of the handling time.
infty<-which(is.infinite(dat$frate))
dat$frate[infty]<-1/dat$htime[infty]
dat$att[infty]<-1/(dat$htime[infty]*dat$Density[infty])

infty<-which(is.infinite(dat$att))
if(length(infty)>0){warning(paste(length(infty),'feeding observations made on prey with density estimate of zero.'))}

########
# Export
########
write.table(dat,'ExpPatch-Data/ExpPatch_IntxnStr.csv',sep=',',row.names=FALSE)

##############################################################################
##############################################################################
##############################################################################


