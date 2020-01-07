##########################################################
##########################################################
# Script to pars xls files containing species counted from
# monthly photographs analysed using ImageJ
##########################################################
##########################################################
rm(list=ls()) # clears workspace
options(stringsAsFactors=F)

#############
setwd('/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches/ExpPatch-Pics/ExpPatchPics-Processed/')
# setwd('~/Desktop')
#############
require(gdata) # for reading xls files
require(plyr)
require(lubridate)
require(RCurl) # to read in Google spreadsheet
require(reshape2)
source('/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches/ExpPatch-Scripts/ExpPatch_MiscFunctions.r') # For "ConvertDate2SurveyDate" to bin dates into "survey set" dates.

#############
# Import data
#############
#  Species Types
Spp<-read.csv('../../ExpPatch-Info/ExpPatch_SpeciesTypesIDs.txt',sep=',',skip=1)
Patches<-c(LETTERS[1:7],paste0('A',LETTERS[2:7]),paste0('B',LETTERS[2:6]))

Barns<-c('Type 1', 'Type 2','Type 3','Type 4')
Muss<-c('Type 7','Type 8','Type 15','Type 16','Type 17','Type 18')

# Google spreadsheet
inf<-read.csv(text=getURL("https://docs.google.com/spreadsheets/d/1BWpBvagR6X-Qo8gHF7YrqhImVRb-bwtQ1LQPNgXA4Js/export?gid=0&format=csv"), header=T, skip=0,sep=",", na.strings = c("*", "NA"))
colnames(inf)<-c('DPQ','Counter','InProg','CropM','CropB','Algae','Notes')
# Remove 'extra' columns
inf<-inf[,1:7]

# list of 'xls' files from server that were produced by ImageJ
files<-list.files(path='.',full.names=TRUE,recursive=TRUE,include.dirs=TRUE, pattern=".xls")

# remove in progress files
rem<-grep("IP",files)
if(length(rem)>0){files<-files[-rem]}

# Initiate error file
ErrFile<-paste0('../../ExpPatch-Output/ParsingErrors/ExpPatch_PhotoParseWarnings-',Sys.Date(),'.txt')
	sink(file=ErrFile)
	print("The following are errors associated with the parsing of the photo quadrat counts.")
	sink()

#################
# Parse xls files
#################
NumFiles<-length(files)
DataErr<-dim(0)
tdat<-dim(0)
for (f in 1:length(files)){
	temp<-strsplit(readLines(files[f],warn=FALSE),split='\t')
	if(length(temp)!=2){DataErr<-rbind(DataErr,files[f])}
	if(length(temp)==2){

	# remove trailing zeroes
	rem<-which(temp[[1]]=='')
	if(length(rem)>0){temp[[1]]<-temp[[1]][-rem];temp[[2]]<-temp[[2]][-rem]}

	sp<-length(temp[[1]])-1
	temp<-data.frame(matrix(cbind(temp[[1]][2:(sp+1)],temp[[2]][2:(sp+1)]),ncol=2))
	colnames(temp)<-c('Type','Count')

	# parse out the date-site-patch-quad info (could probably be done more efficiently)
	dsq<-sub("*.xls", "", basename(files[f]))
	DPQ<-sub("_c","",dsq)
	dsq2<-strsplit(dsq,"_")[[1]]
	date<-dsq2[1]
	sq<-strsplit(dsq2,'-')[[2]]
	site<-sq[1]
	patch<-strsplit(sq[2],'q')[[1]][1]
	quad<-strsplit(sq[2],'q')[[1]][2]

	# The following is a bit clunky, but ensures that zeroes get added for the correct list of unobserved species
	eDPQ<-c(DPQ=DPQ,Date=date,Site=site, Patch=patch, Quad=quad,Cropped=dsq2[3],Type=NA)
	fDPQ<-as.data.frame(matrix(rep(eDPQ,max(sp,nrow(Spp))), nrow=max(sp,nrow(Spp)),byrow=TRUE))
	colnames(fDPQ)<-names(eDPQ)
	ifelse(any(Spp$Type%in%temp$Type==FALSE),fDPQ$Type<-Spp$Type,fDPQ$Type<-temp$Type)
	temp<-merge(fDPQ,temp,by='Type',all.x=TRUE)
	temp$Count[is.na(temp$Count)]<-0

	# remove zero-count non-Barn or Muss species from cropped pics
	if(!is.na(dsq2[3])){
		rem<-which(temp$Type%in%c(Barns,Muss)==FALSE & temp$Count==0)
		if(length(rem)>0){temp<-temp[-rem,]}	}

	tdat<-rbind(tdat,temp)
}}
dat<-tdat

# Count successful imports
NumImp<-nrow(unique(dat[,c('DPQ','Cropped')]))
if(NumFiles!=NumImp){
	warn<-'Not all xls files were successfully imported.  Turn off "warn=FALSE" in readLines function.\n'
	warning(warn, immediate.=TRUE)
	sink(file=ErrFile,append=TRUE)
		print(warn)
		print("");print("");   sink()  }

# Report unsuccessful imports with data problems
if(!is.null(DataErr)){
	if(nrow(DataErr>0)){
	warn<-'The following files have problems with their data.\n'
	warning(warn, immediate.=TRUE)
	sink(file=ErrFile,append=TRUE)
		print(warn)
		print(DataErr)
		print("");print("");   sink()  }}

dat$Count<-as.numeric(dat$Count)
dat$Date<-as.Date(dat$Date)

##############################
# Parse count area information
##############################
inf<-subset(inf,InProg=='Finished')
dpq<-strsplit(inf$DPQ,"_")
inf$Date<-as.Date(unlist(lapply(dpq,function(x){x[1]})))
pq<-unlist(lapply(strsplit(unlist(lapply(dpq,function(x){x[2]})),'-'),function(x){x[2]}))
pq2<-strsplit(pq,'q')
inf$Patch<-unlist(lapply(pq2,function(x){x[1]}))
inf$Quad<-unlist(lapply(pq2,function(x){x[2]}))
inf$Quad<-as.numeric(substr(inf$Quad,1,1)) # remove 'b' from quad number

###################################################
# Identify dubplicate entries in Google spreadsheet
###################################################
dups1<-which(duplicated(inf$DPQ))
dups2<-which(duplicated(inf$DPQ,fromLast=TRUE))
dups<-sort(c(dups1,dups2))
if(length(dups1)>0){
	warn<-'The following are duplicated in the Google Spreadsheet. The second of each pair has been removed!:\n'
	warning(warn, immediate.=TRUE)
		print(inf[dups,])
	sink(file=ErrFile,append=TRUE)
		print(warn)
		print(inf[dups,]); print("");print("");   sink()
	inf<-inf[-dups2,]
	}

#################################################################
# Count matches and mismatches between xls and google spreadsheet
#################################################################
Udat<-unique(dat[,c('DPQ','Date','Patch','Quad')])
mDat<-which(Udat$DPQ%in%inf$DPQ==FALSE)
mInf<-which(inf$DPQ%in%Udat$DPQ==FALSE)

if(length(mDat)>0){
	warn<-'The following are missing from the Google Spreadsheet:\n'
	warning(warn, immediate.=TRUE)
		print(Udat[mDat,])
	sink(file=ErrFile,append=TRUE)
		print(warn)
		print(Udat[mDat,]); print("");print("");   sink()  }
if(length(mInf)>0){
	warn<-'The following are missing from the xls files:\n'
	warning(warn, immediate.=TRUE)
		print(inf[mInf,c('DPQ','Counter','CropM','CropB')])
	sink(file=ErrFile,append=TRUE)
		print(warn)
		print(inf[mInf,c('DPQ','Counter','CropM','CropB')]); print('');  sink()	}

#################################
# Merge in count area information
#################################
n1<-nrow(dat)
# Note, this will lose many files if they're not in both information sources!
dat<-merge(dat,inf,by=c('DPQ','Date','Patch','Quad'), all=FALSE)
n2<-nrow(dat)
if(n1!=n2){
	warn<-paste0(n1-n2,' species records in the xls files have been lost because they could not be found on the Google spreadsheet, or were present in the spreadsheet but had no corresponding xls file.')
	warning(warn,immediate.=TRUE)
	sink(file=ErrFile,append=TRUE)
		print(warn);	print("");print("");   sink()
}

########################
# Merge in species names
########################
# Remove unknown species types having zero counts
rem<-which(dat$Type%in%Spp$Type==FALSE & dat$Count==0)
if(length(rem)>0){dat<-dat[-rem,]}
Mspp<-dat[which(dat$Type%in%Spp$Type==FALSE),c('DPQ','Counter','Type','Count')]
Mspp<-Mspp[ do.call(order, Mspp), ]
if(nrow(Mspp)>0){
	warn<-"These xls files contain unknown species types that have been removed from the data."
	warning(warn, immediate.=TRUE)
	print(Mspp)
	sink(file=ErrFile,append=TRUE)
		print(warn);	print(Mspp);	print("");print("");   sink()
}
dat<-merge(dat,Spp[,1:3],by='Type',all.x=TRUE) # all.x=TRUE removes species not in the Type list
# # Flag unknown species types having non-zero counts
# dat$Species[is.na(dat$Species)]<-'Unknown'

########################################
# Fix Cropped vs. Full duplicate entries
########################################
# Remove Balanus and Cthalamus full count rows if counted in cropped pics
rem<-which(dat$Type%in%Barns & dat$CropB!='Full' & is.na(dat$Cropped))
remC<-rem[which(dat$Count[rem]>0)]
if(length(remC)>0){
	warn<-'The following lines had non-zero acorn barnacle counts but were inconsistent in having been obtained from full quadrat pictures even though acorn barnacles were specified as having been counted in the cropped or half-cropped pictures.  They have been removed from the data.  Zero counts have also been removed.\n'
	warning(warn, immediate.=TRUE)
	out<-dat[remC,c('DPQ','Counter','Species','Count','Cropped','CropB')]
	print(out)
	sink(file=ErrFile,append=TRUE)
	print(warn);	print(out);	print("");print("");   sink()	}
if(length(rem)!=0){ dat<-dat[-rem,]	}

# Remove Balanus and Cthalamus cropped count rows if counted in full pics
rem<-which(dat$Type%in%Barns & dat$CropB=='Full' & !is.na(dat$Cropped))
remC<-rem[which(dat$Count[rem]>0)]
remZ<-rem[which(dat$Count[rem]==0)]
if(length(remC)>0){
	warn<-'The following lines had non-zero acorn barnacle counts but were inconsistent in having been obtained a cropped quadrat pictures even though acorn barnacles were specified as having been counted in the full pictures.  They have NOT YET been removed from the data.  (Currently only lines with zero counts have been removed.)\n'
	warning(warn, immediate.=TRUE)
	out<-dat[remC,c('DPQ','Counter','Species','Count','Cropped','CropB')]
	print(out)
	sink(file=ErrFile,append=TRUE)
	print(warn);	print(out);	print("");print("");   sink()	}
if(length(remZ)!=0){ dat<-dat[-remZ,]	}

# Remove Mytilus count rows if counted in cropped pics
rem<-which(dat$Type%in%Muss & dat$CropM!='Full' & is.na(dat$Cropped))
remC<-rem[which(dat$Count[rem]>0)]
if(length(remC)>0){
	warn<-'The following lines had non-zero mussel counts but were inconsistent in having been obtained from full quadrat pictures even though mussels were specified as having been counted in the cropped or half-cropped pictures.  They have been removed from the data. Zero counts have also been removed.\n'
	warning(warn, immediate.=TRUE)
	out<-dat[remC,c('DPQ','Counter','Species','Count','Cropped','CropB')]
	print(out)
	sink(file=ErrFile,append=TRUE)
	print(warn);	print(out);	print("");print("");   sink()	}
if(length(rem)!=0){ dat<-dat[-rem,]	}

# Remove Balanus and Cthalamus cropped count rows if counted in full pics
rem<-which(dat$Type%in%Muss & dat$CropM=='Full' & !is.na(dat$Cropped))
remC<-rem[which(dat$Count[rem]>0)]
remZ<-rem[which(dat$Count[rem]==0)]
if(length(remC)>0){
	warn<-'The following lines had non-zero mussel counts but were inconsistent in having been obtained a cropped quadrat pictures even though mussels were specified as having been counted in the full pictures.  They have NOT YET been removed from the data.  (Currently only lines with zero counts have been removed.)\n'
	warning(warn, immediate.=TRUE)
	out<-dat[remC,c('DPQ','Counter','Species','Count','Cropped','CropB')]
	print(out)
	sink(file=ErrFile,append=TRUE)
	print(warn);	print(out);	print("");print("");   sink()		}
if(length(remZ)!=0){ dat<-dat[-remZ,]	}

# Remove species other than Balanus, Cthalamus and Mytilus mussels) that have values for cropped pictures. (These species should all have been counted in the full pictures.)
rem<-which(dat$Type%in%c(Barns,Muss)==FALSE & !is.na(dat$Cropped))
remC<-rem[which(dat$Count[rem]>0)]
if(length(remC)>0){
	warn<-'The following lines had non-zero counts made in the cropped picture for species that should have been counted in full pictures.  They have been removed from the data. Zero counts have also been removed.\n'
	warning(warn, immediate.=TRUE)
	out<-dat[remC,c('DPQ','Counter','Species','Count','Cropped','CropB')]
	print(out)
	sink(file=ErrFile,append=TRUE)
	print(warn);	print(out);	print("");print("");   sink()		}
if(length(rem)!=0){ dat<-dat[-rem,]	}

#############################
# Convert Counts to Densities
#############################
QuadArea<-0.25*0.35	#25x35cm
CropArea<-0.125*0.125 # 12.5x12.5cm (Updated 8/26/15: Earlier code had incorrect 10x10cm area.  Note that pics from the two camera's have different full-image pixel dimensions.  Thus their cropped images are cropped to differing pixel dimensions to achieve 12.5 x 12.5 cm cropped images.)
HalfCropArea<-CropArea/2

dat$Area<-NA

# Species counted in Full pictures
dat$Area[is.na(dat$Cropped)]<-QuadArea
# Mytilus mussels counted in Cropped pictures
dat$Area[which(dat$Type%in%Muss & dat$CropM=='Cropped' & !is.na(dat$Cropped))]<-CropArea
# Balanus and Cthalamus counted in Cropped pictures
dat$Area[which(dat$Type%in%Barns & dat$CropB=='Cropped' & !is.na(dat$Cropped))]<-CropArea
# Mytilus mussels counted in Half-Cropped pictures
dat$Area[which(dat$Type%in%Muss & dat$CropM=='HalfCropped' & !is.na(dat$Cropped))]<-HalfCropArea
# Balanus and Cthalamus counted in Half-Cropped pictures
dat$Area[which(dat$Type%in%Barns & dat$CropB=='HalfCropped' & !is.na(dat$Cropped))]<-HalfCropArea

prob<-which(is.na(dat$Area))
if(length(prob)>0){
	warn<-'The following lines could not be assigned sampling areas (i.e. full vs. cropped vs. half-cropped).  Thus their densities will remain as NAs.\n'
	warning(warn,immediate.=TRUE)
	out<-dat[prob,c('DPQ','Counter','Species','Count','Cropped','CropM','CropB')]
	print(out)
	sink(file=ErrFile,append=TRUE)
	print(warn);	print(out);	print("");print("");   sink()		}

dat$Density<-dat$Count/dat$Area

###################
# Summarize records
###################
 # These should all be divisible by nrow(Spp).  If not, then some species are counted twice (probably barnacles or mussels in both full and cropped pictures)
DP<-table(dat$Date,dat$Patch)

DQP<-table(dat$Date,dat$Quad,dat$Patch)
DQPerr<-which(DQP>0&DQP!=nrow(Spp),arr.ind=TRUE)
if(nrow(DQPerr)>0){
	warn<-paste0('The following quadrats contain more or less species counts than the ',nrow(Spp),' that are expected.\n')
	warning(warn,immediate.=TRUE)
	out<-data.frame(Date=dimnames(DQP)[[1]][DQPerr[,1]], Patch=dimnames(DQP)[[3]][DQPerr[,3]], Quad=dimnames(DQP)[[2]][DQPerr[,2]],Count=DQP[DQPerr])
	out<-merge(unique(dat[,c('Date','Patch','Quad','Counter')]),out, by=c('Date','Patch','Quad'),all.y=TRUE)
	print(out)
	sink(file=ErrFile,append=TRUE)
	print(warn);	print(out);	print("");print("");   sink()		}

#####################
# Close warnings file
#####################
if(sink.number()>0){sink()}

#####################################
# Aggregate "True dates" into "Dates"
#####################################
dat$Date<-parse_date_time(dat$Date,'%y-%m-%d')
dat<-ConvertDate2SurveyDate(dat)

##########################
# Reorder columns and rows
##########################
DesiredCols<-c('DPQ','Date','TrueDate','Site','Patch','Quad','Abb','Species','Type', 'Count','Area','Density','Cropped','Counter','InProg','CropM','CropB','Algae','Notes')
dat<-dat[,DesiredCols]
# Order rows
dat<-dat[order(dat$Date,dat$Patch,dat$Species,dat$Quad),]

###############################
# Count quads/patches completed
###############################
# Number of quadrats done within each patch per survey period
PQD<-table(dat[,c('Date','Patch')])/nrow(Spp)

# Number of quadrats done
DQD<-table(dat[,c('Date','Quad','Patch')])/nrow(Spp)

######################################################################################
# Combine counts of unidentified black mussels "Mytilus spp." and "Mytilus trossolus"
######################################################################################
muss<-subset(dat,Species=='Mytilus_trossulus'|Species=='Mytilus_spp')
muss<-ddply(muss,.(DPQ,Date,TrueDate,Site,Patch,Quad),summarize,Abb='Mt2', Species='Mytilus_trossulus2',Type='NA',Count=sum(Count),Area=0.0875, Density=sum(Density),Cropped=NA,Counter=NA,InProg=NA,CropM=NA,CropB=NA, Algae=NA,Notes=NA)
dat<-rbind(dat,muss)


dmuss<-subset(dat,Species=='Mytilus_trossulus_(dead)'|Species=='Mytilus_spp_(dead)')
dmuss<-ddply(dmuss,.(DPQ,Date,TrueDate,Site,Patch,Quad),summarize,Abb='MtD2', Species='Mytilus_trossulus2_(dead)', Type='NA',Count=sum(Count),Area=0.0875, Density=sum(Density),Cropped=NA,Counter=NA,InProg=NA,CropM=NA,CropB=NA, Algae=NA,Notes=NA)
dat<-rbind(dat,dmuss)

#######################
# Export back to Server
#######################
write.table(dat, '../../ExpPatch-Data/ExpPatch_PhotoCounts.csv',sep=',',row.names=FALSE)
write.table(dat, paste0('../../ExpPatch-Data/Backups/ExpPatch_PhotoCounts-',Sys.Date(),'.csv'), sep=',',row.names=FALSE)
write.table(inf, paste0('../../ExpPatch-Data/Backups/ExpPatch_PhotoCounts_NotesExport-',Sys.Date(),'.csv'), sep=',',row.names=FALSE)

#######################
system("say -v Vicki Just finished parsing photos!") # only works on Mac
########################################################################################
########################################################################################
