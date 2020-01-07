# The corner of each patch, the temperature tidbits, and ocean water level (measured at two time points) were measured relative to the laser.  This script converts these to absolute tide height above MLLW, using the verified MLLW heights from South Beach OR (http://tidesandcurrents.noaa.gov/waterlevels.html?id=9435380) and calculates the average tidal height of each patch.  Heights are in centimeters.
# Measurements were taken on 6/2/15
###################################################################
rm(list=ls()) # clears workspace
options(stringsAsFactors=F)

setwd('/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches/')

# Water level
wl<-read.csv('ExpPatch-Data/ExpPatch_PatchTidalHeights_raw.csv',skip=2,nrow=2)
colnames(wl)<-c('Ref','Height','MLLW')
wl<-wl[,1:3]

# Patches
dat<-read.csv('ExpPatch-Data/ExpPatch_PatchTidalHeights_raw.csv',skip=6,nrow=18)

# Tidbits
tb<-read.csv('ExpPatch-Data/ExpPatch_PatchTidalHeights_raw.csv',skip=26,nrow=4)
tb<-tb[,1:2]

################
# Mean height of laser to MLLW
lh<-mean(wl$Height+wl$MLLW)

# Avg patch relative to MLLW
dat$HeightMLLW<-round(lh-apply(dat[,-1],1,mean),0)
patch<-dat[,c(1,6)]

# Tidbit relative to MLLW
tb$HeightMLLW<-round(lh-tb$Height,0)
tidbit<-tb[,c(1,3)]
###############
# Export
write.csv(patch,'ExpPatch-Data/ExpPatch_PatchTidalHeights.csv',row.names=FALSE)
write.csv(tidbit,'ExpPatch-Data/ExpPatch_TidbitTidalHeights.csv',row.names=FALSE)

###################################################################
