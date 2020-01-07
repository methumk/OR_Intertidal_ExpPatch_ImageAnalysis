##############################################################
# Miscallaneous functions that are useful in multiple contexts
##############################################################

# minDiff is Minimum assumed difference between survey sets
ConvertDate2SurveyDate<-function(dat,minDiff=9){
	dat$Date<-as.Date(dat$Date)
	SDates<-sort(unique(dat$Date))
	Dates<-data.frame(SurveyDate=SDates,Date=SDates)
	Dates$difDates<-c(0,diff(Dates$Date))
	for(i in 2:nrow(Dates)){ if(Dates$difDates[i]<minDiff){Dates$SurveyDate[i]<-Dates$SurveyDate[i-1]}	}
	dat<-merge(dat,Dates[,-3],by='Date',all.x=TRUE)
	# Switch names (in order to use 'Date' subsequently)
	colnames(dat)[which(colnames(dat)=='Date')]<-'TrueDate'
	colnames(dat)[which(colnames(dat)=='SurveyDate')]<-'Date'
	dat<-dat[,c(length(dat),1:(length(dat)-1))]
	return(dat)
}


# Match-up and rename Survey dates in two datasets
# (e.g., abundance dynamics and feeding observations)
# Spits out only the 2nd dataset (i.e. it conforms the dates in the 2nd dataset
# to match up with those of the 1st dataset.
MatchDates<-function(datA,datB,minDiff=9){
	datA$Date<-as.Date(datA$Date)
	datB$Date<-as.Date(datB$Date)
	SDatesA<-sort(unique(datA$Date))
	SDatesB<-sort(unique(datB$Date))
	for(i in 1:length(SDatesA)){
		tDate<-SDatesA[i]
		tDateSeq<-seq(tDate-minDiff,tDate+minDiff,1)
		Match<-which(datB$Date%in%tDateSeq)
		if(length(Match)!=0){datB$Date[Match]<-tDate}
		if(length(Match)==0){warning(paste0('No date match found for ',tDate))}
	}
	return(datB)
}

# Scale the points of a graph by their x-value
ptscale<-function(x,Scale=1,base=4){return(Scale*log(x,exp(base)))}


# Computes the variance of a weighted mean following Cochran 1977 
var.wtd.mean.cochran <- function(x,w,na.rm=TRUE){
	if(na.rm){	x<-x[!is.na(x)]; w<-w[!is.na(x)] }
	n = length(w)
	xWbar = weighted.mean(x,w)
	wbar = mean(w)
	out = n/((n-1)*sum(w)^2)*(sum((w*x-wbar*xWbar)^2)-2*xWbar* sum((w-wbar)*(w*x-wbar*xWbar))+xWbar^2*sum((w-wbar)^2))
	return(out)
}