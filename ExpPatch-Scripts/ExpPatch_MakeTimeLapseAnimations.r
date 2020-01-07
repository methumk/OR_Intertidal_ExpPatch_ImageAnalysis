rm(list=ls()) # clears workspace
options(stringsAsFactors=F)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to make movies out of ExpPatch pics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Requirements:
# 	Must have ImageMagick installed on machine.  (May require intalling MacPorts, too.)
# Arguments:
# 	filedir - the parent directory "ExperimentalPatches"
# 	Patches - the patches for which movies are desired.  Enter vector of "A" to "BF", or leave unspecified to do all patches (default).
# 	Quads - specify which quads (1-9), 'All', or leave blank to do only overview patch pictures and not the individual quads (default)
#	 Cropped - TRUE/FALSE to select the (TRUE) full quadrat pictures or (FALSE, default) the cropped pics
# 	MovieSizeFrac - % of original image dimensions with which to create movie
#	DelayTime - how long to show each image (default = 180)

MakeMovies<-function(
	filedir= '/Volumes/NovakLab/Projects/OR_Intertidal/ExperimentalPatches',
	Patches=c(LETTERS[1:7],paste0('A',LETTERS[2:7]),paste0('B',LETTERS[2:6])),
	Quads=NULL,
	Cropped=FALSE,
	MovieSizeFrac=10,
	DelayTime=180){

	Patches<-paste0('-',Patches) # Hyphen is needed to distinguish 'B' from 'AB' and 'BB'
	if(is.null(Quads)){PatchQuads<-Patches}
	if(!is.null(Quads)){
		if(Quads!='All'){
		PatchQuads<-as.vector(outer(Patches, paste0('q',Quads), paste, sep=""))	}
		if(Quads=='All'){
		PatchQuads<-as.vector(outer(Patches, paste0('q',1:9), paste, sep=""))	}
		if(Cropped){PatchQuads<-paste0(PatchQuads,'_c')}
	}

	Tempdir<-"~/Desktop/ExpPatch-TempTimeLapsPics"
	dir.create(Tempdir,showWarnings=FALSE)
	if(is.null(Quads)){Moviedir<-paste0(filedir,'/ExpPatch-TimeLapseGIFs/Patches/')}
	if(!is.null(Quads)){Moviedir<-paste0(filedir,'/ExpPatch-TimeLapseGIFs/Quads/')}
	dir.create(Moviedir,showWarnings=FALSE)


	# Get locations of all jpeg files
	print('Searching for all jpg files in "Processed" and "NeedProcessing" folders')
	setwd(filedir)
	Pfiles<-list.files(path='./ExpPatch-Pics/ExpPatchPics-Processed', pattern=".jpg",ignore.case=TRUE,full.names=TRUE,recursive=TRUE,include.dirs=TRUE)
	NPfiles<-list.files(path='./ExpPatch-Pics/ExpPatchPics-NeedProcessing', pattern=".jpg",ignore.case=TRUE,full.names=TRUE,recursive=TRUE,include.dirs=TRUE)
	AllFiles<-union(Pfiles,NPfiles)

	print('Creating movies for specified Patches/Quadrats')
	for(p in 1:length(PatchQuads)){
		GetFiles<-c(grep('0-Black.jpg',AllFiles,ignore.case=TRUE), grep('9-White.jpg',AllFiles,ignore.case=TRUE), grep(paste0(PatchQuads[p],'.jpg'),AllFiles,ignore.case=TRUE))
		files<-AllFiles[GetFiles]
		print(files)
		if(length(files)>1){
			# Make temporary pic copies in movie folder
			for (f in 1:length(files)){	file.copy(files[f],Tempdir)}
			setwd(Tempdir)
			# convert the .jpg files to one .gif file using ImageMagick.
			# The system() function executes the command as if it was done
			# in the terminal. the -delay flag sets the time between showing
			# the frames, i.e. the speed of the animation. The -resize function
			# resizes each image to the provided % of the original images.
			system(paste0("convert -delay ",DelayTime," *.jpg -resize ",MovieSizeFrac,"% ExpPatch_YB", PatchQuads[p],".gif"))

	  		file.remove(list.files(pattern=".jpg",ignore.case=TRUE)) # delete temporary jpg files
	  		mov<-list.files(Tempdir,pattern='.gif')
	  		file.copy(mov,Moviedir,overwrite=TRUE) # move movie to moviedir
	  		file.remove(mov)
			setwd(filedir)
			print("File(s) moved")
	}}
	unlink(Tempdir,recursive=TRUE) # delete temporary folder

	system("say -v Vicki Just finished making animations!") # only works on Mac

}


# Test
# MakeMovies(Patches='A')

# MakeMovies(Patches='A',Quads=1)

# MakeMovies()
# MakeMovies(Quads='All')
