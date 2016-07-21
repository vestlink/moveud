#' Utilization Distribution Contours for dbbmm (Dynamic Brownian Bridge Movement Model)
#'
#' \code{move.contour} function that creates the utilization distribution contours based on the \code{bbmm.contour}
#' from package BBMM and exports the created contour lines as a \code{SpatialLinesDataFrame} or a \code{SpatialPolygonsDataFrame} to a set of shapefiles
#' for further analysis in ArcMap (or GIS program of choice).
#' 
#' @details  This function works pretty well, but to date it is currently pretty limited in functionality as I have several parts of the 
#' \code{brownian.bridge.dyn()} call hard coded in it. As a note, if the time steps are closer together and the extent of the individuals movements is 
#' fairly large relative to the movement steps, that the process slows down considerably and you have to shorten \code{range.subset} value to
#' a smaller number of segments otherwise memory limits will hit pretty rapidly (this may require some trial and error as I have run >1000 segment steps
#' in some cases and have been able only get to 20-30 in other runs.  I should deprecate this function, but some folks are using it for short daily studies
#' so I am leaving it in here until then.  
#' 
#' As a solution to this issue, try the other function \code{move.forud} which is a function that does the same thing that \code{move.contour()} does, 
#' but as a \code{for} loop that outputs one shapefile at a time, versus doing them in batch. The time it takes to do each method seems to be the same as 
#' the slowness of the process is due to the calculations being done by 
#' \code{brownian.bridge.dyn()}.
#' 
#' There is probably some need to restructure how the raster grid estimation is done as right now it estimates values for each cell, so many (1000s) have really small (1.2E-200)
#' values that are effectively zero.  Probably would benefit from reducing the 'extent' size in the \code{move} object, but I have not gotten around to it yet.
#' 
#' @name move.contour
#' @param x, dbbmm object.  Currently, I have not included the ability to manipulate the internal dbbmm object within the \code{move.contour} function
#' and thus the values for raster=5 and time.step=0.2 are set).
#' @param range.subset, range of model step segments that the user wants to create the UD contours for.  Has to be > the margin based on the dbbmm object as estimates of variance for
#' the first few steps are not estimable depending on how big of a margin is specified.  Easiest way to define this range is to use the row number from the input file.
#' @param le, location error value for \code{location.error=} used in a typical dbbmm object
#' @param lev, level of the UD contour as a vector, c(50, 95) that the user is interested in.   Will work with multiple values (e.g., c(50, 95)) and will label those values
#' in the resultant shapefile
#' @param ras, raster background size for \code{brownian.bridge.dyn()} object
#' @param ts, time step for integration of \code{brownian.bridge.dyn()} object
#' @param path, file path (e.g., "C:/") using standard R path nomenclature specifying the location that the output shapefiles are to be written.  
#' @param name, file name, in quotation's, specifying the output files name which will be written to the \code{path} designated above.  This does not 
#' require a file extension (e.g., 'shp'). 
#' @param lines, allows user to select between outputting a shapefile of contour lines or polygons for use in other programs (e.g., ArcMap). Default is FALSE (will export polygons) mainly 
#' because the lines are just lines and don't have any structure associated with them thus ArcMap cannot use zonal statistics on other layers with the lines defining the area.  I used R
#' package \code{PBSmappling} to do lines to polygons within the function, its not overly efficient but I could not come up with a better way.
#' @param crs, coordinate reference system for identifying where the polygons are located.  Uses standard \code{CRS} structure within quotes (e.g., "+proj=longlat +zone=14 +datum=NAD83").
#' @return Output is a set of shapefiles, written to the specified \code{path} 
#' @export
#' @references Kranstauber, D., R. Kays, S. D. Lapoint, M. Wikelski, and K. Safi.  2012.  A dynamic Brownian bridge movement model to estimate
#' utlization distributions for heterogenous animal movement.  Journal of Animal Ecology 81: 738-746. DOI: 10.1111/j.1365-2656.2012.01955.x.
#' @references Byrne, M. E., J. C. McCoy, J. Hinton, M. J. Chamberlain, and B. A. Collier. 2014. Using dynamic brownian bridge movement modeling to measure temporal 
#' patterns of habitat selection. Journal of Animal Ecology, In Press. DOI: 10.1111/1365-2656.12205
#' @author Bret A. Collier <bret@@lsu.edu>
#' @keywords moveud

move.contour=function(x, range.subset, le, lev, ras, ts, path, name, lines=FALSE, crs)
{
	object<-x@DBMvar
	if(length(range.subset)<2)
	stop("\nrange.subset must have length >1\n")
		if(length(le)==1) location.error=rep(c(le), nrow(object))
		if(length(le)>1)  location.error=c(le)

	x.list=list()
	dtime=list()
	vars=list()
	
#maybe add if statement--if(range.subset=value), do the loop, else do the object value of interest?
	for(i in range.subset){
		object@interest<-rep(F, nrow(object)); object@interest[i]<-T;times=object@timestamps[i];var=object@means[i];
		x.out <- brownian.bridge.dyn(object, raster=ras,time.step=ts, location.error=location.error)
		x.list[[i]] <- x.out
		dtime[[i]] <-times
		vars[[i]] <- var
	}
	
#Remove the front end of list with value=NULL as dbbmm will not create estimates for first 4 and if you want a 
#higher range of values you need to get rid of all the empty NULL values in x.list.
	x.list=x.list[-c(1:min(range.subset)-1)]
	dtime=dtime[-c(1:min(range.subset)-1)]
	vars=vars[-c(1:min(range.subset)-1)]
	
#create object for bbmm.contour
	contrs=list()
	for(i in 1:length(x.list)){
		r <- list(x=seq(x@extent@xmin,x@extent@xmax, by=1), y=seq(x@extent@ymin,x@extent@ymax, by=1), probability=x.list[[i]]@data@values)
		contrs[[i]]=r
	}
	
#creates the contour values using bbmm.contour() for each outputted segment
contrs.values=lapply(contrs[1:length(x.list)], function(x){bbmm.contour(x, levels=c(lev), plot=FALSE)})

if(lines=="TRUE"){	
#creates the contour rasters for each contour line problem was with the indexing of level= part of function
	out=sapply(1:length(x.list), function(x){rasterToContour(x.list[[x]], maxpixels=4000000, level=sapply(contrs.values[x], "[[", 2))})
	}
#Creates polygons instead of lines

if(lines=="FALSE"){
	out=sapply(1:length(x.list), function(x){rasterToContour(x.list[[x]], maxpixels=4000000, level=sapply(contrs.values[x], "[[", 2))})
	out=lapply(out[1:length(x.list)], function(x){spTransform(x, CRS(crs))})
	out=lapply(out[1:length(x.list)], function(x){SpatialLines2PolySet(x)})
	out=lapply(out[1:length(x.list)], function(x){PolySet2SpatialPolygons(x)})
	out=lapply(out[1:length(x.list)], function(x){as(x, "SpatialPolygonsDataFrame")})
	}

#bind the timestamps from dbbmm object back into object out that gets exported to shapefile
	for(i in 1:length(x.list)){out[[i]]$time = dtime[[i]]}
	for(i in 1:length(x.list)){out[[i]]$stepvar = vars[[i]]}
	for(i in 1:length(x.list)){out[[i]]$udvals = paste(rev(lev))}
	
#working on writing multiple shapefiles
	for(i in 1:length(x.list)){
		writeOGR(out[[i]], dsn=path, layer=paste(name, i, sep=""), driver="ESRI Shapefile")
		#shapefile(paste(path, name, i, ".shp", sep=""), out[[i]])
	}
}


