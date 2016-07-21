#' Function for merging all the unique utilization distribution shapefiles created by \code{move.forud}
#'
#' \code{dbbmm_combine} function is a function that I worked up based on code from the R-Sig-Geo listserver (\url{https://stat.ethz.ch/pipermail/r-sig-geo/2010-January/007422.html})
#' that takes the utilization distribution contours created from \code{move.forud} that are output as individual shapefiles and imports them all into the workspace, ensures that the 
#' ID values are unique (using spChFIDs, which is the reason I worked this function up), and then merges all the resultant shapefiles together into one object and then
#' assigns them to the selected projection and exports that shapefile to a user defined location.
#' 
#' @details  This function, which I stole the base code for from R-Sig-Geo \url{https://stat.ethz.ch/pipermail/r-sig-geo/2010-January/007422.html} is
#' a simplifying function that collects all the shapefiles created by \code{move.forud} and/or \code{move.contour} function call and merges them all into a single
#' SpatialPolygonDataFrame, while carrying through all the important attributes created by \code{move.forud}, and allows for the user to write (using \code{writeOGR})
#' the resultant combined data into a shapefile for further use.  The the primary reason for this is that when I create the dbbmm polygons for each time step, I did not 
#' adjust (cause I am not smart enough yet) the FID values for each, and thusly \code{rbind} does not work to merge all the files together, even when nested within a 
#' \code{do.call()} function (although, surprisingly enough, rbind(out[[i]], out[[i+1]]) does work, but if you add out[i+2] it bombs.  There was a suggestiont that rbind(..., makeUniqueIDs = TRUE) would work, but it does not).  
#' So, until I figure out how to make one of the apply statements work, this crude hack will have to work. 
#' 
#' Again, I note this is just a function I worked up that I shamelessly stole from list authors at \url{https://stat.ethz.ch/pipermail/r-sig-geo/2010-January/007422.html}.  Note that I did not define any extra functions 
#' for \code{writeOGR} as I did not want to mess with them, but if someone needs another (such as driver= something other than ESRI Shapefile), let me know.  
#' 
#' @name dbbmm_combine
#' @param filepath,  file path (e.g., "C:/") statement to directory where all shapefiles from \code{move.forud} output are located
#' @param crs, coordinate reference system for identifying where the polygons are located.  Uses standard \code{CRS} structure within quotes: CRS("+proj=longlat +zone=14 +datum=NAD83")
#' @param outpath, file path using standard R path nomenclature specifying the location that the output shapefiles are to be written.  Note that because \code{writeOGR} is
#' being used, the outpath must include both the file path (e.g., "C:/" and the name/extension that the user want's the merged shapefile to be named (e.g., "C:/TestShapefile.shp")   
#' @param layer, layer name as required by \code{writeOGR}
#' @param write, writes the resultant merged shapefiles to the location identified by outpath and layer, default is TRUE, but if FALSE returns the merged shapefile as a R object.
#' @export
#' @references Kranstauber, D., R. Kays, S. D. Lapoint, M. Wikelski, and K. Safi.  2012.  A dynamic Brownian bridge movement model to estimate
#' utlization distributions for heterogenous animal movement.  Journal of Animal Ecology, DOI: 10.1111/j.1365-2656.2012.01955.x.
#' @author Bret A. Collier <bret@@lsu.edu>
#' @keywords moveud

dbbmm_combine=function(filepath, crs, outpath, layer, write=TRUE)
{
	setwd(filepath)
	if(write==TRUE){
		files <- list.files(filepath, pattern = "shp")
		uid <- 1
		# get polygons from first file
		out <- readOGR(files[1], gsub(".shp","",files[1]))
		n <- length(slot(out, "polygons"))
		out <- spChFIDs(out, as.character(uid:(uid+n-1)))
		uid <- uid + n
		# add polygons from remaining files
		for (i in 2:length(files)) {
			x <- readOGR(files[i], gsub(".shp","",files[i]))
			n <- length(slot(x, "polygons"))
			xx <- spChFIDs(x, as.character(uid:(uid+n-1)))
			uid <- uid + n
			out <- spRbind(out,xx)
		}
		xx <- spTransform(out, CRSobj=CRS(crs))
		writeOGR(xx, dsn=outpath, layer=layer, driver="ESRI Shapefile")
	}
	if(write==FALSE){
		files <- list.files(filepath, pattern = "shp")
		uid <- 1
		# get polygons from first file
		out <- readOGR(files[1], gsub(".shp","",files[1]))
		n <- length(slot(out, "polygons"))
		out <- spChFIDs(out, as.character(uid:(uid+n-1)))
		uid <- uid + n
		# add polygons from remaining files
		for (i in 2:length(files)) {
			x <- readOGR(files[i], gsub(".shp","",files[i]))
			n <- length(slot(x, "polygons"))
			xx <- spChFIDs(x, as.character(uid:(uid+n-1)))
			uid <- uid + n
			out <- spRbind(out,xx)
		}
		res <- spTransform(out, CRSobj=CRS(crs))
		return(res)
	}
}

