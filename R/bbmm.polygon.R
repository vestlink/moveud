#' Export Routine for BBMM polygons 
#'
#' \code{bbmm.polygon} function that creates the utilization distribution contours based on the \code{bbmm.contour}
#' from package BBMM and exports the created contour lines as polygon shapefile for further analysis in ArcMap (or GIS program of choice).  
#' 
#' @details  This is a simple extractor function for a \code{brownian.bridge()} that exports the \code{bbmm.contour()} results from \code{R} package BBMM into a polygon shapefile.  
#' Since BBMM requires UTM, the functions transforms the data from the current format (defined by \code{CRS()} to UTM internally). The primary requisite is that the column names for the 
#' input data frame's longitude and latitude data are labeled "Lon", and "Lat", and the format needs to be appropriate for a BBMM analysis, it will error otherwise.  
#' 
#' @name bbmm.polygon
#' @param x, a data frame formatted appropriately for use in a \code{brownian.bridge} object with the requirement that spatial data columns are labeled as "Lat" and "Lon"
#' @param crs.current, coordinate reference system identifying the current state of the data (e.g., lat/long).  Uses standard \code{CRS} structure within quotes: CRS("+proj=longlat +zone=14 +datum=NAD83")
#' @param crs.utm, coordinate reference system that transforms the curent state to UTM.  Uses standard \code{CRS} structure within quotes: CRS("+proj=utm +zone=14 +datum=NAD83")
#' @param lev, level of the UD contour as a vector, c(50, 95) that the user is interested in.   Will work with multiple values (e.g., c(50, 95)) and will label those values
#' in the resultant shapefile.  Note that you don't have to put these values in as percentages (e.g., 0.50 for 50 percent).
#' @param plot, do you want \code{R} to plot the image on screen, default is FALSE. 
#' @param path, file path (e.g., "C:/") using standard R path nomenclature specifying the location that the output shapefile is to be written.  path uses \code{setwd()} currently.  
#' @param indID, Unique identifier for each individual, in quotation's, specifying the output files name which will be written to the \code{path} designated above.  Will additionally add 
#' a column to the attribute table with the individuals unique ID.
#' @return Output is a polygon shapefile, written to the specified \code{path}, with \code{n} polygon features equivalent to the utilization distribution levels requested by the user  
#' @export
#' @references Horne, J. S., E. O. Garton, S. M. Krone, and J. S. Lewis.  2007.  Analyzing animal movements using Brownian bridges.  Ecology 88:2354-2363
#' @references Sawyer, H., M. J. Kauffman, R. M. Nielson and J. S. Horne.  2009.  Identifying and prioritizing ungulate migration routes for landscape-level conservation.  Ecological Applications 19: 2016-2025.
#' @author Bret A. Collier <bret@@lsu.edu>
#' @keywords moveud

bbmm.polygon=function(x, crs.current, crs.utm, lev, plot=FALSE, path, indID){
	coordinates(x)= ~ Lon + Lat
	proj4string(x)=CRS(crs.current)
	x=data.frame(spTransform(x, CRS(crs.utm)))
	out.bbmm=brownian.bridge(x=x$Lon, y=x$Lat, time.lag=x$tl[-1], location.error=15, cell.size=20, max.lag=180)
	contours=bbmm.contour(out.bbmm, levels=lev, locations=x, plot=plot)
	probs <- data.frame(x=out.bbmm$x,y=out.bbmm$y,z=out.bbmm$probability)
	out.raster <- rasterFromXYZ(probs,crs=CRS(crs.utm),digits=5) 
	raster.contour <- rasterToContour(out.raster,levels=contours$Z) 
	raster.contour <- spChFIDs(raster.contour,paste(lev,"% Contour Line",sep="")) 
	out=spTransform(raster.contour, CRS(crs.utm))
	out=SpatialLines2PolySet(out)
	out=PolySet2SpatialPolygons(out)
	out=as(out, "SpatialPolygonsDataFrame")
	out$UDlevels=paste(rev(lev))
	out$BandID=paste(indID)
	setwd(path)
	writeOGR(obj=out,dsn=".",layer=paste(indID),driver="ESRI Shapefile")
}

