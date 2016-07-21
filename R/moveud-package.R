#' Simple export routines for utilization distributions for BBMM and dbbmm objects
#' 
#' This package creates either a \code{SpatialLineDataFrame} or a \code{SpatialPolygonsDataFrame} and outputs individual step-specific shapefiles of contour lines
#' representing the utlization distribution for a specific time step for dynamic brownian bridge movement models
#' as detailed in Kranstauber et al. (2012).  Additionally, this package allows for exporting \code{bbmm.contour()} output to 
#' a polygon shapefile as well.  Note that this package uses a significant number of other R packages to stitch the various things together
#' so I wanted to give credit where it was due so be sure to see Description for the list of required packages I used. 
#' 
#' \tabular{ll}{ Package: \tab moveud \cr Type: \tab Package\cr Version:
#' \tab 1.1.0\cr Date: \tab 2016-05-26\cr License: \tab GPL (>=2)\cr LazyLoad: \tab
#' yes\cr }
#' 
#' @name moveud-package
#' @aliases moveud-package moveud
#' @docType package
#' @author Bret Collier <bret@@lsu.edu>
#' @references Kranstauber, D., R. Kays, S. D. Lapoint, M. Wikelski, and K. Safi.  2012.  A dynamic Brownian bridge movement model to estimate
#' utlization distributions for heterogenous animal movement.  Journal of Animal Ecology 81: 738-746. DOI: 10.1111/j.1365-2656.2012.01955.x.
#' @references Horne, J. S., E. O. Garton, S. M. Krone, and J. S. Lewis.  2007.  Analyzing animal movements using Brownian bridges.  Ecology 88:2354-2363
#' @references Sawyer, H., M. J. Kauffman, R. M. Nielson and J. S. Horne.  2009.  Identifying and prioritizing ungulate migration routes for landscape-level conservation.  
#' Ecological Applications 19: 2016-2025.
#' @references Byrne, M. E., J. C. McCoy, J. Hinton, M. J. Chamberlain, and B. A. Collier. 2014. Using dynamic brownian bridge movement modeling to measure temporal 
#' patterns of habitat selection. Journal of Animal Ecology,  83: 1234-1243.
#' @keywords package
#' 
NULL

#' Raw turkey dataset
#' 
#' @details The raw turkey dataset is exactly what it says it is, a unformatted dataset which I have not turned into a \code{move} object, however, the code
#' for manipulation a raw dataset such as this into a \code{move} object is included below.  Additionally, the code below suggests that the user look at their
#' data before creating the \code{move} object, as this is a nice example of having one outlier point that causes problems when estimating the utilization
#' distribution.
#' 
#' @name rawturkey
#' @docType data
#' @format The format is a dataframe 
#' \describe{\item{Try}{Unique identifier for each GPS location attempt, NA's are allowed}
#' \item{FixDate}{Date for each fix taken}
#' \item{FixTime}{Time for each fix taken}
#' \item{Lat}{Latitude}
#' \item{Lon}{Longitude}
#' \item{Sats}{Number of satellites used to calculate location}
#' \item{HDOP}{Horizontal dilution of precision; measure of positional accuracy (lower is better)}}
#' @keywords datasets
#' @references Guthrie, J. D., M. E. Byrne, J. B. Hardin, C. O. Kochanny, K. L. Skow, R. T. Snelgrove, M. J. Butler, M. J. Peterson, M. J. Chamberlain, 
#' and B. A. Collier. 2011. Evaluation of a GPS backpack transmitter for wild turkey research. Journal of Wildlife Management 75:539-547.
#' @examples 
#' 
#' \donttest{
#' data(rawturkey)
#' head(rawturkey)
#' plot(rawturkey$Lon, rawturkey$Lat)
#' rawturkey=rawturkey[rawturkey$Lon< -98.1,]
#' turkey=na.omit(rawturkey)
#' #rawturkey$DateTime=strptime(paste(rawturkey$FixDate, rawturkey$FixTime),"%m/%d/%Y %H:%M:%S") 
#' #Create object of class: move
#' #move.out <- move(x=turkey$Lon, y=turkey$Lat, time=as.POSIXct(turkey$DateTime, tz="GMT"), data=turkey, proj=CRS("+proj=longlat"), animal="3057")
#' #out <- spTransform(move.out, CRSobj="+proj=aeqd", center=TRUE)
#' #turkey <- brownian.bridge.dyn(object=out, location.error=15, margin=5, window.size=21, dimSize=85)
#' }
#' 
#' 
#' 
NULL

#' Turkey dataset
#' 
#' The data included in this package can be accessed using \code{data(turkey)} represent a dbbmm object for a single Rio Grande wild turkey 
#' tagged with a GPS transmitter in Texas during 2009.  
#' 
#' @details  This package contains 2 functions \code{move.contour} and \code{move.forud} both which create contour lines for user-specified utilization distributions for individual
#' time steps using the input of a \code{turkey} object from the R package move and the contour function \code{bbmm.contour} from the R package \code{BBMM}.
#' 
#' @name turkey
#' @docType data
#' @format The format is a turkey object of Rio Grande wild turkey movements created with the R package move
#' @keywords datasets
#' @references Guthrie, J. D., M. E. Byrne, J. B. Hardin, C. O. Kochanny, K. L. Skow, R. T. Snelgrove, M. J. Butler, M. J. Peterson, M. J. Chamberlain, 
#' and B. A. Collier. 2011. Evaluation of a GPS backpack transmitter for wild turkey research. Journal of Wildlife Management 75:539-547.
#' @examples 
#' 
#' \donttest{
#' data(turkey)
#' move.contour(turkey, 10:12, 15, c(50, 95), "C:/", "Test", crs="+proj=longlat +ellps=WGS84")
#' }
#' 
#' \donttest{
#' data(turkey)
#' move.forud(turkey, range.subset=10:12, le=6, lev=c(50, 95), ras=5, ts=.2, path="C:/", name="test", crs="+proj=longlat +ellps=WGS84", ID="123") 
#' }
#' 
#' 
#' 
NULL



