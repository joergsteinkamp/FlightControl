#' split the callSign by a temporal break
#'
#' @param flights the flights data.frame
#' @param pause.limit which temporal delta time should be considered as new flight
#' @return new flights data.frame
#' @export
#' @author Joerg Steinkamp \email{joergsteinkamp@@yahoo.de}
split.pause.callSign <- function(flights, pause.limit=900) {
  flights.lst <- lapply(unique(flights$callSign), function(x) {
    callSign=NULL
    f  <- subset(flights, callSign == x)
    dt <- diff(f$date)
    sp <- which(dt > pause.limit)
    if (length(sp) == 0)
      return(f)
    for (i in 1:length(sp)) {
      ransuff <- paste0(sample(c(letters, LETTERS, as.character(0:9)), 6, replace=TRUE), collapse="")
      if (i == 1) {
        f$callSign[1:sp[i]] = paste0(f$callSign[1:sp[i]], "_", ransuff)
      } else {
        f$callSign[(sp[i - 1] + 1):sp[i]] = paste0(f$callSign[(sp[i - 1] + 1):sp[i]], "_", ransuff)
      }
      if (i == length(sp)) {
        f$callSign[(sp[i] + 1):nrow(f)] = paste0(f$callSign[(sp[i] + 1):nrow(f)], "_", ransuff)
      }
    }
    return(f)
  })
  flights <- data.frame()
  for (i in 1:length(flights.lst)) {
    flights <- rbind(flights, flights.lst[[i]])
  }
  return(flights)
}

#' Filter for a certain region
#'
#' Applies some subsets by coordinates
#'
#' @param flights the flights data.frame
#' @param west western edge
#' @param east eastern edge
#' @param south southern edge
#' @param north northern edge
#' @param length minimum number of nodes per flight track
#' @return new flights data.frame
#' @export
#' @author Joerg Steinkamp \email{joergsteinkamp@@yahoo.de}
filter.position <- function(flights, west=-180, east=180, south=-90, north=90, length=1) {
  lon=lat=NULL
  flights <- subset(flights, lon > west)
  flights <- subset(flights, lon < east)
  flights <- subset(flights, lat > south)
  flights <- subset(flights, lat < north)
  nodes   <- table(flights$callSign)
  flights <- subset(flights, flights$callSign %in% names(nodes[nodes >= length]))
  return(flights)
}

#' Applies a filter by total altitude
#'
#' Creates subsets, if the plane is descending, ascending or passing by, based on a threshold value of total altitude change
#'
#' @param flights the flights data.frame
#' @param direction character, either"a(scending)", "d(escending)" or "p(assing)". For anything else all are returned in a list
#' @param threshold absolute treshold value to distinguish between the the previous classes.
#' @return new flights data.frame
#' @export
#' @importFrom plyr ddply .
#' @author Joerg Steinkamp \email{joergsteinkamp@@yahoo.de}
filter.direction <- function(flights, direction="xxx", threshold=500) {
  summarise=dz=NULL
  flights$dz = 0
  for (callSign in unique(flights$callSign)) {
    callSign.id <- which(flights$callSign == callSign)
    flights$dz[callSign.id[2:length(callSign.id)]] = diff(flights$altitude[callSign.id])
  }
  rm(callSign, callSign.id)
  flights$callSign = gsub("\\?", "_", flights$callSign)
  total.dz <- ddply(flights[,c("callSign", "dz")], .(callSign), summarise, dz=sum(dz))

  pass.flights <- subset(flights, callSign %in% total.dz$callSign[abs(total.dz$dz) < abs(threshold)])
  asc.flights  <- subset(flights, callSign %in% total.dz$callSign[total.dz$dz >= abs(threshold)])
  dec.flights  <- subset(flights, callSign %in% total.dz$callSign[total.dz$dz <= -abs(threshold)])
  if (grepl("^p", direction)) {
    return(pass.flights)
  } else if (grepl("a", direction)) {
    return(asc.flights)
  } else if (grepl("d", direction)) {
    return(dec.flights)
  } else {
    return(list(pass=pass.flights, ascend=asc.flights, decend=dec.flights))
  }
}

## calculate a surface of the minimum flight height
##
## Interpolation methods:
## \itemize{
## \item{raw}{no interpolation}
## \item{idw}{kriging see \code{\link[gstst]{krige}}}
## \item{sph/gau}{variogram model see \code{\link[gstat]{vgm}}}
## }
##
## @param flights flights data.frame as returned by
## @param extent a spatial extent object
## @param res target resolution
## @param proj target projection
## @param method interpolation method (see Details)
## @return a interpolated SpatialPixelsDataFrame of minimum flight altitude
## @author Joerg Steinkamp \email{joergsteinkamp@@yahoo.de}
## @export
#' @import sp
#' @import raster
#' @importFrom gstat idw krige
### This resulted somehow in warnings during R CMD check
### @importFrom sp coordinates gridded spTransform proj4string CRS GridTopology as.SpatialPolygons.GridTopology over bbox
### @importFrom raster projection
#' @importFrom gstat variogram fit.variogram vgm
min.flight.altitude <- function(flights, extent=NULL, res=NULL, proj=NULL, method="raw") {
  lon=lat=NULL
  x <- seq(floor(extent@xmin / res) * res,
           ceiling(extent@xmax / res) * res, res)
  y <- seq(ceiling(extent@ymax / res) * res,
           floor(extent@ymin / res) * res, -res)

  grid <- data.frame(x=rep(x, length(y)), y=rep(y, each=length(x)))
  coordinates(grid) = ~x+y
  gridded(grid) = TRUE
  projection(grid) = CRS(proj)

  ## reproject
  coordinates(flights) = ~lon+lat
  projection(flights) = CRS("+init=epsg:4326")
  flights <- spTransform(flights, CRS(proj4string(grid)))

  ## minimum altitude in raster -> new spatial object (altitude)
  ## taken from http://stackoverflow.com/questions/24702437/in-r-how-to-average-spatial-points-data-over-spatial-grid-squares
  gt <- GridTopology(bbox(flights)[,1], cellsize=c(res, res), cells.dim=c(length(x), length(y)))
  polys <- as.SpatialPolygons.GridTopology(gt)
  proj4string(polys) <- CRS(proj)

  altitude <- as.data.frame(grid)
  altitude$low = NA
  ## how to change this to an apply call?
  for (i in 1:length(polys)) {
    altitude$low[i] = min(flights$altitude[which(!is.na(over(flights, polys[i])))], na.rm=TRUE)
  }
  altitude = altitude[is.finite(altitude$low), ]

  coordinates(altitude) = ~x+y
  projection(altitude) = CRS(proj)

  if (method=="raw")
    return(as(altitude, "SpatialPixelsDataFrame"))

  ## from the gstat manual (https://cran.r-project.org/web/packages/gstat/vignettes/gstat.pdf)
  if (method=="idw") {
    altitude.idw <- idw(low~1, altitude, grid)
    min.altitude <- altitude.idw["var1.pred"]
    names(min.altitude) <- c("altitude")
    return(altitude.idw["var1.pred"])
  }

  altitude.vgm = variogram(low~1, altitude)
  if (grepl("sph", tolower(method))) {
    altitude.fit.sph = fit.variogram(altitude.vgm, model = vgm("Sph"))
    altitude.kriged.sph = krige(low~1, altitude, grid, model = altitude.fit.sph)
    min.altitude <- altitude.kriged.sph["var1.pred"]
    names(min.altitude) <- c("altitude")
    return(min.altitude)
  } else if (grepl("gau", tolower(method))) {
    altitude.fit.gau = fit.variogram(altitude.vgm, model = vgm("Gau"))
    altitude.kriged.gau = krige(low~1, altitude, grid, model = altitude.fit.gau)
    min.altitude <- altitude.kriged.gau["var1.pred"]
    names(min.altitude) <- c("altitude")
    return(min.altitude)
  }
  stop(paste0("The method '", method, "' is not known/implemented."))
}
