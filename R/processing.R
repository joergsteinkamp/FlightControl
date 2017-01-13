#' rename the callSign at a temporal break
#'
#' A random string of six characters is attached to the callSign each time it appears again after more than `pause.limit` seconds.
#'
#' @param flights the flights data.frame
#' @param pause.limit which temporal delta time should be considered as new flight
#' @return new flights data.frame
#' @export
#' @author Joerg Steinkamp \email{joergsteinkamp@@yahoo.de}
breakCallSign <- function(flights, pause.limit=900) {
  flights.lst <- lapply(unique(flights$callSign), function(x) {
    callSign=NULL
    f  <- subset(flights, callSign == x)
    dt <- as.double(diff(f$date), units="secs")
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
        ransuff <- paste0(sample(c(letters, LETTERS, as.character(0:9)), 6, replace=TRUE), collapse="")
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
FlightsPositionSubset <- function(flights, west=-180, east=180, south=-90, north=90, length=1) {
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
#' @param direction character, either"a(scending)", "d(escending)" or "p(assing)". For anything else the full flights data.frame is returned including a new column `dz`.
#' @param threshold absolute treshold value to distinguish between the the previous classes.
#' @return new flights data.frame
#' @export
#' @importFrom plyr ddply .
#' @author Joerg Steinkamp \email{joergsteinkamp@@yahoo.de}
flightsDirectionSplit <- function(flights, direction=NA, threshold=500) {
  summarise=dz=NULL
  flights$dz = 0
  for (callSign in unique(flights$callSign)) {
    callSign.id <- which(flights$callSign == callSign)
    flights$dz[callSign.id[2:length(callSign.id)]] = diff(flights$altitude[callSign.id])
  }
  rm(callSign, callSign.id)

  if (!(grepl("^p", direction) || grepl("^a", direction) || grepl("^d", direction)))
    return(flights)

  ## flights$callSign = gsub("\\?", "_", flights$callSign)
  total.dz <- ddply(flights[,c("callSign", "dz")], .(callSign), summarise, dz=sum(dz))

  pass.flights <- subset(flights, callSign %in% total.dz$callSign[abs(total.dz$dz) < abs(threshold)])
  asc.flights  <- subset(flights, callSign %in% total.dz$callSign[total.dz$dz >= abs(threshold)])
  dec.flights  <- subset(flights, callSign %in% total.dz$callSign[total.dz$dz <= -abs(threshold)])
  if (grepl("^p", direction)) {
    return(pass.flights)
  } else if (grepl("^a", direction)) {
    return(asc.flights)
  } else if (grepl("^d", direction)) {
    return(dec.flights)
  }
}

#' apply a function on al flight nodes per gridcell
#'
#' @param data the flights data.frame (at least with column names "lon", "lat" and "altitude")
#' @param FUN function to apply
#' @param col.names column names of the flights data.frame to use 1st for x-coordinate, 2nd for y-coordinate and 3rd for z-coordinate
#' @param proj.in proj4string: projection of the input flights data.frame
#' @param proj.out proj4string: projection of the output
#' @param res.out output resulution in the unit of proj.out
#' @param ... further parameters passed to FUN
#' @return a SpatialPixelsDataFrame
#' @export
#' @import sp
#' @import raster
#' @author Joerg Steinkamp \email{joergsteinkamp@@yahoo.de}
#' @examples
#' library(raster)
#' extent <- extent(c(7.25, 8.5, 49.5, 50.2))
#' N <- 750
#' set.seed(987654)
#' data <- data.frame(lon=rbeta(N, 5, 2) * (extent@xmax - extent@xmin) + extent@xmin,
#'                    lat=rbeta(N, 2.5, 2.5) * (extent@ymax - extent@ymin) + extent@ymin)
#' data$altitude = (data$lon - 7.9)^3 - 3 * (data$lon - 7.9) * (data$lat- 50)^2
#' fgrid = gridFlights(data, min)
#' plot(fgrid)
gridFlights <- function(data, FUN, col.names=c("lon", "lat", "altitude"),
                         proj.in="+init=epsg:4326",
                         proj.out="+proj=utm +zone=32 +ellps=WGS84 +dataum=WGS84 +units=m +no_defs",
                         res.out=1000,
                         ...) {

  if (col.names[1] != "lon") {
    df.cnames <- colnames(data)
    i <- which(df.cnames == col.names[1])
    df.cnames[i] = "lon"
    colnames(data) <- df.cnames
  }
  if (col.names[2] != "lat") {
    df.cnames <- colnames(data)
    i <- which(df.cnames == col.names[2])
    df.cnames[i] = "lat"
    colnames(data) <- df.cnames
  }
  if (col.names[3] != "altitude") {
    df.cnames <- colnames(data)
    i <- which(df.cnames == col.names[3])
    df.cnames[i] = "altitude"
    colnames(data) <- df.cnames
  }

  if (!is.function(FUN))
    stop("'FUN' is not a function!")

  ## reproject input data.frame
  coordinates(data) <- ~lon+lat
  projection(data) <- CRS(proj.in)
  data <- spTransform(data, CRS(proj.out))

  ## create the underlying grid coordinates
  x <- seq(floor(extent(data)@xmin / res.out) * res.out - res.out / 2,
           ceiling(extent(data)@xmax / res.out) * res.out + res.out / 2, res.out)
  y <- seq(ceiling(extent(data)@ymax / res.out) * res.out + res.out / 2,
           floor(extent(data)@ymin / res.out) * res.out - res.out / 2, -res.out)

  ## convert input back to data.frame
  data = as.data.frame(data)

  ## loop over all gridcells, selecting the
  data.lst = lapply(x[1:(length(x)-1)], function(x) {
    x.id <- which(data$lon > x & data$lon <= x + res.out)
    if (length(x.id) == 0) {
      return(data.frame(x=x + res.out/2, y=y[1:(length(y)-1)] + diff(y)/2, altitude=NA))
    } else {
      data.tmp = data[x.id, ]
      data.tmp.lst = lapply(y[1:(length(y)-1)], function(y) {
        y.id <- which(data.tmp$lat < y & data.tmp$lat >= y - res.out)
        if (length(y.id) == 0) {
          return(data.frame(x=x + res.out/2, y=y - res.out/2, altitude=NA))
        } else {
          alt <- FUN(data.tmp$altitude[y.id], ...)
          ## return(data.frame(x=x + res.out/2, y=y - res.out/2, altitude=min(data.tmp$altitude[y.id])))
          return(data.frame(x=x + res.out/2, y=y - res.out/2, altitude=alt))
        }
      })
      return(data.tmp.lst)
    }
  })
  ## loop over the list
  new.data = data.frame()
  for (i in 1:length(data.lst)) {
    if (is.data.frame(data.lst[[i]])) {
      new.data = rbind(new.data, data.lst[[i]])
    } else if (is.list(data.lst[[i]])) {
      for (j in 1:length(data.lst[[i]])) {
        if (is.data.frame(data.lst[[i]][[j]])) {
          new.data = rbind(new.data, data.lst[[i]][[j]])
        } else {
          stop("BUG: This should not have happened!")
        }
      }
    } else {
      stop("BUG: This should not have happened!")
    }
  }
  ## new.data = new.data[complete.cases(new.data), ]

  coordinates(new.data) = ~x+y
  projection(new.data) = CRS(proj.out)
  return(as(new.data, "SpatialPixelsDataFrame"))
}

#' Spatial interpolation
#'
#' @param data a SpatialPixelsDataFrame
#' @param method interpolation method. One of 'loess' (default), 'gam', 'gau' or 'sph'
#' @param k if method='gam' parameter for the smooth term in te
#' @param ... further parameters passed on to 'loess'.
#' @return a SpatialPixelsDataFrame
#' @export
#' @import sp
#' @import raster
#' @importFrom methods as
#' @importFrom stats loess
#' @author Joerg Steinkamp \email{joergsteinkamp@@yahoo.de}
#' @examples
#' require(raster)
#' extent <- extent(c(7.25, 8.5, 49.5, 50.2))
#' N <- 750
#' set.seed(987654)
#' data <- data.frame(lon=rbeta(N, 5, 2) * (extent@xmax - extent@xmin) + extent@xmin,
#'                    lat=rbeta(N, 2.5, 2.5) * (extent@ymax - extent@ymin) + extent@ymin)
#' data$altitude = (data$lon - 7.9)^3 - 3 * (data$lon - 7.9) * (data$lat- 50)^2
#' fgrid = gridFlights(data, min)
#' res <- smoothFlightsGrid(fgrid, "loess")
#' names(res) <- "loess"
#' res2 <- smoothFlightsGrid(fgrid, "gam")
#' res@data$gam = res2@data$altitude
#' res2 <- smoothFlightsGrid(fgrid, "gau")
#' res@data$gau = res2@data$altitude
#' res2 <- smoothFlightsGrid(fgrid, "sph")
#' res@data$sph = res2@data$altitude
#' spplot(res)
smoothFlightsGrid <- function(data, method="loess", k=5, ...) {
  ## TODO: check input data formats
  ## TODO: pass on ... to te in gam-method

  ## create the output grid
  x <- seq(data@bbox["x", "min"] + data@grid@cellsize["x"] / 2,
           data@bbox["x", "max"] - data@grid@cellsize["x"] / 2,
           data@grid@cellsize["x"])
  y <- seq(data@bbox["y", "max"] - data@grid@cellsize["x"] / 2,
           data@bbox["y", "min"] + data@grid@cellsize["x"] / 2,
           -data@grid@cellsize["y"])
  ogrid <- expand.grid(list(x=x, y=y))

  p.df <- as.data.frame(data)
  p.df <- subset(p.df, is.finite(p.df$altitude))
  ## default loess spline interpolation
  if (method == "loess") {
    loess.mod <- loess(altitude ~ x*y, data=p.df, ...)
    pred <- predict(loess.mod, newdata=ogrid)
    data$altitude = as.vector(t(pred))
    return(data)
    ## gam interpolation
  } else if (method == "gam") {
    if (!requireNamespace("mgcv", quietly = TRUE))
      stop("Package 'mgcv' not available! Method 'gam' not possible")
    gam.mod <- eval(parse(text=paste0("mgcv::gam(altitude ~ te(x, y, k=", k, "), data = p.df)")))
    pred <- predict(gam.mod, newdata=ogrid)
    data$altitude = as.vector(t(matrix(pred,c(length(x), length(y)))))
    return(data)
    ## idw (Inverse Distnace Weighting) interpolation from the gstat package
  } else if (method == "idw") {
    if (!requireNamespace("gstat", quietly = TRUE))
      stop("Package 'gstat' not available! Method 'idw' not possible")
    coordinates(p.df) = ~ x * y
    projection(p.df) <- CRS(proj4string(data))
    altitude.idw <- gstat::idw(altitude~1, p.df, data)
    data <- altitude.idw["var1.pred"]
    names(data) <- c("altitude")
    return(data["var1.pred"])
    ## kriging
  } else if (tolower(method) == "sph" || tolower(method) == "gau") {
    if (!requireNamespace("gstat", quietly = TRUE))
      stop(paste0("Package 'gstat' not available! Method '", method, "' not possible"))
    coordinates(p.df) = ~ x * y
    projection(p.df) <- CRS(proj4string(data))

    altitude.vgm = gstat::variogram(altitude~1, p.df)

    if (tolower(method) == "sph") {
      altitude.fit = gstat::fit.variogram(altitude.vgm, model = gstat::vgm("Sph"))
    } else {
      altitude.fit = gstat::fit.variogram(altitude.vgm, model = gstat::vgm("Gau"))
    }
    altitude.kriged = gstat::krige(altitude~1, p.df, data, model = altitude.fit)
    data <- altitude.kriged["var1.pred"]
    names(data) <- c("altitude")
    return(data)
  } else {
    stop(paste0("The method '", method, "' is not known/implemented."))
  }
}
