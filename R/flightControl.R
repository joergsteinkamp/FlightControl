#' read the air traffic csv file
#'
#' reads a csv file recorded by \href{https://github.com/antirez/dump1090}{dump1090} and returns it as data.frame
#'
#' @param file path to and name of the csv file
#' @param raw logical i ftrue returns the full data set otherwise extract only position/altitude/time with callSign (flightID)
#' @param min.altitude ignore positions below that altitude in the given unit
#' @param max.altitude ignore positions above that altitude in the given unit
#' @param unit default m(eter). If anything else than f(eet) is given the default is used
#' @return data.frame
#' @author Joerg Steinkamp \email{joergsteinkamp@@yahoo.de}
#' @references \href{https://github.com/antirez/dump1090}{dump1090}
#' @export
#' @importFrom utils read.table
airplanes <- function(file, raw=FALSE, min.altitude=-999, max.altitude=5000, unit="m") {
  message=type=altitude=hexIdent=callSign=NULL
  flight.raw <- read.table(file, sep=",", stringsAsFactors=FALSE)
  if (raw)
    return(flight.raw)
  colnames(flight.raw) <- c("message", "type",
                            "sessionID", "aircraftID", "hexIdent", "flightID",
                            "gen.date", "gen.time", "log.date", "log.time",
                            "callSign", "altitude", "groundSpeed", "track",
                            "lat", "lon", "verticalRate", "squawk", "alert",
                            "emergency", "SPI", "isOnGround")

  flight.ids <- subset(flight.raw, message == "MSG" & type == 1)[, c(5, 7:11)]
  flight.pos <- subset(flight.raw, message == "MSG" & type == 3)[, c(5, 7:10, 12, 15, 16)]

  ## remove empty spaces in callSign
  flight.ids$callSign = gsub(" ", "", flight.ids$callSign)

  ## feet -> m
  if (!grepl("^f", tolower(unit)))
    flight.pos$altitude = 0.3048 * flight.pos$altitude
  ## only above min.altitude
  flight.pos = subset(flight.pos, altitude >= min.altitude)
  ## only below max.altitude
  flight.pos = subset(flight.pos, altitude <= max.altitude)


  ## create a real datetime column
  flight.ids$date <- strptime(paste(flight.ids$gen.date, flight.ids$gen.time), format="%Y/%m/%d %H:%M:%S")
  flight.pos$date <- strptime(paste(flight.pos$gen.date, flight.pos$gen.time), format="%Y/%m/%d %H:%M:%S")

  ## first occurence of hexIdent/callSign combination (MSG type 1)
  first.ids <- flight.ids
  first.ids$hexCallID = paste0(first.ids$hexIdent, first.ids$callSign)
  first.ids <- first.ids[match(unique(first.ids$hexCallID), first.ids$hexCallID), ]

  ## relate the callSign (flight number) to the hexIdent in the flight.pos data.frame.
  ## apply with half an hour (1800s) threshold (if positions were sent in advance of first ID)
  ## and also if the airplane returns
  fpos.tmp <- apply(first.ids, 1, function(x) {
    ## makes it much faster, when subset is  split in two steps
    fpos <- subset(flight.pos, hexIdent == x['hexIdent'])
    fpos <- subset(fpos, date > as.POSIXlt(x['date']) - 1800 &
                         date < as.POSIXlt(x['date']) + 3600)
    if (nrow(fpos)==0)
      return(NULL)
    fpos$callSign = x['callSign']
    return(fpos)
  })
  fpos <- fpos.tmp[[1]]
  for (i in 2:length(fpos.tmp)) {
    fpos <- rbind(fpos, fpos.tmp[[i]])
  }
  ## save those flight positions, where no callSign could be found
  fpos.tmp <- merge(flight.pos, fpos, all=TRUE)
  fpos.tmp <- subset(fpos.tmp, is.na(callSign))

  ## some callSign appear again after quiet a long time.
  ## Use a threshold of half an hour here to split it
  for (id in unique(fpos$callSign)) {
    if (any(diff(fpos$date[fpos$callSign == id]) > 1800)) {
      flights.tmp <- subset(fpos, callSign == id)
      newCallSign <- flights.tmp$callSign
      tid <- which(diff(flights.tmp$date) > 1800)
      for (i in 1:length(tid)) {
        if (i != 1 && i != length(tid)) {
          newCallSign[(tid[i-1]+1):tid[i]] = paste0(id, letters[i+1])
        } else {
          if (i==1) {
            newCallSign[1:tid[i]] = paste0(id, letters[i])
          }
          if (i==length(tid)) {
            newCallSign[(tid[i]+1):nrow(flights.tmp)] = paste0(id, letters[i+1])
          }
        }
      }
      fpos$callSign[fpos$callSign==id] = newCallSign
    }
  }
  flight.pos <- rbind(fpos, fpos.tmp)
  rownames(flight.pos) <- 1:nrow(flight.pos)
  return(flight.pos)
}

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
      if (i == 1) {
        f$callSign[1:sp[i]] = paste0(f$callSign[1:sp[i]], "_", letters[1])
      } else {
        f$callSign[(sp[i - 1] + 1):sp[i]] = paste0(f$callSign[(sp[i - 1] + 1):sp[i]], "_", letters[i])
      }
      if (i == length(sp)) {
        f$callSign[(sp[i] + 1):nrow(f)] = paste0(f$callSign[(sp[i] + 1):nrow(f)], "_", letters[i + 1])
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

## add linear interpolated points to flight tracks
##
## @param flights the flights data.frame
## @param res spatial resolution
## @return a new flights data.frame with more rows, but some neglected columns
## @author Joerg Steinkamp \email{joergsteinkamp@@yahoo.de}
add.knots <- function(flights, res) {
  UID=NULL
  ## add linear interpolated points to flight track in the distance of sqrt(res^2 + res^2),
  ## if the distance is larger.
  ## make to also work for several days by including the date in a unique ID
  flights$UID = paste0(flights$callSign, strftime(flights$date, format="%Y%m%d"))

  ## list of high resolution flight tracks
  lhres.flights <- lapply(unique(flights$UID), function(x){
    flights.tmp = subset(flights, UID == x)[c("lon", "lat", "date", "altitude", "callSign")]
    flights = flights[!duplicated(flights[c("lon", "lat")]),]
    hres.flights <- flights.tmp[1,]
    if (nrow(flights.tmp) > 1) {
      for (i in 2:nrow(flights.tmp)) {
        n <- round(sqrt((flights.tmp$lon[i - 1] - flights.tmp$lon[i])^2 +
                        (flights.tmp$lat[i - 1] - flights.tmp$lat[i])^2) / res)
        if (n > 1) {
          hres.flights <- rbind(hres.flights, data.frame(
                                                lon=seq(flights.tmp$lon[i - 1], flights.tmp$lon[i], length.out=n),
                                                lat=seq(flights.tmp$lat[i - 1], flights.tmp$lat[i], length.out=n),
                                                date=seq(flights.tmp$date[i - 1], flights.tmp$date[i], length.out=n),
                                                altitude=seq(flights.tmp$altitude[i - 1], flights.tmp$altitude[i], length.out=n),
                                                callSign=flights.tmp$callSign[1])[2:n, ])
        } else {
          hres.flights <- rbind(hres.flights, flights.tmp[i, ])
        }
      }
    }
    return(hres.flights)
  })
  hres.flights <- lhres.flights[[1]]
  for (i in 2:length(lhres.flights)) {
    hres.flights <- rbind(hres.flights, lhres.flights[[i]])
  }
  return(hres.flights[!duplicated(hres.flights), ])
}

#' calculate a surface of the minimum flight height
#'
#' Interpolation methods:
#' \itemize{
#' \item{raw}{no interpolation}
#' \item{idw}{kriging see \code{\link[gstst]{krige}}}
#' \item{sph/gau}{variogram model see \code{\link[gstat]{vgm}}}
#' }
#'
#' @param flights flights data.frame as returned by
#' @param extent a spatial extent object
#' @param res target resolution
#' @param proj target projection
#' @param method interpolation method (see Details)
#' @return a interpolated SpatialPixelsDataFrame of minimum flight altitude
#' @author Joerg Steinkamp \email{joergsteinkamp@@yahoo.de}
#' @export
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

  ## BUG: hardcoded resolution increase of flight tracks; approx. every sqrt(2 * res^2)
  ## Improvement/Solution: check unit of projection and scale based on center of extent

  ## takes too long and no hardly any gain
  ##flights <- add.knots(flights, 30 / 3.6e6 * res)

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

  if (method=="spline") {
    stop("doesn't work currectly!")
    ## http://stackoverflow.com/questions/28059182/smoothing-out-ggplot2-map
    ##fld <- interp(x=altitude$lon, y=altitude$lat, z=altitude$min,
    ##              duplicate="min", xo=lon, yo=lat, extrap=TRUE, linear=FALSE)
    ##return(fld)

    ##http://stackoverflow.com/questions/20848740/smoothing-surface-plot-from-matrix
    ##mod <- gam(min ~ te(lon, lat), data = altitude)
    ##m2 <- matrix(fitted(mod), nrow = length(lon), ncol = length(lat))
    ##return(m2)
  }

  coordinates(altitude) = ~x+y
  projection(altitude) = CRS(proj)

  if (method=="raw")
    return(altitude)

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
##  } else if (grepl("spl", tolower(method))) {
##    altitude.fit.spl = fit.variogram(altitude.vgm, model = vgm("Spl"))
##    altitude.kriged.spl = krige(min~1, altitude, grid, model = altitude.fit.spl)
##    colnames(altitude.kriged.spl["var1.pred"]@data) <- "altitude"
##    return(altitude.kriged.spl["var1.pred"])
  }
  stop(paste0("The method '", method, "' is not known/implemented."))
}

#' 2D map animation of series of pictures
#'
#' Creates a time series of images in tempdir(). If present it uses ffmpeg to create an animation out of them.
#'
#' @param flights the flights data.frame
#' @param step a new picture every "step" minutes"
#' @export
#' @author Joerg Steinkamp \email{joergsteinkamp@@yahoo.de}
#' @import OpenStreetMap
### @importFrom OpenStreetMap openmap openproj
#### @importMethodsFrom OpenStreetMap autoplot
#' @importFrom ggplot2 autoplot aes geom_path annotate scale_colour_gradientn xlab ylab
#' @importFrom viridis inferno
#' @importFrom grDevices dev.off png
animate.flights.2d <- function(flights, step=5) {
  lon=lat=altitude=callSign=NULL
  extent <- extent(x=c(min(flights$lon), max(flights$lon)),
                   y=c(min(flights$lat), max(flights$lat)))
  extent@xmin <- extent@xmin - (extent@xmax-extent@xmin) * 0.05
  extent@xmax <- extent@xmax + (extent@xmax-extent@xmin) * 0.05
  extent@ymin <- extent@ymin - (extent@ymax-extent@ymin) * 0.05
  extent@ymax <- extent@ymax + (extent@ymax-extent@ymin) * 0.05

  ## retrieve the OpenSteetMap data and reproject it
  map <- openmap(c(extent@ymax, extent@xmin), c(extent@ymin, extent@xmax),
                 type = "apple-iphoto", minNumTiles=10)
  mapLatLon <- openproj(map)

  if (requireNamespace("Cairo", quietly = TRUE)) {
    Cairo::CairoPNG(file.path(tempdir(), "movie%04d.png"), width=1280, height=1024, res=96)
  } else {
    png(file.path(tempdir(), "movie%04d.png"), width=1280, height=1024)
  }
  ## one hour time offset to keep flight tracks on map
  time.offset <- 3600
  delta.sec <- step * 60 - as.numeric(strftime(min(flights$date), format="%S"))
  delta.sec[2] = step * 60 - as.numeric(strftime(max(flights$date), format="%S"))
  time.seq <- seq(min(flights$date) + delta.sec[1] + time.offset,
                  max(flights$date) + delta.sec[2], by=step * 60)

  ## loop over time steps to create individual figures
  for (i in 1:length(time.seq)) {
    p <- autoplot(mapLatLon)
    p <- p + geom_path(data=subset(flights, date > time.seq[i] - time.offset & date < time.seq[i]),
                       aes(x=lon, y=lat, color=altitude, group=callSign),
                       alpha=0.5, size=0.8)
    p <- p + annotate("text",
                      x=extent@xmin + 0.075*(extent@xmax - extent@xmin), y=min(flights$lat),
                      label=strftime(time.seq[i], "%d-%m-%Y %H:%M"))
    p <- p + scale_colour_gradientn(colours=inferno(255))
    p <- p + xlab("longitude") + ylab("latitude")
    print(p)
  }
  dev.off()
  ## https://www.ffmpeg.org/ffmpeg-utils.html#Video-size (-s option, adjust also width and height above)
  if (length(system2("which", "ffmpeg", stdout=TRUE))>0) {
    system(paste0("ffmpeg -y -i ", file.path(tempdir(), "movie%04d.png"), " -s sxga -r ntsc -vcodec libx264 flightControl2D.mp4"))
    system(paste0("rm ", file.path(tempdir(), "movie*.png")))
  } else {
    message(paste0("'ffmpeg' not found. Files are in '", tempdir(), "'"))
  }
  ## one individual figure with all flight tracks
  ##CairoPNG("flightControl.png", width=1280, height=800, res=96)
  p <- autoplot(mapLatLon)
  p <- p + geom_path(data=flights,
                     aes(x=lon, y=lat, color=altitude, group=callSign),
                     alpha=0.5, size=0.8)
  p <- p + scale_colour_gradientn(colours=inferno(255))
  p <- p + xlab("longitude") + ylab("latitude")
  print(p)
  ##dev.off()
}

#' 3D isosurface of lowest flight altitude
#'
#' @param flights the flights data.frame
#' @param dem digital elevation model
#' @param tracks logical if the individual tracks should be shown (default) or a transparent 'carpet' of the spatially interpolated lowest flight altitude.
#' @author Joerg Steinkamp \email{joergsteinkamp@@yahoo.de}
#' @export
#' @import rgl
#' @importFrom methods as
#' @importFrom raster raster projectRaster crop projection
#' @importFrom rgdal project
#' @importFrom sp CRS spTransform proj4string coordinates
#' @importFrom grDevices terrain.colors
animate.flights.3d <- function(flights, dem, tracks=TRUE) {
  callSign=NULL
  extent <- extent(data.frame(x=c(min(flights$lon), max(flights$lon)),
                              y=c(min(flights$lat), max(flights$lat))))
  extent@xmin <- extent@xmin - (extent@xmax-extent@xmin)*0.05
  extent@xmax <- extent@xmax + (extent@xmax-extent@xmin)*0.05
  extent@ymin <- extent@ymin - (extent@ymax-extent@ymin)*0.05
  extent@ymax <- extent@ymax + (extent@ymax-extent@ymin)*0.05

  ## project to UTM zone 32U
  proj   <- "+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
  extent <- as(extent, "SpatialPolygons")
  proj4string(extent) <- CRS("+init=epsg:4326")
  extent <- spTransform(extent, CRS(proj))
  extent <- extent(extent)

  ## TODO: filter out helicopters, historic planes, ...

  ## calculate a raster surface ("carpet") of the lowest flight altitude.
  if (!tracks) {
    min.altitude <- min.flight.altitude(flights, extent=extent, res=1000, proj=proj, method="raw")
    if (class(min.altitude) != "SpatialPixelsDataFrame")
      min.altitude = as(min.altitude, "SpatialPixelsDataFrame")
  }

  ## reproject lon/lat of flights data.frame
  flights.proj <- project(as.matrix(flights[c("lon", "lat")]), proj)
  flights$lon <- flights.proj[,1]
  flights$lat <- flights.proj[,2]

  ## load a digital elevation model (DEM from SRTM data)
  ##full.srtm <- raster("data/srtm.tif")
  ##full.srtm <- projectRaster(full.srtm, crs=CRS(proj4string(min.altitude)))
  ##dem <- crop(full.srtm, extent)

  ## extract the coordinates and z-values for the 3D surface from the DEM
  xy <- coordinates(dem)
  dem.x <- sort(unique(xy[,1]))
  dem.y <- sort(unique(xy[,2]), decreasing=TRUE)
  dem.z <- t(as.matrix(dem))
  dem.z[dem.z==0] = NA
  dem.z[is.na(dem.z)] = min(dem.z, na.rm=TRUE)

  ## color lookup table for DEM
  zlim <- range(dem.z)
  zlen <- ceiling(zlim[2] - zlim[1])

  dem.col <- terrain.colors(zlen)
  dem.col <- dem.col[ ceiling(dem.z - zlim[1]) ]

  if (!tracks) {
    ## eqivalent as above for altitude
    alt.x <- seq(min.altitude@bbox["x", "min"] + min.altitude@grid@cellsize["x"]/2,
                 min.altitude@bbox["x", "max"] - min.altitude@grid@cellsize["x"]/2,
                 min.altitude@grid@cellsize["x"])
    alt.y <- seq(min.altitude@bbox["y", "min"] + min.altitude@grid@cellsize["y"]/2,
                 min.altitude@bbox["y", "max"] - min.altitude@grid@cellsize["y"]/2,
                 min.altitude@grid@cellsize["x"])
#    xy <- coordinates(min.altitude)
#    alt.x <- sort(unique(xy[,1]))
#    alt.y <- sort(unique(xy[,2]), decreasing=TRUE)
    alt.z <- as.matrix(min.altitude)
    zlim <- range(alt.z, na.rm=TRUE)
  } else {
    zlim <- range(flights$altitude, na.rm=TRUE)
  }
  zlen <- ceiling(zlim[2] - zlim[1])

  flight.alt.col <- inferno(zlen)

  if (tracks) {
    open3d(windowRect=c(100, 100, 1280, 1024), scale=c(1, 1, 5), FOV=1)
    rgl.bg(color = "black")
    surface3d(dem.x, dem.y, dem.z, color = dem.col, back = "cull")

    ## both methods (for and lapply) for all flight tracks are very slow!
    for (id in unique(flights$callSign)) {
      f = subset(flights, callSign == id)
      if (nrow(f) == 0)
        next
      lines3d(f$lon, f$lat, f$altitude, col=flight.alt.col[f$altitude], alpha=0.15, lwd=2)
    }

    rgl.viewpoint(0, -75, fov=1)

    ## play3d(spin3d(axis = c(0, 0, 1), rpm = 2))
    movie3d(spin3d(axis = c(0, 0, 1), rpm = 2), 30, dir=tempdir(), convert=FALSE, clean=FALSE)
    rgl.close()
    if (length(system2("which", "ffmpeg", stdout=TRUE))>0) {
      system(paste0("ffmpeg -y -i ", file.path(tempdir(), "movie%03d.png"), " -s sxga -r ntsc -vcodec libx264 -pix_fmt yuv420p flightControl3D_top.mp4"))
      system(paste0("rm ", file.path(tempdir(), "movie*.png")))
    } else {
      message(paste0("'ffmpeg' not found. Files are in '", tempdir(), "'"))
    }
  } else {
    ## flat look with "carpet"
    alt.col <- flight.alt.col[ ceiling(alt.z - zlim[1]) ]
    alt.alpha <- sqrt(max(alt.z)-alt.z) / sqrt(max(alt.z)-min(alt.z))/2

    open3d(windowRect=c(100, 100, 1280, 1024), scale=c(1, 1, 5), FOV=1)
    rgl.bg(color = "black")
    surface3d(dem.x, dem.y, dem.z, color = dem.col, back = "cull")

    surface3d(alt.x, alt.y, alt.z, color = "white", alpha=0.4, back = "cull")

    coords = data.frame(x=rep(alt.x, length(alt.y)), y=rep(alt.y, each=length(alt.x)), z=as.vector(alt.z))
    e <- expand.grid(1:(length(alt.x)-1), 1:length(alt.y))
    i1 <- apply(e,1,function(z)z[1]+length(alt.x)*(z[2]-1))
    i2 <- i1+1
    i3 <- i1+length(alt.x)
    i4 <- i2+length(alt.x)
    i <- rbind(i1,i2,i4,i3)
    quads3d(coords$x[i], coords$y[i], coords$z[i], col="white", alpha=0.4, front="fill", back="fill")

    rgl.viewpoint(0, -85, fov=1)

    movie3d(spin3d(axis = c(0, 0, 1), rpm = 2), 30, dir=tempdir(), convert=FALSE, clean=FALSE)
    rgl.close()
    if (length(system2("which", "ffmpeg", stdout=TRUE))>0) {
      system(paste0("ffmpeg -y -i ", file.path(tempdir(), "movie%03d.png"), " -s sxga -r ntsc -vcodec libx264 -pix_fmt yuv420p flightControl3D_flat.mp4"))
      system(paste0("rm ", file.path(tempdir(), "movie*.png")))
    } else {
      message(paste0("'ffmpeg' not found. Files are in '", tempdir(), "'"))
    }
  }
}

#' Convert csv to RData
#'
#' Convert daily csv files produced by dump1080 to RData files
#'
#' @param dates a vector of dates
#' @param source.dir path to where the flights_<yyyymmdd>.csv files are
#' @param target.dir path, where to save the RData files
#' @param verbose print some information
#' @param ... additional parameters passed on to \code{\link{airplanes}}
#' @export
#' @importFrom utils str
#' @author Joerg Steinkamp \email{joergsteinkamp@yahoo.de}
convert <- function(dates, source.dir=".", target.dir=".", verbose=TRUE, ...) {
  for (i in 1:length(dates)) {
    if (verbose)
      print(dates[i])
    fname <- paste0(source.dir, "/flights_", format(dates[i], "%Y%m%d"), ".csv")
    flights <- airplanes(fname, ...)
    if (verbose)
      print(str(flights))
    save(file = paste0(target.dir, "/flights_", format(dates[i], "%Y%m%d"), ".RData"), flights)
  }
}

#' get the flights table in the workspace
#'
#' @param dates list of dates
#' @param source.dir path to where the RData flights_<yyyymmdd>.RData files are
#' @param verbose print some information during processing
#' @return a data.frame of all flights
#' @export
#' @author Joerg Steinkamp \email{joergsteinkamp@yahoo.de}
import <- function(dates, source.dir=".", verbose=TRUE) {
  flights = NULL
  flights.full <- data.frame()
  for (i in 1:length(dates)) {
    fname <- file.path(source.dir, paste0("flights_", format(dates[i], "%Y%m%d"), ".RData"))
    load(fname)
    if (verbose)
      print(paste(dates[i], fname, nrow(flights)))
    flights.full = rbind(flights.full, flights)
  }
  return(flights.full)
}

#' @description
#' This dataset contains unsplit flight tracks of October 30th, 2016 below 3000 meters.
#' @title flights
#' @name flights
#' @docType data
#' @author Joerg Steinkamp \email{joergsteinkamp@@yahoo.de}
#' @keywords data
NULL

#' @description
#' This is a Digital elevation model for the Rhein/Main region
#' @title dem
#' @name dem
#' @docType data
#' @author Joerg Steinkamp \email{joergsteinkamp@@yahoo.de}
#' @keywords data
NULL



##################################
### END functions ################
##################################

## read raw data and save as RData
##dates <- seq(strptime("2016-10-15", format="%Y-%m-%d"), strptime("2016-11-08", format="%Y-%m-%d"), 86400)

#load("data/flights_20160806.RData")
#flights <- split.pause.callSign(flights)
#animate.flights.2d(flights)

## read the previously saved RData and append all to one large data.frame
#dates <- seq(strptime("2016-08-01", format="%Y-%m-%d"), strptime("2016-08-07", format="%Y-%m-%d"), 86400)

#flights.full <- split.pause.callSign(flights.full)
#animate.flights.3d(flights.full)

## ## http://stackoverflow.com/questions/22597663/r-from-spatialpointsdataframe-to-spatiallines
## fid <- unique(flights$callSign)
## ll <- vector("list", length(fid))
## for (i in 1:length(fid)) {
##   l <- Line(subset(flights, callSign==fid[i])[c("lon", "lat")])
##   ls <- Lines(list(l),ID=fid[i])
##   ll[[i]] = ls
## }
## lflights <- SpatialLines(ll, proj4string=CRS(proj))
## rm(l, ls, ll)


