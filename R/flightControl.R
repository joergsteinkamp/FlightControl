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
#' @importFrom viridis inferno
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

  ## calculate a raster surface ("carpet") of the lowest flight altitude.
  if (!tracks)
    min.altitude <- min.flight.altitude(flights, extent=extent, res=1000, proj=proj, method="raw")

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
    alt.alpha <- sqrt(max(alt.z) - alt.z) / sqrt(max(alt.z) - min(alt.z)) / 2

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

