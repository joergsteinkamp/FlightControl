#' read the air traffic csv file
#'
#' reads a csv file recorded by \href{https://github.com/antirez/dump1090}{dump1090} and returns it as data.frame
#'
#' @param file path to and name of the csv file
#' @param raw logical if TRUE returns the full data set otherwise extract only position/altitude/time with callSign (flightID)
#' @param min.altitude ignore positions below that altitude in the given unit
#' @param max.altitude ignore positions above that altitude in the given unit
#' @param unit default m(eter). If anything else than f(eet) is given the default is used
#' @return data.frame
#' @author Joerg Steinkamp \email{joergsteinkamp@@yahoo.de}
#' @references \href{https://github.com/antirez/dump1090}{dump1090}
#' @export
#' @importFrom utils read.table
airplanes <- function(file, raw=FALSE, min.altitude=-999, max.altitude=999999, unit="m") {
  message=type=altitude=hexIdent=callSign=NULL
  if (grepl("gz$", file)) {
    flight.raw <- read.table(gzfile(file), sep=",", stringsAsFactors=FALSE)
  } else {
    flight.raw <- read.table(file, sep=",", stringsAsFactors=FALSE)
  }
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

  ## filter out wrong reported callSigns
  flight.ids <- subset(flight.ids, callSign != "????????")

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

  ## remove unneeded columns
  flight.pos$gen.date <- NULL
  flight.pos$gen.time <- NULL
  flight.pos$log.date <- NULL
  flight.pos$log.time <- NULL

  ## first occurence of hexIdent/callSign combination (MSG type 1)
  first.ids <- flight.ids
  first.ids$hexCallID = paste0(first.ids$hexIdent, first.ids$callSign)
  first.ids <- first.ids[match(unique(first.ids$hexCallID), first.ids$hexCallID), ]

  ## relate the callSign (flight number) to the hexIdent in the flight.pos data.frame.
  ## apply with 20 minutes (1800s) threshold (if positions were sent in advance of first ID)
  ## and also if the airplane returns
  fpos.tmp <- apply(first.ids, 1, function(x) {
    ## makes it much faster, when subset is  split in two steps
    fpos <- subset(flight.pos, hexIdent == x['hexIdent'])
    fpos <- subset(fpos, date > as.POSIXlt(x['date']) - 1800)
    fpos <- subset(fpos, date < as.POSIXlt(x['date']) + 1800)

    if (nrow(fpos) == 0)
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
  ## Use a threshold of 30 minutes here to split it
  for (id in unique(fpos$callSign)) {
    if (any(diff(fpos$date[fpos$callSign == id]) > 1800)) {
      flights.tmp <- subset(fpos, callSign == id)
      newCallSign <- flights.tmp$callSign
      tid <- which(diff(flights.tmp$date) > 1800)
      for (i in 1:length(tid)) {
        if (i != 1 && i != length(tid)) {
          newCallSign[(tid[i-1]+1):tid[i]] = paste0(id, letters[i+1])
        } else {
          if (i == 1) {
            newCallSign[1:tid[i]] = paste0(id, letters[i])
          }
          if (i == length(tid)) {
            newCallSign[(tid[i]+1):nrow(flights.tmp)] = paste0(id, letters[i+1])
          }
        }
      }
      fpos$callSign[fpos$callSign==id] = newCallSign
    }
  }
  flight.pos <- rbind(fpos, fpos.tmp)
  rownames(flight.pos) <- 1:nrow(flight.pos)
  return(flight.pos[, c(5,6,3,4,2)])
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
    fname <- file.path(source.dir, paste0("flights_", format(dates[i], "%Y%m%d"), ".csv"))
    if (!file.exists(fname)) {
      if (file.exists(paste0(fname, ".gz"))) {
        fname = paste0(fname, ".gz")
      } else {
        stop(paste0("File '", fname, "' not found!"))
      }
    }
    flights <- airplanes(fname, ...)
    if (verbose)
      print(str(flights))
    save(file=paste0(target.dir, "/flights_", format(dates[i], "%Y%m%d"), ".RData"), flights, compress="xz")
  }
  return(TRUE)
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
