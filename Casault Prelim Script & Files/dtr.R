dtr <- function(lon, lat, ref_long, ref_lat) {
  
  # Convert lat/lon to easting/northing (rectangular) coordinates
  # based on the dtr.c code written by A.H. Sandstrom (Mar 3 1995)
  
  DEG_IN_RAD <- 57.295779
  FACTOR <- 0.0067686275
  X_FACT <- 0.032339
  Y_FACT <- 0.032559
  
  radlat <- (lat + ref_lat)/(2.0 * DEG_IN_RAD)
  radlon <- (lon - ref_long)/(2.0 * DEG_IN_RAD)
  tmp1 <- sin(radlat)
  tmp1 <- tmp1^2
  tmp2 <- 1.0 - FACTOR * tmp1
  a <- sqrt(tmp2)
  b <- tmp2^1.5
  
  x <- ((lon - ref_long)/(X_FACT * a)) * cos(radlat) * 3.6
  y <- ((lat - ref_lat)/(Y_FACT * b)) * cos(radlon) * 3.6
  
  return(list(x=x,y=y))
}
