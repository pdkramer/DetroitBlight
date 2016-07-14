#Parse latitude and longitude from locations in the form "8430 ARTESIAN DETROIT, MI (42.354592, -83.226085)"

Get_Lat_Lon <- function(loc) {
  m <- regexec("[^\\(]*\\(([^,]+), *([^\\)]+)",loc)
  latlong <- do.call(rbind, lapply(regmatches(loc,m), `[`, c(2L, 3L)))
  colnames(latlong) <- c("lat","lon")
  latlong
}

#Test data
#inputlatlon <- Get_Lat_Lon(c("8430 ARTESIAN DETROIT, MI (42.354592, -83.226085)","","3960 GARLAND DETROIT, MI (42.378203, -82.989918)"))
