interpolate_storm_points <- function(ind_file, timesteps_ind, coarse_points_ind) {
  # -- create a SpatialPointsDataFrame with a point for each timestep --

  # get the vector of timepoints we want to include
  times <- as.POSIXct(strptime(depends[['timesteps']]$times, format='%b %d %I:%M %p', tz="America/Puerto_Rico"))

  # interpolate latitudes and longitudes to the timestamps we actually want
  coords_interp <- data_frame(
    lat_interp = approx(points_simpler$DateTime, points_simpler$LAT, xout = times)$y,
    lon_interp = approx(points_simpler$DateTime, points_simpler$LON, xout = times)$y) %>%
    filter(., complete.cases(.))
  pts <- cbind(lon_interp[!is.na(lon_interp)], lat_interp[!is.na(lon_interp)])
  location <- SpatialPoints(pts, proj4string=CRS("+proj=longlat +datum=WGS84"))

  # construct a SpatialPointsDataFrame with the above locations. No timestamps,
  # just locations that precisely correspond to the times from the `timesteps`
  # item
  data.out <- data.frame(
    id = paste0('storm-', seq_len(length(location))),
    class = "storm-dot",
    r = "12",
    stringsAsFactors = FALSE)
  row.names(data.out) <- row.names(location)
  sp.data.frame <- as(object = location, Class = paste0(class(location), "DataFrame"))
  sp.data.frame@data <- data.out
  row.names(sp.data.frame) <- row.names(data.out) # i think this line is unnecessary but harmless


}
