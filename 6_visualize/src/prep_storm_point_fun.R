
prep_storm_point_fun <- function(storm_points_ind_file, DateTime){
  storm_points_sf <- readRDS(sc_retrieve(storm_points_ind_file))
  this_DateTime <- as.POSIXct(DateTime, tz = "UTC") # WARNING, IF WE EVER MOVE FROM UTC elsewhere, this will be fragile/bad.
  this_dot <- filter(storm_points_sf, DateTime == this_DateTime)
  plot_fun <- function(){
    plot(this_dot, add = TRUE, col = 'red', pch = 20, cex = 2) # should style args be in a config?
  }
  return(plot_fun)
}
