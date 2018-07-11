
prep_storm_point_fun <- function(ind_file, storm_points_ind_file){
  storm_points_sf <- readRDS(sc_retrieve(storm_points_ind_file))
  # parse date from ind_file
  this_DateTime <- POSIX_from_filename(ind_file)
  this_dot <- filter(storm_points_sf, DateTime == this_DateTime)
  plot_fun <- function(){
    plot(this_dot, add = TRUE, col = 'red', pch = 20, cex = 2) # should style args be in a config?
  }
  write_put_fun(plot_fun, ind_file)
}
