
prep_storm_point_fun <- function(storm_points_sf, DateTime, hurricane_cols){
  this_DateTime <- as.POSIXct(DateTime, tz = "UTC") # WARNING, IF WE EVER MOVE FROM UTC elsewhere, this will be fragile/bad.
  plot_fun <- if(is.null(storm_points_sf)) {
    function() {} # storms are optional
  } else {
    this_dot <- filter(storm_points_sf, DateTime == this_DateTime)
    hurricane_col <- hurricane_cols[(this_dot$SS + 1)]
    plot_fun <- function(){
      plot(sf::st_geometry(this_dot), add = TRUE, col = hurricane_col, pch = 20, cex = 4) # should style args be in a config?
    }
  }
  return(plot_fun)
}
