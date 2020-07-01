prep_storm_line_fun <- function(storm_points_sf, DateTime, storm_line_cfg){
  this_DateTime <- as.POSIXct(DateTime, tz = "UTC")
  colfunc <- colorRampPalette(c(storm_line_cfg$light_col, storm_line_cfg$dark_col))
  plot_fun <- if(is.null(storm_points_sf)) {
    function() {} # storms are optional
  } else {
    before_this_dot <- filter(storm_points_sf, DateTime <= this_DateTime)
    tail_lengths <- seq(storm_line_cfg$tail_length, storm_line_cfg$fade_i, by = -storm_line_cfg$fade_i)
    cols <- colfunc(length(tail_lengths))
    function(){
      for(i in 1:length(tail_lengths)) {
        plot(st_geometry(tail(before_this_dot, tail_lengths[i])), add=TRUE,
             col=cols[i], type = 'l', lty="dotted", lwd = 2)
      }
    }
  }
  return(plot_fun)
}
