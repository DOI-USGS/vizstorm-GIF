prep_storm_line_fun <- function(storm_points_sf, DateTime, storm_line_cfg){
  this_DateTime <- as.POSIXct(DateTime, tz = "UTC")
  colfunc <- colorRampPalette(c(storm_line_cfg$light_col, storm_line_cfg$dark_col))
  plot_fun <- if(is.null(storm_points_sf)) {
    function() {} # storms are optional
  } else {
    before_this_dot <- filter(storm_points_sf, DateTime <= this_DateTime)
    tail_lengths <- seq(storm_line_cfg$tail_length, storm_line_cfg$fade_i, by = -storm_line_cfg$fade_i)
    cols <- colfunc(length(tail_lengths))

    # Keep a faint line to show where the hurricane has been
    tail_lengths <- c(nrow(before_this_dot), tail_lengths)
    cols <- c("grey80", cols)

    function(){
      for(i in 1:length(tail_lengths)) {
        # Plot each fading section individually
        # For the last one, use the last row in the data
        n_start <- nrow(before_this_dot) - tail_lengths[i]
        n_end <- nrow(before_this_dot) - ifelse(i==length(tail_lengths), 0, tail_lengths[i+1])

        # For the initial ones where tail_lengths may be > nrow
        if(n_start < 1) { n_start <- 1 }
        if(n_end < 1) { n_end <- nrow(before_this_dot) }

        plot(st_geometry(slice(before_this_dot, n_start:n_end)), add=TRUE,
             col=cols[i], type = 'l', lty="dotted", lwd = 2)
      }
    }
  }
  return(plot_fun)
}

# source("6_visualize/src/visualize_utils.R")
# storm_points_sf <- fetch_read('2_process/out/storm_points_interp.rds.ind')
# this_DateTime <- as.POSIXct("2018-09-14", tz = "UTC")
# before_this_dot <- filter(storm_points_sf, DateTime <= this_DateTime)
# colfunc <- colorRampPalette(c("gray90", "gray20"))
# tail_lengths <- seq(25, 5, -5)
# cols <- colfunc(length(tail_lengths))
#
# # We want a faint line to stay to show where it was the whole time
# tail_lengths <- c(nrow(before_this_dot), tail_lengths)
# cols <- c("gray90", cols)
#
# # Don't overlap the lines, so slice the data frame
# plot(st_geometry(storm_points_sf), type = 'l', col="white")
#
# for(i in 1:length(tail_lengths)) {
#   # Plot each fading section individually
#   n_start <- nrow(before_this_dot) - tail_lengths[i]
#   n_end <- nrow(before_this_dot) - tail_lengths[i+1]
#
#   # For the last one, use the last row in the data
#   if(i == length(tail_lengths)) n_end <- nrow(before_this_dot)
#
#   plot(st_geometry(slice(before_this_dot, n_start:n_end)), add=TRUE,
#        col=cols[i], type = 'l', lty="dotted", lwd = 2)
# }

