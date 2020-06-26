prep_timeline_fun <- function(DateTime, timestep_ind, timeline_config, spark_config, legend_text_cfg) {

  this_DateTime <- as.POSIXct(DateTime, tz = "UTC") # WARNING, IF WE EVER MOVE FROM UTC elsewhere, this will be fragile/bad.

  # Compute the full x limits for all datetimes
  storm_timesteps <- fetch_read(timestep_ind)
  date_lims <- as.POSIXct(range(storm_timesteps), tz = "UTC")

  # Compute normalized plot coordinates for all sites
  x_coords <- c(spark_config$xleft, spark_config$xright)
  y_coords <- spark_config$ytimeline

  # Define functions that will convert each site's shapes to user coordinates
  dateTime_to_x <- function(dateTime, x_user) {
    date_lims_num <- as.numeric(date_lims, units='days')
    date_time_num <- as.numeric(dateTime, units='days')
    x_frac <- (date_time_num - date_lims_num[1]) / diff(date_lims_num)
    x_user[1] + x_frac*diff(x_user)
  }

  # Create and return a closure/function that can be called to add sparklines
  # when making the full plot
  plot_fun <- function(){

    # closure needs x_coords, y_coords, shapes, dateTime_to_x(), stage_to_y()

    # Ask the open device for the user coordinates
    coord_space <- par()$usr

    # Convert normalized plot coordinates to user coordinates
    loc_x <- coord_space[1] + x_coords * diff(coord_space[1:2])
    loc_y <- coord_space[3] + y_coords * diff(coord_space[3:4])
    loc_date_y <- loc_y - loc_y*0.05

    # Convert this the current date to user coordinates
    loc_date_x <- dateTime_to_x(this_DateTime, loc_x)

    # Add timeline and dot to plot
    points(loc_x, rep(loc_y, length(x_coords)), type = 'l', col = timeline_config$line_col)
    points(loc_date_x, loc_y, col = timeline_config$pt_col, cex=timeline_config$pt_cex, pch = timeline_config$pt_pch)
    text(x=loc_date_x, y=loc_date_y, labels=format(this_DateTime, "%B %d"), adj=c(0.5, 0),
         family=legend_text_cfg$family, cex = timeline_config$line_cex)

  }

}
