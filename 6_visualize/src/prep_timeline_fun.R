prep_timeline_fun <- function(DateTime, date_lims, timeline_config, spark_config, legend_text_cfg, local_tz) {

  this_DateTime <- convert_to_local_tz(as.POSIXct(DateTime, tz = "UTC"), local_tz)

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

    # Ask the open device for the user coordinates
    coord_space <- par()$usr

    # Convert normalized plot coordinates to user coordinates
    loc_x <- coord_space[1] + c(spark_config$xleft, spark_config$xright) * diff(coord_space[1:2])
    loc_y <- coord_space[3] + spark_config$ytimeline * diff(coord_space[3:4])
    loc_date_y <- coord_space[3] + spark_config$ytimeline_text * diff(coord_space[3:4])

    # Convert this the current date & start/end to user coordinates
    loc_date_x <- dateTime_to_x(this_DateTime, loc_x)
    loc_start_x <- dateTime_to_x(date_lims[1], loc_x)
    loc_end_x <- dateTime_to_x(date_lims[2], loc_x)

    # Add timeline
    points(loc_x, rep(loc_y, length(loc_x)), type = 'l', col = timeline_config$line_col,
           lwd = timeline_config$line_lwd, lty = timeline_config$line_lty)

    # Add start/end date labels that are 2/3 the size of the moving date
    text(x=c(loc_start_x, loc_end_x), y=rep(loc_date_y, 2),
         labels=format(date_lims, "%b %d"), adj=c(0.5, 0), #pos = 1,
         family=legend_text_cfg$family, cex = timeline_config$font_cex*0.66,
         col = timeline_config$line_col)

    # Add current date tracking symbol just above line
    points(loc_date_x, loc_y,
           col = timeline_config$pt_col, bg = timeline_config$pt_col,
           cex=timeline_config$pt_cex, pch = timeline_config$pt_pch)

    # Add moving date text when it won't overlap with the start/end labels
    timeline_dist <- loc_end_x - loc_start_x
    if(loc_date_x > loc_start_x + 0.15*timeline_dist &
       loc_date_x < loc_end_x - 0.15*timeline_dist) {
      text(x=loc_date_x, y=loc_date_y, labels=format(this_DateTime, "%b %d"), adj=c(0.5, 0),
           family=legend_text_cfg$family, cex = timeline_config$font_cex, col = timeline_config$font_col)
    }

  }

}
