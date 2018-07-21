
prep_spark_line_fun <- function(storm_data, dates_config, spark_config, gage_col_config, DateTime){
  sites <- unique(storm_data$site_no)
  this_DateTime <- as.POSIXct(DateTime, tz = "UTC") # WARNING, IF WE EVER MOVE FROM UTC elsewhere, this will be fragile/bad.
  this_spark <- filter(storm_data, dateTime <= this_DateTime) # keep all data up until this timestep

  # Compute the full x limits for all datetimes
  date_lims <- as.POSIXct(c(dates_config$start, dates_config$end), tz = "UTC")

  # Compute normalized plot coordinates for all sites
  x_coords <- c(spark_config$xleft, spark_config$xright)
  vertical_spacing <- (spark_config$ytop - spark_config$ybottom) / length(shapes)
  y_coords <- data_frame(
    site_no=sites,
    lower=seq(spark_config$ybottom, spark_config$ytop - vertical_spacing, length.out=length(sites)),
    upper=lower + vertical_spacing)

  # Define functions that will convert each site's shapes to user coordinates
  dateTime_to_x <- function(dateTime, x_user) {
    date_lims_num <- as.numeric(date_lims, units='days')
    date_time_num <- as.numeric(dateTime, units='days')
    x_frac <- (date_time_num - date_lims_num[1]) / diff(date_lims_num)
    x_user[1] + x_frac*diff(x_user)
  }
  stage_to_y <- function(stage_normalized, y_user) {
    y_frac <- stage_normalized # stage_normalized is already a fraction between 0 and 1
    y_user[1] + y_frac*diff(y_user)
  }

  shapes <- list()
  for(site in sites) {

    # Filter data to just one site & create polygon out of it
    storm_data_i <- filter(this_spark, site_no == site) %>%
      arrange(dateTime) %>%
      sf::st_set_geometry(NULL)

    # Skip if there's no data at or before this timestep
    if(nrow(storm_data_i) == 0) {
      shapes[[site]] <- NULL
      next
    }

    # Create a hydrograph line
    hydro_line <- storm_data_i %>% select(dateTime, stage_normalized)

    # Create a polygon for the full background polygon for the sparkline
    full_poly <- bind_rows(
      data_frame(dateTime = head(hydro_line$dateTime, 1), stage_normalized = 0),
      hydro_line,
      data.frame(dateTime = tail(hydro_line$dateTime, 1), stage_normalized = 0))

    # Replace values lower than flood stage with the stage & then create a polygon out of it
    flood_stage_va <- unique(storm_data_i$flood_stage_normalized)
    flood_stage_line <- hydro_line %>%
      mutate(stage_normalized = pmax(stage_normalized, flood_stage_va))
    flood_poly <- bind_rows(
      data_frame(dateTime = head(flood_stage_line$dateTime, 1), stage_normalized = flood_stage_va),
      flood_stage_line %>% select(dateTime, stage_normalized),
      data.frame(dateTime = tail(flood_stage_line$dateTime, 1), stage_normalized = flood_stage_va))

    # Package the line and polygons into a list within the shapes list
    shapes[[site]] <- list(
      full_poly=full_poly,
      flood_poly=flood_poly,
      hydro_line=hydro_line
    )
  }

  plot_fun <- function(){

    # needs x_coords, y_coords, shapes

    # Ask the open device for the user coordinates
    coord_space <- par()$usr

   for(site in names(shapes)) {
      # Skip if there's no data at or before this timestep
      if(is.null(shapes[[site]])) { next }

      # Convert normalized plot coordinates to user coordinates
      x_user <- coord_space[1] + x_coords * diff(coord_space[1:2])
      y_coords_site <- y_coords %>% filter(site_no==site) %>% {c(.$lower, .$upper)}
      y_user <- coord_space[3] + y_coords_site * diff(coord_space[3:4])

      # Convert this site's shapes to user coordinates
      full_poly <- shapes[[site]]$full_poly %>% mutate(
        x = dateTime_to_x(dateTime, x_user),
        y = stage_to_y(stage_normalized, y_user))
      flood_poly <- shapes[[site]]$flood_poly %>% mutate(
        x = dateTime_to_x(dateTime, x_user),
        y = stage_to_y(stage_normalized, y_user))
      hydro_line <- shapes[[site]]$hydro_line %>% mutate(
        x = dateTime_to_x(dateTime, x_user),
        y = stage_to_y(stage_normalized, y_user))

      # Add stage shapes to plot
      polygon(full_poly$x, full_poly$y, col = gage_col_config$gage_norm_col, border=NA)
      polygon(flood_poly$x, flood_poly$y, col = gage_col_config$gage_flood_col, border=NA)
      points(hydro_line$x, hydro_line$y, col = gage_col_config$gage_flood_col, type='l', lwd=2.5)
    }
  }

  return(plot_fun)
}