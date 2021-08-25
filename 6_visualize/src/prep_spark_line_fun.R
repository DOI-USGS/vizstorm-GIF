# This function prepares the coordinates and shapes (polygons and line) for each
# site as much as is possible without yet knowing the final plot view
# coordinates. This function is called from both prep_spark_lines_fun and
# prep_spark_starts_fun.
prep_spark_funs_data <- function(stage_data, site_data, timestep_ind, spark_config, DateTime) {
  # Compute the full x limits for all datetimes
  storm_timesteps <- fetch_read(timestep_ind)
  date_lims <- as.POSIXct(range(storm_timesteps), tz = "UTC")

  # Choose order of sparklines
  sites <- site_data %>%
    bind_cols(., as_data_frame(sf::st_coordinates(.))) %>%
    sf::st_set_geometry(NULL) %>%
    arrange(desc(Y)) %>%
    pull(site_no)

  # Filter timeseries gage data
  this_DateTime <- as.POSIXct(DateTime, tz = "UTC") # WARNING, IF WE EVER MOVE FROM UTC elsewhere, this will be fragile/bad.
  this_spark <- filter(stage_data, dateTime >= date_lims[1], dateTime <= this_DateTime) # keep all data up until this timestep

  # Compute normalized plot coordinates for all sites
  x_coords <- c(spark_config$xleft, spark_config$xright)
  vertical_spacing <- (spark_config$ytop - spark_config$ybottom) / length(sites)
  y_coords <- data_frame(
    site_no=sites,
    lower=seq(spark_config$ytop - vertical_spacing, spark_config$ybottom, length.out=length(sites)),
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

  # Prepare a list of shapes for each site: two polygons and one line
  shapes <- list()
  for(site in sites) {

    # Filter data to just one site & create polygon out of it
    stage_data_i <- filter(this_spark, site_no == site) %>%
      arrange(dateTime) %>%
      sf::st_set_geometry(NULL)

    # Stop if there's no data at or before this timestep
    if(nrow(stage_data_i) == 0) {
      # stop(sprintf('no stage data for dateTime<=%s and site=%s', DateTime, site))
      warning(sprintf('no stage data for dateTime<=%s and site=%s', DateTime, site))
      shapes[[site]] <- list(list())
      next
    }

    # look for gaps so we can break lines/polygons into chunks
    data_chunks_meta <- rle(is.na(stage_data_i$stage_normalized)) %>% {
      data_frame(
        is_gap=.$values,
        end=cumsum(.$lengths),
        start=end - .$lengths + 1,
        before=ifelse(start-1==0, NA, start-1),
        after=ifelse(end+1>nrow(stage_data_i), NA, end+1),
        duration_hr=as.numeric(stage_data_i$dateTime[end] - stage_data_i$dateTime[start], units='hours'))
    }

    # Package each chunk of lines/polygons into a list within the shapes list
    # for this site
    shapes[[site]] <- list()
    j <- 1
    # tack an empty list to the beginning of shapes() if there's
    # a data gap at the beginning
    if(head(data_chunks_meta$is_gap, 1)) {
      shapes[[site]] <- c(shapes[[site]], list(list()))
      j <- j + 1
    }
    for(i in which(!data_chunks_meta$is_gap)) {
      # Pick out one continuous chunk of data
      data_chunk_meta <- data_chunks_meta[i,]
      data_chunk <- stage_data_i[data_chunk_meta$start : data_chunk_meta$end, ]

      # Create a hydrograph line
      hydro_line <- data_chunk %>% select(dateTime, stage_normalized)

      # Create a polygon for the full background polygon for the sparkline
      full_poly <- bind_rows(
        data_frame(dateTime = head(hydro_line$dateTime, 1), stage_normalized = 0),
        hydro_line,
        data.frame(dateTime = tail(hydro_line$dateTime, 1), stage_normalized = 0))

      # Replace values lower than flood stage with the stage & then create a polygon out of it
      flood_stage_va <- unique(na.omit(data_chunk$flood_stage_normalized))
      flood_stage_line <- hydro_line %>%
        mutate(stage_normalized = pmax(stage_normalized, flood_stage_va))
      flood_poly <- bind_rows(
        data_frame(dateTime = head(flood_stage_line$dateTime, 1), stage_normalized = flood_stage_va),
        flood_stage_line %>% select(dateTime, stage_normalized),
        data.frame(dateTime = tail(flood_stage_line$dateTime, 1), stage_normalized = flood_stage_va))

      # add to any previous chunks of those geometry datasets
      shapes[[site]][[j]] <- list(
        full_poly=full_poly,
        flood_poly=flood_poly,
        hydro_line=hydro_line)
      j <- j + 1
    }
    # tack on an empty list if there's a gap at the end
    if(tail(data_chunks_meta$is_gap, 1)) {
      shapes[[site]] <- c(shapes[[site]], list(list()))
      j <- j + 1
    }
  }

  return(list(
    x_coords=x_coords,
    y_coords=y_coords,
    shapes=shapes,
    dateTime_to_x=dateTime_to_x,
    stage_to_y=stage_to_y
  ))

}

prep_spark_line_fun <- function(stage_data, site_data, timestep_ind, spark_config, gage_col_config, DateTime, legend_text_cfg) {

  # most of the prep work happens in prep_spark_funs_data, which is shared with prep_spark_starts_fun
  spark_funs_data <- prep_spark_funs_data(stage_data, site_data, timestep_ind, spark_config, DateTime)
  rm(stage_data, site_data, timestep_ind, spark_config, DateTime) # clean up to keep closure small
  # now unpack the results
  x_coords <- spark_funs_data$x_coords
  y_coords <- spark_funs_data$y_coords

  shapes <- spark_funs_data$shapes
  dateTime_to_x <- spark_funs_data$dateTime_to_x
  stage_to_y <- spark_funs_data$stage_to_y

  # Create and return a closure/function that can be called to add sparklines
  # when making the full plot
  plot_fun <- function(){

    # closure needs x_coords, y_coords, shapes, dateTime_to_x(), stage_to_y()

    # Ask the open device for the user coordinates
    coord_space <- par()$usr

    # Plot the sparklines title
    title_x <- coord_space[2]*0.85 #+ mean(x_coords) * diff(coord_space[1:2])
    title_y <- coord_space[4] + (coord_space[3]-coord_space[4])*0.075

    text(x=title_x, y=title_y, labels="Water levels at select USGS gages", #adj=c(0.5, 0.5),
         cex=legend_text_cfg$cex*1, col=legend_text_cfg$col, family=legend_text_cfg$family, font = 2)

    for(site in names(shapes)) {

      # Convert normalized plot coordinates to user coordinates
      x_user <- coord_space[1] + x_coords * diff(coord_space[1:2])
      y_coords_site <- y_coords %>% filter(site_no==site) %>% {c(.$lower, .$upper)}
      y_user <- coord_space[3] + y_coords_site * diff(coord_space[3:4])

      num_chunks <- length(shapes[[site]])
      for(i in seq_len(num_chunks)) {
        chunk <- shapes[[site]][[i]]

        # Skip if there's no data at or before this timestep
        if(length(chunk) == 0) { next }

        # Convert this site's shapes to user coordinates
        full_poly <- chunk$full_poly %>% mutate(
          x = dateTime_to_x(dateTime, x_user),
          y = stage_to_y(stage_normalized, y_user))
        flood_poly <- chunk$flood_poly %>% mutate(
          x = dateTime_to_x(dateTime, x_user),
          y = stage_to_y(stage_normalized, y_user))
        hydro_line <- chunk$hydro_line %>% mutate(
          x = dateTime_to_x(dateTime, x_user),
          y = stage_to_y(stage_normalized, y_user))

        # Add stage shapes to plot
        #text(full_poly$x[1]-strwidth("5555"), full_poly$y[1], labels=site)
        polygon(full_poly$x, full_poly$y, col = gage_col_config$gage_norm_col, border=NA)
        polygon(flood_poly$x, flood_poly$y, col = gage_col_config$gage_flood_col, border=NA)
        points(hydro_line$x, hydro_line$y, col = gage_col_config$gage_line_col, type='l', lwd=2)

        # add the x and/or o
        if(num_chunks > 1) {
          if(i < num_chunks) {
            points(tail(hydro_line$x,1), tail(hydro_line$y,1), col=gage_col_config$gage_norm_col, pch=4, cex=1.2, lwd=5) #make it stand out more, adding "outline" behind it.
            points(tail(hydro_line$x,1), tail(hydro_line$y,1), col=gage_col_config$gage_out_col, pch=4, cex=1.2, lwd=4)
          }
          if(i > 1) {
            points(head(hydro_line$x,1), head(hydro_line$y,1), col=gage_col_config$gage_norm_col, pch=19, cex=1, lwd=5) #make it stand out more
            points(head(hydro_line$x,1), head(hydro_line$y,1), col=gage_col_config$gage_out_col, pch=19, cex=1, lwd=4)
          }
        }
      }
    }
  }

  return(plot_fun)
}

prep_spark_starts_fun <- function(stage_data, site_data, timestep_ind, spark_config, DateTime) {

  # most of the prep work happens in prep_spark_funs_data, which is shared with prep_spark_line_fun
  spark_funs_data <- prep_spark_funs_data(stage_data, site_data, timestep_ind, spark_config, DateTime)
  rm(stage_data, site_data, timestep_ind, spark_config, DateTime) # clean up to keep closure small
  # now unpack the results
  x_coords <- spark_funs_data$x_coords
  y_coords <- spark_funs_data$y_coords
  shapes <- lapply(spark_funs_data$shapes, `[[`, 1) # only need the first data chunk from each shape
  dateTime_to_x <- spark_funs_data$dateTime_to_x
  stage_to_y <- spark_funs_data$stage_to_y

  extract_spark_starts_fun <- function() {
    # closure needs x_coords, y_coords, shapes, dateTime_to_x(), stage_to_y()

    # Ask the open device for the user coordinates
    coord_space <- par()$usr

    spark_starts <- bind_rows(lapply(names(shapes), function(site) {
      # Skip if there's no data for this site
      if(is.null(shapes[[site]])) { next }

      # Convert normalized plot coordinates to user coordinates
      x_user <- coord_space[1] + x_coords * diff(coord_space[1:2])
      y_coords_site <- y_coords %>% filter(site_no==site) %>% {c(.$lower, .$upper)}
      y_user <- coord_space[3] + y_coords_site * diff(coord_space[3:4])

      # Convert this site's shapes to user coordinates
      hydro_start <- shapes[[site]]$hydro_line[1,] %>% mutate(
        site_no = site,
        x = dateTime_to_x(dateTime, x_user),
        y = stage_to_y(stage_normalized, y_user))

      hydro_start
    }))

    return(spark_starts)
  }

}
