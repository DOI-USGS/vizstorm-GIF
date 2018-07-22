# animate gage points so they move vertically, round a corner, and then move
# horizontally until they stop at their respective sparkline start locations
prep_gage2spark_fun <- function(intro_config, timestep, storm_data, site_data, gage_color_config, timestep_ind, spark_config, DateTime) {

  # identify locations and styling for the sites
  this_DateTime <- as.POSIXct(DateTime, tz = "UTC") # WARNING, IF WE EVER MOVE FROM UTC elsewhere, this will be fragile/bad.
  sites <- filter(storm_data, dateTime == this_DateTime) %>%
    mutate(
      is_flooding = stage_normalized >= flood_stage_normalized,
      background=gage_color_config$gage_norm_col,
      color=ifelse(is_flooding, gage_color_config$gage_flood_col, NA)) %>%
      {bind_cols(., as_data_frame(sf::st_coordinates(.)))} %>%
    select(site_no, X, Y, background, color)
  n_sites <- nrow(sites)

  # get a closure that will provide us with sparkline start locations when we
  # run it during create_animation_frame
  spark_starts_fun <- prep_spark_starts_fun(storm_data, site_data, timestep_ind, spark_config, DateTime)

  # prepare other animation information
  r <- intro_config$elbow_radius # radius of the curvature at the elbow of the dot travel path, in 10000ths of degrees
  t_start <- seq(1, intro_config$t_last_start, length.out=n_sites) # timestep at which each site dot first leaves its lat/lon location, vector of 1 value per site
  t_end <- seq(intro_config$n_frames-intro_config$t_last_start+1, intro_config$n_frames, length.out=n_sites) # timestep at which the site dot arrives at its sparkline location, vector of 1 value per site

  # make the function. we need to do a lot of the calculations within the
  # function because we don't know where the sparklines start until we're in the
  # function
  plot_fun <- function(){
    # identify locations for the starts of the spark lines
    spark_starts <- spark_starts_fun()
    x2 <- spark_starts$x # beginning of spark line
    y2 <- spark_starts$y

    # order gage locations to match order of sparklines, then create simpler
    # variable names for calculating transition locations
    sites_ordered <- spark_starts %>%
      select(site_no) %>%
      left_join(sites, by='site_no')
    x1 <- sites_ordered$X
    y1 <- sites_ordered$Y

    # calculate additional variables. naming convention for d and t: lowercase =
    # position in space/time, uppercase = length/duration
    r_revised <- pmin(r, abs(x2 - x1), abs(y2 - y1))
    r_rise <- r_revised * sign(y2 - y1)
    r_run <- r_revised * sign(x2 - x1)
    T_move <- t_end - t_start # length of the move in number of timesteps. probably the same for all sites
    D_rise <- y2 - r_rise - y1 # length of the purely vertical segment of the travel path
    D_round <- pi*r/2 # length of the segment of the travel path that rounds the corner from vertical to horizontal
    D_run <- x2 - r_run - x1 # length of the purely horizontal segment of the travel path
    D <- abs(D_rise) + abs(D_round) + abs(D_run) # length of the full travel path

    # define phases by their first timestep (different for each site=row)
    phases <- data_frame(
      prelude = rep(-Inf, length(x1)),
      rise = t_start,
      round = t_start + T_move * abs(D_rise)/D,
      run = t_start + T_move * (abs(D_rise)+abs(D_round))/D,
      epilog = t_end,
      end = Inf) %>%
      as.matrix()

    # figure out which phase each site is in
    phase <- sapply(seq_len(n_sites), function(site) {
      breaks_all <- phases[site,]
      empty_bins <- breaks_all[diff(breaks_all)==0]
      breaks_nonempty <- breaks_all[setdiff(names(breaks_all), names(empty_bins))]
      cut(timestep, breaks=breaks_nonempty, labels=setdiff(names(breaks_nonempty), 'end'), right=FALSE) %>%
        as.character()
    }) %>% ordered(levels=colnames(phases))

    # calculate phase_pos, the fraction of the way each site is through its current phase
    phase_start <- phases[cbind(seq_len(n_sites), phase)]
    phase_end <- phases[cbind(seq_len(n_sites), as.numeric(phase)+1)]
    phase_pos <- (timestep - phase_start)/(phase_end - phase_start)
    theta <- phase_pos*pi/2 # angle as fraction of 90 degrees (pi/2 radians); calculated for all, applies only to round phase

    # calculate coordinates for this timestep for each site, first for all 5
    # possible phases for every site (too_many_coords), and then subsetting to the
    # phase we're actually in for each site (coords)
    too_many_coords <- list(
      prelude = data_frame(x=x1, y=y1),
      rise = data_frame(x=x1, y=y1 + phase_pos*D_rise),
      round = data_frame(x=x1 + r_run*(1-cos(theta)), y=y2 - r_rise*(1 - sin(theta))),
      run = data_frame(x=x1 + r_run + phase_pos*D_run, y=y2),
      epilog = data_frame(x=x2, y=y2))
    coords <- bind_rows(lapply(seq_len(n_sites), function(site) {
      too_many_coords[[as.character(phase[site])]][site,]
    }))

    # add points to the plot
    points(
      x = coords$x, y = coords$y, pch = 21,
      bg = intro_config$point_background, col = sites_ordered$color)
  }
  return(plot_fun)
}
