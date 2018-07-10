create_storm_frame <- function(
  png_file, timestep, config,
  view_polygon, focus_geoms, secondary_geoms,
  storm_line_ind='2_process/out/storm_line.rds.ind', storm_points_ind='2_process/out/storm_points_interp.rds.ind',
  precip_rasters_ind='2_process/out/precip_rasters.rds.ind',
  stream_data_ind='1_fetch/out/streamdata.rds.ind'
) {

  # open the plotting device. this should eventually be a gif function, but i don't know how that looks yet
  png(filename=png_file, width=config$width, height=config$height, units='px')

  # do some computations here to add info to config - whatever we can automate,
  # or perhaps some combination between computing defaults here and allowing the
  # viz_config.yml to override defaults here
  config$sparklines <- list(
    box_width=config$width*0.28,
    box_height=config$height*0.95,
    box_x=config$width*0.7)
  config$legend <- list(
    x=20,
    y=config$height-20)

  # plot the pieces in order, passing through data files or R objects from the
  # scipiper pipeline
  plot_basemap(timestep, config, view_polygon, focus_geoms, secondary_geoms)
  add2plot_storm(timestep, config, storm_line_ind, storm_points_ind)
  add2plot_precip(timestep, config, precip_rasters_ind)
  add2plot_sparklines(timestep, config, stream_data_ind)
  add2plot_datetime(timestep, config)
  add2plot_legend(timestep, config)
  add2plot_USGS_logo(timestep, config)

  # close off the plotting device
  dev.off()
}
