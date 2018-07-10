create_storm_frame <- function(png_file, timestep, config, view_polygon, ...) {

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
  plot_basemap(timestep, config, view_polygon)

  files_to_map <- c(...)
  for (ind_file in files_to_map){
    map_data <- readRDS(gd_get(ind_file))
    do.call(plot, map_data)
  }

  # close off the plotting device
  dev.off()
}
