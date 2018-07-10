create_storm_frame <- function(png_file, timestep, config, view_polygon, ...) {

  # open the plotting device. this should eventually be a gif function, but i don't know how that looks yet
  png(filename=png_file, width=config$width, height=config$height, units='px')

  # plot the pieces in order, passing through data files or R objects from the
  # scipiper pipeline
  plot_function_inds <- c(...)
  for (plot_fun_ind in plot_function_inds){
    plot_fun <- readRDS(sc_retrieve(plot_fun_ind))
    plot_fun()
  }

  # close off the plotting device
  dev.off()
}
