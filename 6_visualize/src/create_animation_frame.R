create_animation_frame <- function(png_file, config, ...) {

  plot_type <- switch(Sys.info()[['sysname']],
                      Windows= "cairo",
                      Linux  = "Xlib",
                      Darwin = "quartz")
  # open the plotting device. this should eventually be a gif function, but i don't know how that looks yet
  png(filename=png_file, width=config$width, height=config$height, units='px', type = plot_type)
  par(family = 'abel')
  showtext_begin()
  # plot the pieces in order, passing through data files or R objects from the
  # scipiper pipeline
  plot_funs <- c(...)

  for (plot_fun in plot_funs){
    plot_fun()
  }
  showtext_end()
  # close off the plotting device
  dev.off()
}
