create_animation_frame <- function(png_file, config, ...) {

  plot_type <- switch(Sys.info()[['sysname']],
                      Windows= "cairo",
                      Linux  = "Xlib",
                      Darwin = "quartz")
  # open the plotting device. this should eventually be a gif function, but i don't know how that looks yet
  png(filename=png_file, width=config$width, height=config$height, units='px', type = plot_type)

  # begin using google fonts
  par(family = 'abel') # may need to install from Google Fonts
  showtext_begin()

  # plot the pieces in order, passing through data files or R objects from the
  # scipiper pipeline
  plot_funs <- c(...)

  for (plot_fun in plot_funs){
    plot_fun()
  }

  # close off google fonts
  showtext_end()

  # close off the plotting device
  dev.off()
}
