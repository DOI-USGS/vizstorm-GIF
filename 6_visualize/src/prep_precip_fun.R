
prep_precip_fun <- function(precip_rasters, precip_bins, timestep){

  breaks <- unique(c(precip_bins$left_break,
                     precip_bins$right_break))
  breaks[which(breaks == Inf)] <- 100000 # Just quiets an error
  breaks <- c(0, 1, breaks[2:length(breaks)]) # Add a tiny break for alpha

  colors <- as.character(precip_bins$col)
  colors <- c(paste0(substr(colors[1], 1, 7), "00"), colors)

  one_precip_raster <- precip_rasters[[timestep]]

  # clean up the environment to keep the closure small
  rm(precip_rasters, precip_bins, timestep)

  plot_fun <- function(){
    par(mai = c(0,0,0,0))
    plot(one_precip_raster, add = TRUE, breaks = breaks, col = colors, legend = FALSE)
  }
  return(plot_fun)
}
