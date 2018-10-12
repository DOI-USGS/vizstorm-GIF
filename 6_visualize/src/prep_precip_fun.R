
prep_precip_fun <- function(precip_rasters, precip_bins, timestep){

  breaks <- c(precip_bins$left_break[1], precip_bins$right_break)
  colors <- precip_bins$col

  if(!(timestep %in% names(precip_rasters))) {
    stop(sprintf("precip_raster for timestep %s is unavailable", timestep))
  } # missing timesteps can give a cryptic error later, so catch it now
  one_precip_raster <- precip_rasters[[timestep]]

  # clean up the environment to keep the closure small
  rm(precip_rasters, precip_bins, timestep)

  plot_fun <- function(){
    plot(one_precip_raster, add = TRUE, breaks = breaks, col = colors, legend = FALSE)
  }
  return(plot_fun)
}
