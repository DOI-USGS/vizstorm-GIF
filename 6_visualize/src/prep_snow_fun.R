
prep_snow_fun <- function(snow_raster_ind, snow_data_yml, snow_cfg, timestep) {
  
  # timestep is at finer resolution than ymd_str
  snow_raster <- raster::raster(sc_retrieve(snow_raster_ind, snow_data_yml))
  
  browser()
  snow_raster_timestep <- raster::subset(snow_raster, timestep)
  
  # remove NAs here for now
  x <- raster::getValues(snow_raster_timestep)
  x[which(x <= 0)] <- NA 
  snow_raster_timestep_clean <- raster::setValues(snow_raster_timestep, x)

  # clean up the environment to keep the closure small
  rm(snow_raster_ind, snow_data_yml, snow_raster, x, snow_raster_timestep)
  
  plot_fun <- function(){
    plot(snow_raster_timestep_clean, 
         add = TRUE,
         breaks = snow_cfg$snow_breaks,
         col = snow_cfg$snow_col,
         legend = FALSE)
  }
  return(plot_fun)
}
