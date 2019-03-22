
prep_snow_fun <- function(snow_raster_ind, snow_data_yml, snow_cfg, timestep) {
  
  # timestep is at finer resolution than ymd_str
  one_snow_raster <- readRDS(sc_retrieve(snow_raster_ind, snow_data_yml))
  # remove NAs here for now
  x <- raster::getValues(one_snow_raster)
  x[which(x <= 0)] <- NA 
  one_snow_raster_clean <- raster::setValues(one_snow_raster, x)
  
  # clean up the environment to keep the closure small
  rm(snow_raster_ind, snow_data_yml, x, one_snow_raster)
  
  plot_fun <- function(){
    plot(one_snow_raster_clean, 
         add = TRUE,
         breaks = snow_cfg$snow_breaks,
         col = snow_cfg$snow_col,
         legend = FALSE)
  }
  return(plot_fun)
}
