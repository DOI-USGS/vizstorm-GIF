
prep_snow_fun <- function(snow_raster_ind, snow_data_yml, snow_cfg, timestep, timesteps_all_ind, frame_step) {
  
  # timestep is at finer resolution than ymd_str
  timesteps_all <- readRDS(sc_retrieve(timesteps_all_ind))
  timesteps_corrected <- timesteps_all[seq(1, by = frame_step, to = length(timesteps_all))] 
  
  band_i <- which(timesteps_corrected == as.POSIXct(timestep, tz="UTC"))
  snow_raster_file_info <- readRDS(sc_retrieve(snow_raster_ind, snow_data_yml))
  
  # Raster layer index is different from band_i because we split into multiple files
  raster_fn <- snow_raster_file_info$file[which(snow_raster_file_info$band == band_i)]
  raster_layer <- snow_raster_file_info$raster_count[which(snow_raster_file_info$band == band_i)]
  snow_raster <- raster::raster(raster_fn, band=raster_layer)
  
  # clean up the environment to keep the closure small
  rm(snow_raster_ind, snow_data_yml, timestep, timesteps_all_ind, 
     frame_step, timesteps_all, timesteps_corrected, band_i)
  
  plot_fun <- function(){
    plot(snow_raster, 
         add = TRUE,
         breaks = snow_cfg$snow_breaks,
         col = snow_cfg$snow_col,
         legend = FALSE)
  }
  return(plot_fun)
}
