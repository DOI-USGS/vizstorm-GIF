
prep_snow_fun <- function(snow_raster_ind, snow_data_yml, snow_cfg, timestep) {
  
  # timestep is at finer resolution than ymd_str
  # snow_palette <- colorRampPalette(c("#FAFBF399", "#F0F8E3CC", "#D4E9CAE6", "#BBE0CE", "#B7DAD0", "#B0CCD7", "#A9B8D7"))
  snow_palette <- c(zero = "#d5dfc6", slush = "#a5a59c", snow = "#D4E9CAE6")
  one_snow_raster <- readRDS(sc_retrieve(snow_raster_ind, snow_data_yml))
  
  # change categories for colors
  one_snow_raster_cat <- raster::cut(one_snow_raster, breaks=c(-1, 0, 50, 100))
  
  # clean up the environment to keep the closure small
  rm(snow_raster_ind, snow_data_yml, one_snow_raster)
  
  plot_fun <- function(){
    plot(one_snow_raster_cat, 
         add = TRUE,
         col = snow_palette,
         legend = FALSE)
  }
  return(plot_fun)
}
