
prep_snow_fun <- function(snow_raster_ind, snow_data_yml, snow_cfg, timestep) {
  
  # timestep is at finer resolution than ymd_str
  snow_palette <- colorRampPalette(c("#FAFBF399", "#F0F8E3CC", "#D4E9CAE6", "#BBE0CE", "#B7DAD0", "#B0CCD7", "#A9B8D7"))
  one_snow_raster <- readRDS(sc_retrieve(snow_raster_ind, snow_data_yml))
  
  # clean up the environment to keep the closure small
  rm(snow_raster_ind, snow_data_yml)
  
  plot_fun <- function(){
    plot(one_snow_raster, 
         add = TRUE, 
         # breaks=cuts, 
         #col = snow_palette(7),
         legend = FALSE)
  }
  return(plot_fun)
}
