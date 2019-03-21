
prep_snow_fun <- function(snow_rasters, snow_cfg, timestep) {
  
  # timestep is at finer resolution than ymd_str
  # need some code here to figure out which 
  # ymd_str to use based on the timestep
  ymd_str <- format(as.POSIXct(timestep, tz="UTC"), "%Y%m%d")
  snow_palette <- colorRampPalette(c("#FAFBF399", "#F0F8E3CC", "#D4E9CAE6", "#BBE0CE", "#B7DAD0", "#B0CCD7", "#A9B8D7"))
  
  # subset raster to just this timestep
  one_snow_raster <- snow_rasters[[ymd_str]]
  #Crop here for now
  crop_extent <- readRDS("1_fetch/out/focus_geoms.rds")
  one_snow_raster_crop <- raster::crop(one_snow_raster, crop_extent)
  
  # clean up the environment to keep the closure small
  rm(snow_rasters, timestep)
  
  plot_fun <- function(){
    plot(one_snow_raster, 
         add = TRUE, 
         # breaks=cuts, 
         #col = snow_palette(7),
         legend = FALSE)
  }
  return(plot_fun)
}
