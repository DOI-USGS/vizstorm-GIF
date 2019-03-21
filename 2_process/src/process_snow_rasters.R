
process_snow_rasters <- function(ind_file, snow_data_ind, proj_str) {

  # temp solution; should have more robust one for real deal
  snow_files_ind <- names(yaml::yaml.load_file(snow_data_ind))
  snow_files <- as_data_file(snow_files_ind)
  # ymd_str <- gsub("\\(([1-9]{8})\\)", "\\1", snow_files)
  ymd_str_almost <- gsub("1_fetch/out/snow_", "", snow_files)
  ymd_str <- gsub(".dat", "", ymd_str_almost)
  
  # Size of SNODAS data
  n_col <- 8192
  n_row <- 4096 
  
  # boundary extent for unmasked SNODAS
  x0 <- -130.516666666661
  x1 <- -62.2499999999975
  y0 <- 24.0999999999990
  y1 <- 58.2333333333310
  
  snow_raster_list <- list()
  for(i in 1:length(snow_files)) {
    snow_depth <- readBin(snow_files[i], integer(), n=n_row*n_col, size=2, signed=TRUE, endian='big')
    snow_depth[snow_depth <= 0] <- as.numeric(NA)
    snow_depth_mat <- matrix(snow_depth, nrow = n_col)
    
    # Convert to raster
    snow_raster <- raster::raster(ncol=n_col, nrow=n_row, xmn = x0, xmx = x1, ymn = y0, ymx = y1)
    snow_raster <- raster::setValues(snow_raster, snow_depth)
    
    # Add the correct projection
    # Source for WGS 84 projection: "Projecting SNODAS Data" section on https://nsidc.org/data/g02158
    raster::crs(snow_raster) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    
    # Now reproject for this visualization
    snow_raster_proj <- raster::projectRaster(snow_raster, crs = raster::crs(proj_str))
    
    snow_raster_list[i] <- snow_raster_proj
    message(sprintf("Completed %s out of %s", i, length(snow_files)))
  }
  
  # get list of rasters named by ymd_str
  names(snow_raster_list) <- ymd_str
  
  data_file <- as_data_file(ind_file)
  saveRDS(snow_raster_list, data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
  
}
