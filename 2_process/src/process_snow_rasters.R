
process_snow_raster <- function(ind_file, snow_data_ind, snow_data_yml, crop_extent_raster_ind, proj_str) {
  
  snow_data_fn <- sc_retrieve(snow_data_ind, snow_data_yml)
  crop_extent_sfcpolygon <- readRDS(sc_retrieve(crop_extent_raster_ind))
  
  # Size of SNODAS data
  n_col <- 8192
  n_row <- 4096 
  
  # boundary extent for unmasked SNODAS
  x0 <- -130.516666666661
  x1 <- -62.2499999999975
  y0 <- 24.0999999999990
  y1 <- 58.2333333333310
  
  snow_depth <- readBin(snow_data_fn, integer(), 
                        n=n_row*n_col, size=2, signed=TRUE, endian='big')
  snow_depth[snow_depth <= 0] <- 0
  snow_depth_mat <- matrix(snow_depth, nrow = n_col)
  
  # Convert to raster
  snow_raster <- raster::raster(ncol=n_col, nrow=n_row, xmn = x0, xmx = x1, ymn = y0, ymx = y1)
  snow_raster <- raster::setValues(snow_raster, snow_depth)
  
  # Add the correct projection
  # Source for WGS 84 projection: "Projecting SNODAS Data" section on https://nsidc.org/data/g02158
  raster::crs(snow_raster) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  # Now reproject for this visualization
  snow_raster_proj <- raster::projectRaster(snow_raster, crs = raster::crs(proj_str))
  
  # Now crop
  crop_extent_geom <- sf::st_sf(sf::st_geometry(crop_extent_sfcpolygon))
  snow_raster_proj_crop <- raster::crop(snow_raster_proj, crop_extent_geom)
  
  data_file <- as_data_file(ind_file)
  saveRDS(snow_raster_proj_crop, data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
  
}
