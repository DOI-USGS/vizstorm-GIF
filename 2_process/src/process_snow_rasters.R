
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

interpolate_snow_raster_layers <- function(ind_file, timestep_ind, frame_step, fetch_snow_tasks_ind) {

  snow_raster_rds_inds <- names(yaml::yaml.load_file(fetch_snow_tasks_ind))
  all_timestep <- readRDS(sc_retrieve(timestep_ind))
  
  timesteps <- all_timestep[seq(1, by = frame_step, to = length(all_timestep))] 
  
  # Expect one snow raster per day
  daily_timesteps <- unique(as.Date(timesteps))
  stopifnot(length(daily_timesteps) == length(snow_raster_rds_inds))
  
  # Get RDS daily raster files read in to list first
  daily_rasters <- lapply(snow_raster_rds_inds, function(snow_ind) readRDS(as_data_file(snow_ind)))
  
  message(length(daily_rasters), " Rasters read in, starting interpolation")
  
  # Loop through rasters and interpolate
  rasters_interp_list <- list()
  n_interp <- seq_along(daily_timesteps) 
  for(day in n_interp) {
    snow_depth0 <- daily_rasters[[day]]
    
    # Interpolate the final day using only itself at 00
    if(day == length(n_interp)) {
      # Only loops through hour=0 for the final day
      hourly_timesteps <- 0
      snow_depth1 <- NULL
    } else {
      hourly_timesteps <- seq(0, 23, by = frame_step)
      snow_depth1 <- daily_rasters[[day+1]]
    }
    
    for(hour in hourly_timesteps) {
      
      if(hour == 0) {
        # no interpolation for the daily steps (causes NA issues)
        snow_depth_interp <- snow_depth0
      } else {
        # interpolate between the daily rasters
        snow_depth_interp <- 
          (snow_depth0 * (1 / hour) + snow_depth1 * (1 / (24-hour))) / 
          (( 1 / hour) + (1 / (24-hour)))
      }
      
      # Change 0s & less than 0s to NAs for plotting
      snow_depth_interp[snow_depth_interp <= 0] <- NA
      
      # build name of current timestep
      cur_timestep <- paste(strftime(daily_timesteps[day], format = '%Y%m%d', tz = 'UTC'), sprintf("%02d", hour), sep = "_")
      
      # Add interpolation to the list
      rasters_interp_list[[cur_timestep]] <- snow_depth_interp
    }
  }
  
  raster_stack <- raster::stack(x = rasters_interp_list)
  
  data_file <- as_data_file(ind_file)
  raster::writeRaster(raster_stack, data_file, overwrite=TRUE)
  
  # It is quicker to build locally, then push + pull a file of this size (~700 MB for 8 days)
  gd_put(remote_ind = ind_file, dry_put = TRUE) 
}
