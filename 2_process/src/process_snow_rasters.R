
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

interpolate_snow_raster_layers <- function(timestep_in_hours, ...) {

  snow_raster_rds_inds <- c(...) #would be either passing in dates or files here?
  rasters_list <- list()
  #need to get a list of rasters read in from .RDS files
  for(i in seq_along(snow_raster_rds_inds)) {
    rasters_list[[i]] <- readRDS(as_data_file(snow_raster_rds_inds[i]))
  }
  raster_stack <- raster::stack(x = rasters_list)

  daily_rasters <- raster::nlayers(raster_stack)
  hours_covered <- (daily_rasters - 1)*24 #since we aren't extrapolating
  steps <- seq(from = 0, by = timestep_in_hours,
               length.out = (hours_covered/timestep_in_hours)+1)
  have_data <- which(steps %% 24 == 0) #these are the steps to interpolate between
  #insert all NA layers, then na.spline interpolates NA values
  empty <- rep(NA, length(steps))
  fun <- function(y) {
    if(all(is.na(y))) {
      empty
    } else {
      zoo::na.approx((empty[have_data] <- y), xout=1:length(steps))
    }
  }
  interp_raster_stack <- calc(raster_stack,  fun)
  #could create raster timeseries here for referencing to timestep
  #https://rdrr.io/cran/rts/man/rts.html

  #TODO: write out indicator and data files
  #use raster::subset to get  individual layers
}
