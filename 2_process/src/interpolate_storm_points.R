#' create an sf POINTS collection with a point along the storm track for each
#' timestep. interpolates from coarse_points
interpolate_storm_points <- function(ind_file, timesteps_ind, coarse_points_ind) {

  # read in the coarse points storm track
  coarse_points <- readRDS(sc_retrieve(coarse_points_ind, remake_file = getOption("scipiper.remake_file")))

  if(is.null(coarse_points)) {
    coords_interp_sf <- NULL
  } else {
    # get the vector of timepoints we want to include
    timesteps <- readRDS(sc_retrieve(timesteps_ind, remake_file = getOption("scipiper.remake_file")))

    # interpolate the already-projected points to the timestamps we actually want
    coords_interp <- data_frame(
      DateTime = timesteps,
      X = approx(coarse_points$DateTime, sf::st_coordinates(coarse_points)[,'X'], xout = timesteps)$y,
      Y = approx(coarse_points$DateTime, sf::st_coordinates(coarse_points)[,'Y'], xout = timesteps)$y,
      SS = round(approx(coarse_points$DateTime, coarse_points$SS, xout = timesteps)$y)) %>%
      filter(., complete.cases(.))
    coords_interp_sf <- sf::st_as_sf(coords_interp, coords=c('X','Y'))
  }

  # write the sf POINTS object to file
  data_file <- as_data_file(ind_file)
  saveRDS(coords_interp_sf, data_file)
  gd_put(ind_file, data_file)
}
