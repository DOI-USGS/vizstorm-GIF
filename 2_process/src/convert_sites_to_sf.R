#' Convert the sites data frame into an sf data.frame
#'
#' @param ind_file
#' @param sites_ind indicator file for a data.frame with at least
#'    the following columns: site_no, dec_lat_va, dec_long_va, flood_stage
convert_sites_to_sf <- function(ind_file, sites_ind) {

  # read the sites data frame from the shared cache
  sites_df <- readRDS(sc_retrieve(sites_ind))

  # convert to sf
  sites_sf <- sf::st_as_sf(sites_df, coords = c("dec_long_va", "dec_lat_va"), crs = 4326)

  # write the data file and the indicator file
  data_file <- as_data_file(ind_file)
  saveRDS(sites_sf, data_file)
  gd_put(ind_file, data_file)
}
