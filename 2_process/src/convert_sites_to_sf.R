#' Convert the sites data frame into an sf data.frame
#'
#' @param ind_file
#' @param sites_ind indicator file for a data.frame with at least
#'    the following columns: site_no, station_nm, dec_lat_va, dec_long_va, site_no_nws, flood_stage
#' @param sites_custom_ind indicator file for a data.frame that has been put
#'    through a custom filtering function with at least the site_no column
convert_sites_to_sf <- function(ind_file, sites_ind, sites_custom_ind) {

  # read the sites data frame from the shared cache
  sites_df <- feather::read_feather(sc_retrieve(sites_ind))
  sites_custom_df <- readRDS(sc_retrieve(sites_custom_ind))
  sites_final_df <- sites_custom_df %>%
    dplyr::left_join(sites_df) %>%
    dplyr::select(site_no, station_nm, dec_lat_va, dec_long_va,
                  site_no_nws, flood_stage, flood_stage_units)

  # convert to sf
  sites_sf <- sf::st_as_sf(sites_final_df, coords = c("dec_long_va", "dec_lat_va"), crs = 4326)

  # write the data file and the indicator file
  data_file <- as_data_file(ind_file)
  saveRDS(sites_sf, data_file)
  gd_put(ind_file, data_file)
}
