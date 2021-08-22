#' Convert the sites data frame into an sf data.frame
#'
#' @param ind_file
#' @param sites_ind indicator file for an sf data.frame with at least
#'    the following columns: site_no, station_nm, geometry, site_no_nws, flood_stage
#' @param sites_custom_ind indicator file for data.frame with at least site_no
filter_using_custom <- function(ind_file, sites_ind, sites_custom_ind) {

  sites_df <- readRDS(sc_retrieve(sites_ind, remake_file = getOption("scipiper.remake_file")))
  sites_custom_df <- readRDS(sc_retrieve(sites_custom_ind, remake_file = getOption("scipiper.remake_file")))
  sites_final_df <- dplyr::filter(sites_df, site_no %in% sites_custom_df$site_no)

  # write the data file and the indicator file
  data_file <- as_data_file(ind_file)
  saveRDS(sites_final_df, data_file)
  gd_put(ind_file, data_file)
}
