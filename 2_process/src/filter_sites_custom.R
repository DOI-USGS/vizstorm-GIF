#' Subset the site list to ones relevant for the storm
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for an sf data.frame of sites with automatic filtering applied already
filter_sites_custom <- function(ind_file, sites_ind, selection_ind) {

  # get sites data frame with NWS data
  sites_df <- readRDS(sc_retrieve(sites_ind))

  # pull in the rds file from the shiny app, then use that to define picked sites
  selection_df <- readRDS(sc_retrieve(selection_ind))
  sites_info_subset <- sites_df %>%
    dplyr::filter(site_no %in% dplyr::filter(selection_df, picked_sites)$site_no)

  # write the data file and the indicator file
  if(packageVersion('scipiper') < package_version('0.0.11')) stop('1-arg version of gd_put requires scipiper 0.0.11+')
  saveRDS(sites_info_subset, as_data_file(ind_file))
  gd_put(ind_file)

}
