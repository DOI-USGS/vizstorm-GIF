
#' Subset the site list to ones relevant for the storm
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind ind file for data.frame of sites with automatic filtering applied already
filter_sites_custom <- function(ind_file, sites_ind) {

  sites_df <- readRDS(scipiper::sc_retrieve(sites_ind))

  # subset the sites from the wide net cast to ones relevant to the storm
  # subset criteria TBD for each storm
  sites_info <- dataRetrieval::readNWISsite(siteNumbers = sites_df$site_no)

  sites_info_subset <- sites_info %>%
    # only include sites with large drainage areas
    filter(drain_area_va > quantile(drain_area_va, 0.75, na.rm=TRUE))

  # write the data file and the indicator file
  data_file <- as_data_file(ind_file)
  saveRDS(sites_info_subset, data_file)
  gd_put(ind_file, data_file)

}
