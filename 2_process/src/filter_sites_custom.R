
#' Subset the site list to ones relevant for the storm
#'
#' @param ind_file character file name where the output should be saved
#' @param sites data.frame of sites with automatic filtering applied already
filter_sites_custom <- function(ind_file, sites_file) {

  sites <- readRDS(scipiper::sc_retrieve(sites_file))

  # subset the sites from the wide net cast to ones relevant to the storm
  # subset criteria TBD
  sites_info <- dataRetrieval::readNWISsite(siteNumbers = sites)

  sites_info_subset <- sites_info %>%
    # only include sites with large drainage areas
    filter(drain_area_va > quantile(drain_area_va, 0.75, na.rm=TRUE))

  # write the data file and the indicator file
  data_file <- as_data_file(ind_file)
  feather::write_feather(sites_info_subset, data_file)
  gd_put(ind_file, data_file)

}
