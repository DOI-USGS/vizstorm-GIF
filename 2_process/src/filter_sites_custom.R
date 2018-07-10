
#' Subset the site list to ones relevant for the storm
#'
#' @param ind_file character file name where the output should be saved
#' @param sites data.frame of sites with automatic filtering applied already
filter_sites_custom <- function(ind_file, sites) {

  sites <- readRDS(scipiper::sc_retrieve(sites))

  # subset the sites from the wide net cast to ones relevant to the storm
  # subset criteria TBD
  sites_info <- dataRetrieval::readNWISsite(siteNumbers = sites)

  #############********* temporarily grab only a few sites *********#############
  set.seed(10)
  sites_info <- dplyr::sample_n(sites_info, 50)
  #############*********

  # write the data file and the indicator file
  data_file <- as_data_file(ind_file)
  feather::write_feather(sites_info, data_file)
  gd_put(ind_file, data_file)

}
