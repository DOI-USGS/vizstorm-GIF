
#' Subset the site list to ones relevant for the storm
#'
#' @param ind_file character file name where the output should be saved
#' @param all_sites data.frame of sites pulled for the bbox
custom_subset <- function(ind_file, all_sites) {

  all_sites <- readRDS(scipiper::sc_retrieve(all_sites))

  # subset the sites from the wide net cast to ones relevant to the storm
  # subset criteria TBD
  sites_info <- dataRetrieval::readNWISsite(siteNumbers = all_sites)

  #############********* temporarily grab only a few sites *********#############
  set.seed(10)
  sites_info <- dplyr::sample_n(sites_info, 50)
  #############*********

  # write the data file and the indicator file
  data_file <- as_data_file(ind_file)
  feather::write_feather(sites_info, data_file)
  gd_put(ind_file, data_file)

}
