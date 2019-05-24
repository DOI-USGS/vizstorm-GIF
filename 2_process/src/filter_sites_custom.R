#' Subset the site list to ones relevant for the storm
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for an sf data.frame of sites with automatic filtering applied already
filter_sites_custom <- function(ind_file, sites_ind) {

  # get sites data frame with NWS data
  sites_df <- readRDS(sc_retrieve(sites_ind))

  ### filter sites_df here ###
  sites_info_subset <- sites_df %>%
    filter(site_no %in% c("01482500", "02481000", "02481510", "02492000", "04186500",
                          "05587450", "06421500", "06446000", "06786000", "07151000",
                          "07152500", "07153000", "07164500", "07165570", "07176500",
                          "07177500", "07178000", "07242380", "08057200", "040851385"))

  # write the data file and the indicator file
  if(packageVersion('scipiper') < package_version('0.0.11')) stop('1-arg version of gd_put requires scipiper 0.0.11+')
  saveRDS(sites_info_subset, as_data_file(ind_file))
  gd_put(ind_file)

}
