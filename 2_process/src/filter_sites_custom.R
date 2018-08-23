
#' Subset the site list to ones relevant for the storm
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for an sf data.frame of sites with automatic filtering applied already
filter_sites_custom <- function(ind_file, sites_ind) {

  # get sites data frame with NWS data
  sites_df <- readRDS(sc_retrieve(sites_ind))

  # subset the sites from the wide net cast to ones relevant to the storm
  # subset criteria TBD for each storm
  sites_info <- dataRetrieval::readNWISsite(siteNumbers = sites_df$site_no)

  sites_info_subset <- sites_info %>%
    # only include sites with large drainage areas
    filter(drain_area_va > quantile(drain_area_va, 0.75, na.rm=TRUE)) %>%
    # manually subsetting to 20 sites
    filter(site_no %in% c("08114000", "08160400", "08066250", "08162000", "08161000",
                          "08033500", "08111500", "08175800", "08066500", "08040600",
                          "08162500", "08116650", "08173900", "08041780", "08159500",
                          "08067000", "08072050", "08106350", "08106500", "08041000"))

  # write the data file and the indicator file
  data_file <- as_data_file(ind_file)
  saveRDS(sites_info_subset, data_file)
  gd_put(ind_file, data_file)

}
