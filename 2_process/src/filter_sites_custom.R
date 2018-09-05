#' Subset the site list to ones relevant for the storm
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for an sf data.frame of sites with automatic filtering applied already
filter_sites_custom <- function(ind_file, sites_ind) {

  # get sites data frame with NWS data
  sites_df <- readRDS(sc_retrieve(sites_ind))
  
  ### filter sites_df here ###
  sites_info <- dataRetrieval::readNWISsite(siteNumbers = sites_df$site_no)
  sites_info_subset <- sites_info %>%
    # only include sites with large drainage areas
    filter(drain_area_va > quantile(drain_area_va, 0.95, na.rm=TRUE)) %>% 
    # this site keeps failing on fetching geometries:
    filter(site_no != "07350500")

  sites_df_subset <- sites_df %>% 
    filter(site_no %in% sites_info_subset$site_no)
  
  # write the data file and the indicator file
  if(packageVersion('scipiper') < package_version('0.0.11')) stop('1-arg version of gd_put requires scipiper 0.0.11+')
  saveRDS(sites_info_subset, as_data_file(ind_file))
  gd_put(ind_file)

}
