#' Subset the site list to ones relevant for the storm
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for an sf data.frame of sites with automatic filtering applied already
filter_sites_custom <- function(ind_file, sites_ind, dates) {

  # get sites data frame with NWS data
  # remove spatial component, so that joining/filtering as data will work
  sites_df <- as.data.frame(readRDS(sc_retrieve(sites_ind)))
  sites_df_notgeom <- dplyr::select(sites_df, site_no, flood_stage)
  
  daily_stage <- dataRetrieval::readNWISdv(
    siteNumbers = sites_df_notgeom$site_no, 
    parameterCd = "00065",
    startDate = as.Date(dates$start),
    endDate = as.Date(dates$end)) %>% 
    renameNWISColumns() %>% 
    # will remove NA observations, so sites where there is data in the time period will stay
    filter(!is.na(GH)) 
  
  only_flood <- left_join(daily_stage, sites_df_notgeom) %>% 
    filter(GH >= flood_stage) 
  
  # select all sites flooding in the time period
  sites_df_flood <- sites_df_notgeom %>% 
    filter(site_no %in% only_flood$site_no)
  
  # randomly select other sites around the area to include
  set.seed(60)
  sites_df_random <- sites_df_notgeom %>% 
    filter(!site_no %in% sites_df_flood$site_no) %>% 
    sample_n(50)

  # combine
  sites_info_subset <- full_join(sites_df_flood, sites_df_random)
  
  # write the data file and the indicator file
  if(packageVersion('scipiper') < package_version('0.0.11')) stop('1-arg version of gd_put requires scipiper 0.0.11+')
  saveRDS(sites_info_subset, as_data_file(ind_file))
  gd_put(ind_file)

}
