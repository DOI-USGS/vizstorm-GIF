#' Subset the site list to ones relevant for the storm
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for an sf data.frame of sites with automatic filtering applied already
filter_sites_custom <- function(ind_file, sites_ind, dates) {

  # get sites data frame with NWS data
  # remove spatial component, so that joining/filtering as data will work
  sites_df <- as.data.frame(readRDS(sc_retrieve(sites_ind)))
  sites_df_notgeom <- dplyr::select(sites_df, site_no, flood_stage)
  
  # Use just Mississippi river states to filter for flooding
  ms_sites <- dataRetrieval::readNWISsite(sites_df$site_no) %>% 
    # MN, WI, IA, IL, KY, MO, MS, AR
    filter(state_cd %in% c(27, 55, 19, 17, 21, 29, 28, 05)) %>% 
    left_join(sites_df_notgeom)
  
  ### Filter to sites that will flood during this time period
  ### This is based on daily average, not inst
  daily_stage <- dataRetrieval::readNWISdv(
    siteNumbers = ms_sites$site_no, 
    parameterCd = "00065",
    startDate = as.Date(dates$start),
    endDate = as.Date(dates$end)) %>% 
    renameNWISColumns() %>% 
    # will remove NA observations, so sites where there is data in the time period will stay
    filter(!is.na(GH)) 
  
  # select all sites flooding in the time period
  only_flood_df <- left_join(daily_stage, ms_sites) %>% 
    filter(GH >= flood_stage)
  sites_df_flood <- ms_sites %>% 
    filter(site_no %in% only_flood_df$site_no)
  
  # randomly select sites along mississippi
  set.seed(60)
  sites_df_ms_random <- ms_sites %>% 
    filter(!site_no %in% sites_df_flood$site_no) %>% 
    sample_n(100)
  
  # randomly select other sites around the area to include
  set.seed(19)
  sites_df_nearby_random <- sites_df_notgeom %>% 
    filter(!site_no %in% c(sites_df_flood$site_no, ms_sites$site_no)) %>% 
    sample_n(50)
  
  # combine
  sites_info_subset <- data.frame(
    site_no = c(sites_df_flood$site_no, 
                sites_df_ms_random$site_no,
                sites_df_nearby_random$site_no)
  )
  
  # write the data file and the indicator file
  if(packageVersion('scipiper') < package_version('0.0.11')) stop('1-arg version of gd_put requires scipiper 0.0.11+')
  saveRDS(sites_info_subset, as_data_file(ind_file))
  gd_put(ind_file)

}
