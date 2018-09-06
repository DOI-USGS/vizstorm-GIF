#' Subset the site list to ones relevant for the storm
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for an sf data.frame of sites with automatic filtering applied already
filter_sites_custom <- function(ind_file, sites_ind) {

  # get sites data frame with NWS data
  sites_df <- readRDS(sc_retrieve(sites_ind))
  
  ### filter sites_df here ###
  set.seed(19)
  sites_info <- dataRetrieval::readNWISsite(siteNumbers = sites_df$site_no)
  sites_info_subset <- sites_info %>%
    # only include sites with large drainage areas
    filter(drain_area_va > quantile(drain_area_va, 0.75, na.rm=TRUE)) %>% 
    # these sites keep failing on fetching river geometries:
    filter(!site_no %in% c("07350500", "07348500")) %>% 
    # after seeing these sites on the map, taking them out for various reasons
    filter(!site_no %in% c("08193000", "08159200", "02244040", "09396100",
                           "07157950", "06847000", "02246500", "02233960",
                           "06910450", "06770200", "08151500", "08188060",
                           "06892350", "07241800", "08062500", "02243960",
                           "08062700", "02148000", "06906500", "06837000",
                           "02315550", "07241550", "06891000", "06835500",
                           "08194000", "06721000", "06610000", "06827500",
                           "06609100", "02486000", "03431790", "06710247",
                           "03377500", "02147020", "06926080", "07159100",
                           "07160000", "08158000", "08194500", "02484650")) %>%
    # these sites didn't have any data during this time period
    filter(!site_no %in% c("06828500", "06891080", "07250550", "06844500")) %>% 
    sample_n(20) %>% 
    arrange(lat_va)

  sites_df_subset <- sites_df %>% 
    filter(site_no %in% sites_info_subset$site_no)
  
  # Now make sure the final data is in the appropriate order
  sites_df_subset <- sites_df_subset[match(sites_info_subset$site_no, sites_df_subset$site_no),]
  
  # write the data file and the indicator file
  if(packageVersion('scipiper') < package_version('0.0.11')) stop('1-arg version of gd_put requires scipiper 0.0.11+')
  saveRDS(sites_df_subset, as_data_file(ind_file))
  gd_put(ind_file)

}
