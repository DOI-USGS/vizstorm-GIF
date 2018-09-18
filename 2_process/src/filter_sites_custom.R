#' Subset the site list to ones relevant for the storm
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for an sf data.frame of sites with automatic filtering applied already
filter_sites_custom <- function(ind_file, sites_ind) {

  # get sites data frame with NWS data
  sites_df <- readRDS(sc_retrieve(sites_ind))

  ### filter sites_df here ### 
  hand_picked_sites <- c('02071000','02084472','02086500','02087570','02089000','02089500','02091500','0209205053',
                         '02092500','02092554','02096500','02096960','02100500','02102000','02102500','02103000',
                         '02104000','02105769','02106500','02108000','02108566','02121500','0212433550','02129000',
                         '02133500','02134170','02134480','02140991','03161000','02110704','02135000','02146800','02171700','02054500','02055000','02076000','03168000')
  sites_df_subset <- sites_df %>% filter(site_no %in% hand_picked_sites)
  
  
  # write the data file and the indicator file
  if(packageVersion('scipiper') < package_version('0.0.11')) stop('1-arg version of gd_put requires scipiper 0.0.11+')
  saveRDS(sites_df_subset, as_data_file(ind_file))
  gd_put(ind_file)

}
