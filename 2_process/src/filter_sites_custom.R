#' Subset the site list to ones relevant for the storm
#'
#' @param ind_file character file name where the output should be saved
#' @param sites_ind indicator file for an sf data.frame of sites with automatic filtering applied already
filter_sites_custom <- function(ind_file, sites_ind) {

  # get sites data frame with NWS data
  sites_df <- readRDS(sc_retrieve(sites_ind))

  ### filter sites_df here ###
  hand_picked_sites <- c("02236125", "02197000", "02223000", "02226000", "0208062765", 
                         "02081000", "02081028", "02091814", "02102500", "02104000", "02105500", 
                         "02105769", "02129000", "02131000", "02147020", "02148000", "02161000", 
                         "02169500", "02169625", "02171700", "02026000", "02035000", "02037500", 
                         "02066000")
  sites_df_subset <- sites_df %>% filter(site_no %in% hand_picked_sites)
  
  
  # write the data file and the indicator file
  if(packageVersion('scipiper') < package_version('0.0.11')) stop('1-arg version of gd_put requires scipiper 0.0.11+')
  saveRDS(sites_df_subset, as_data_file(ind_file))
  gd_put(ind_file)

}
