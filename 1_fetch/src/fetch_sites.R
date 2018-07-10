#' Get vector of state codes from the sf_object
#'
#' @param sf_object spatial object with IDs indicating the state name
fetch_states_from_sf <- function(sf_object) {

  state_names <- sf_object$ID[!grepl(",", sf_object$ID)]
  state_cds <- unique(dataRetrieval::stateCdLookup(state_names))

  return(state_cds)
}

#' Download all NWIS stage sites within the bounding box of a supplied spatial object
#'
#' @param ind_file character file name where the output should be saved
#' @param state_cds spatial object with IDs indicating the state name
#' @param dates object from viz_config.yaml that specifies dates as string
#' @param stream_params pcodes to use from NWIS
fetch_sites_from_states <- function(ind_file, state_cds, dates, stream_params) {

  # Cast wide net for all NWIS sites with stage data that fall within that bbox
  sites_df <- data.frame()
  for(cd in state_cds) {
    sites_df_cd <- dataRetrieval::whatNWISdata(stateCd = cd, parameterCd = stream_params$stage, service = "uv")
    sites_df <- rbind(sites_df, sites_df_cd)
  }
  sites_df <- dplyr::filter(sites_df, site_tp_cd == "ST")
  sites_df <- dplyr::filter(sites_df, end_date >= as.Date(dates$end))
  sites <- sites_df$site_no

  # write the data file and the indicator file
  data_file <- as_data_file(ind_file)
  saveRDS(sites, data_file)
  gd_put(ind_file, data_file)

}

#' Subset the site list to ones relevant for the storm
#'
#' @param ind_file character file name where the output should be saved
#' @param all_sites data.frame of sites pulled for the bbox
subset_sites <- function(ind_file, all_sites) {

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
