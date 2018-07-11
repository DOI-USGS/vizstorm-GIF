#' Download all NWIS stage sites within the bounding box of a supplied spatial
#' object
#'
#' @param ind_file character file name where the output should be saved
#' @param sf_ind indicator file for a spatial object with IDs indicating the
#'   state name
#' @param dates object from viz_config.yaml that specifies dates as string
#' @param stream_params pcodes to use from NWIS
fetch_sites_from_sf <- function(ind_file, sf_ind, dates, stream_params) {

  # read the sf object from the shared cache
  sf_object <- readRDS(sc_retrieve(sf_ind))

  state_names <- sf_object$ID[!grepl(",", sf_object$ID)]
  state_cds <- unique(dataRetrieval::stateCdLookup(state_names))

  # Cast wide net for all NWIS sites with stage data that fall within that bbox
  sites_df <- data.frame()
  for(cd in state_cds) {
    sites_df_cd <- dataRetrieval::whatNWISdata(stateCd = cd, parameterCd = stream_params$stage, service = "uv")
    sites_df <- rbind(sites_df, sites_df_cd)
  }

  # Filtering applied to every storm
  sites_df <- sites_df %>%
    # we only need stream sites
    filter(site_tp_cd == "ST") %>%
    # keeps only sites that have data since the start of the storm
    # if a gage goes out during the storm, this filter would still capture that gage
    filter(end_date >= as.Date(dates$start))

  # return only the site numbers
  sites <- sites_df$site_no

  # write the data file and the indicator file
  data_file <- as_data_file(ind_file)
  saveRDS(sites, data_file)
  gd_put(ind_file, data_file)

}
