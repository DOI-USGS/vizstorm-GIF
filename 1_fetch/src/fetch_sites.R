#' Download all NWIS stage sites within the bounding box of a supplied spatial object
#'
#' @param ind_file character file name where the output should be saved
#' @param sf_object spatial object of region used for storm, sf
#' @param dates object from viz_config.yaml that specifies dates as string
fetch_sites_from_sf <- function(ind_file, sf_object, dates) {

  # Extract bounding box and reproject
  bbox_projected <- sf::st_transform(sf_object, crs = "+proj=longlat +datum=NAD27 +no_defs")
  object_bbox <- sf::st_bbox(bbox_projected)

  # temp fix so this can get into the workflow
  # how we get a bounding box (or maybe we use state fips) will change
  object_bbox[3] <- object_bbox[1] + 10
  object_bbox[2] <- object_bbox[4] - 1.2

  object_bbox <- round(as.vector(object_bbox), 7)

  # Cast wide net for all NWIS sites with stage data that fall within that bbox
  sites_df <- dataRetrieval::whatNWISdata(bBox = object_bbox,
                                       parameterCd = "00065",
                                       service = "uv")
  sites_df <- dplyr::filter(sites_df,
                            site_tp_cd == "ST",
                            end_date >= as.Date(dates$dates$end))
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

  scipiper::sc_retrieve(all_sites)
  all_sites <- readRDS(scipiper::as_data_file(all_sites))

  # subset the sites from the wide net cast to ones relevant to the storm
  # subset criteria TBD
  sites <- dataRetrieval::readNWISsite(siteNumbers = all_sites)

  # write the data file and the indicator file
  data_file <- as_data_file(ind_file)
  feather::write_feather(sites, data_file)
  gd_put(ind_file, data_file)

}
