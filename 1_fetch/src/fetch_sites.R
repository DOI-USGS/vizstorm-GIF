#' Get vector of state codes from the sf_object
#'
#' @param sf_ind indicator file for a spatial object with IDs indicating the
#'   state name
fetch_states_from_sf <- function(sf_ind) {

  # read the sf object from the shared cache
  sf_object <- readRDS(sc_retrieve(sf_ind))

  state_names <- sf_object$ID[!grepl(",", sf_object$ID)]
  state_cds <- unique(dataRetrieval::stateCdLookup(state_names))

  return(state_cds)
}

#' Download all NWIS stage sites within the bounding box of a supplied spatial object
#'
#' @param state_cds spatial object with IDs indicating the state name
#' @param dates object from viz_config.yaml that specifies dates as string
#' @param stream_params pcodes to use from NWIS
fetch_sites_from_states <- function(state_cds, dates, stream_params) {

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

#' Get list of corresponding NWS site numbers from NWIS site numbers
#'
#' @param state_cds vector of states for this storm
fetch_nws_to_nwis_crosswalk <- function(state_cds){

  conversion_table <- data.frame()
  for(s in state_cds) {
    conversion_url <- sprintf("http://www.nws.noaa.gov/oh/hads/USGS/%s_USGS-HADS_SITES.txt", s)
    conversion_table_state <- readr::read_delim(conversion_url,
                                                delim = "|",skip = 4,col_names = FALSE)
    conversion_table <- dplyr::bind_rows(conversion_table, conversion_table_state)
  }


  names(conversion_table) <- c("NWS","USGS","GOES","NWS HSA","lat","lon","name")
  conversion_table$USGS <- gsub(" ","", conversion_table$USGS)
  nws_nwis_crosswalk <- dplyr::select(conversion_table, site_no_nws = NWS, site_no=USGS)

  return(nws_nwis_crosswalk)
}

#' Download National Weather Service data for sites and filter any without stage data
#'
#' @param ind_file character file name where the output should be saved
#' @param nwis_sites vector of site numbers to consider for this storm
#' @param nws_nwis_crosswalk data.frame with NWS site numbers and their corresponding USGS site number
fetch_nws_data <- function(ind_file, nwis_sites, nws_nwis_crosswalk) {

  sites_df <- left_join(nwis_sites, nws_nwis_crosswalk, by="site_no")

  # Start by setting stage info as NA
  sites_df$flood_stage <- NA
  sites_df$flood_stage_units <- NA

  # Download NWS info for each gage
  site_nums_to_query <- unique(sites_df$site_no_nws)
  i_count <- 0
  for(i in site_nums_to_query){

    if(is.na(i)) {
      i_count <- i_count + 1
      print(paste(i_count, "/", length(site_nums_to_query)))
      next
    }

    url_site <- paste0("https://water.weather.gov/ahps2/hydrograph_to_xml.php?gage=",i,"&output=xml")
    return_list <- GET(url_site)
    returnedDoc <- content(return_list,encoding = "UTF-8")
    nws_site <- xml_root(returnedDoc)
    sigstages <- xml_find_all(nws_site, "sigstages")

    if(length(sigstages) > 0){
      sites_df$flood_stage[which(sites_df$site_no_nws %in% i)] <- as.numeric(xml_text(xml_find_all(sigstages, "flood")))
      sites_df$flood_stage_units[which(sites_df$site_no_nws %in% i)] <- xml_attr(xml_find_all(sigstages, "flood"),"units")
    }

    i_count <- i_count + 1
    print(paste(i_count, "/", length(site_nums_to_query)))
  }

  # Filter out any sites that don't have flood stage data from NWS
  sites_w_flood_stage <- dplyr::filter(sites_df, !is.na(flood_stage))

  # write the data file and the indicator file
  data_file <- as_data_file(ind_file)
  feather::write_feather(sites_info, data_file)
  gd_put(ind_file, data_file)

  return(sites_w_flood_stage)
}
