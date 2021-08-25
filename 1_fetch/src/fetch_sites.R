#' Get vector of state codes from the sf_object
#'
#' @param sf_ind indicator file for a spatial object with IDs indicating the
#'   state name
fetch_states_from_sf <- function(sf_ind) {

  # read the sf object from the shared cache
  sf_object <- readRDS(sc_retrieve(sf_ind, remake_file = getOption("scipiper.remake_file")))

  state_names <- sf_object$ID[!grepl(",", sf_object$ID)]

  if(length(state_names) == 0) {
    state_names <- sapply(sf_object$ID, function(x) strsplit(x, ",")[[1]][1])
  }

  state_cds <- unique(dataRetrieval::stateCdLookup(state_names))

  return(state_cds)
}

#' Download all NWIS stage sites within the states specified
#'    that also have NWS flood stage data
#'
#' @param ind_file character file name where the output should be saved
#' @param state_cds spatial object with IDs indicating the state name
#' @param dates object from viz_config.yaml that specifies dates as string
#' @param stream_params pcodes to use from NWIS
fetch_sites_from_states <- function(ind_file, state_cds, dates, stream_params, require_flood_stage=TRUE) {

  # Cast wide net for all NWIS sites with stage data that fall within that bbox
  sites_df <- bind_rows(lapply(state_cds, function(cd) {
    dataRetrieval::whatNWISdata(stateCd = cd, parameterCd = stream_params$stage, service = "uv") %>%
      dplyr::select(site_no, station_nm, dec_lat_va, dec_long_va, site_tp_cd, end_date, begin_date)
  }))

  # Get and join NWS flood stage table
  nws_flood_stage_list <- jsonlite::fromJSON("https://waterwatch.usgs.gov/webservices/floodstage?format=json")
  nws_flood_stage_table <- nws_flood_stage_list[["sites"]]
  sites_df <- sites_df %>%
    left_join(nws_flood_stage_table, by='site_no')

  # Optionally filter out any sites that don't have flood stage data from NWS
  if(require_flood_stage) {
    sites_df <- sites_df %>%
      dplyr::filter(!is.na(flood_stage))
  }

  # Filter by site type and data availability during storm
  sites_filtered <- sites_df %>%
    # we only need stream sites
    dplyr::filter(site_tp_cd == "ST") %>%
    # keeps only sites that have data since the start of the storm
    # if a gage goes out during the storm, this filter would still capture that gage
    # also filter out sites that weren't up before the start of the storm (e.g., we are GIF'ing a historical storm)
    dplyr::filter(end_date >= as.Date(dates$start), begin_date <= as.Date(dates$start))

  # Only keep the columns we need
  sites <- dplyr::select(sites_filtered, site_no, station_nm, dec_lat_va, dec_long_va, flood_stage) %>%
    # reduce to distinct sites (why are there duplicates?? but there are)
    distinct()

  # Write the data file and the indicator file
  data_file <- as_data_file(ind_file)
  saveRDS(sites, data_file)
  gd_put(ind_file, data_file)
}

fetch_rapid_dep_sites <- function(ind_file, event) {

  if(is.null(event)) {
    rdgs <- data.frame()
  } else {
    # fetch the rapid deployment gages (RDGs) from the STN web services
    url <- sprintf('https://stn.wim.usgs.gov/STNServices/Instruments/FilteredInstruments.json?Event=%d&EventType=&EventStatus=&States=&County=&CurrentStatus=&CollectionCondition=&SensorType=1&DeploymentType=1,2', event)

    rdgs <- jsonlite::fromJSON(url)
    if(length(rdgs) > 0) {
      rdgs <- rdgs %>%
        select(sensorType, eventName, timeStamp, site_no, latitude, longitude, siteDescription)
    }
  }

  # if we wanted to look up data for these sites, we could go to NWIS by using
  # this sites table to map the RDG ID (site_no) to an NWIS id (usgs_sid):
  # https://stn.wim.usgs.gov/STNServices/Events/283/sites

  # Write the data file and the indicator file
  saveRDS(rdgs, as_data_file(ind_file))
  gd_put(ind_file)
}
