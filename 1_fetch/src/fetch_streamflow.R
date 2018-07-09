fetch_streamflow <- function(ind_file, sites, dates, stream_params, gd_config) {

  scipiper::sc_retrieve(sites)
  sites <- feather::read_feather(scipiper::as_data_file(sites))

  message('  starting download of NWIS flow data at ', Sys.time())
  flow <- readNWISuv(
    siteNumbers=sites$site_no, parameterCd=stream_params$flow,
    startDate=dates$dates$start, endDate=dates$dates$end)

  data_file <- as_data_file(ind_file)
  saveRDS(flow, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
