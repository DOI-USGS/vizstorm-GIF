fetch_streamdata <- function(ind_file, sites, dates, stream_params, gd_config) {

  scipiper::sc_retrieve(sites)
  sites <- feather::read_feather(scipiper::as_data_file(sites))

  message('  starting download of NWIS data at ', Sys.time())
  data <- readNWISuv(
    siteNumbers=sites$site_no, parameterCd=stream_params$stage,
    startDate=dates$start, endDate=dates$end)

  data_file <- as_data_file(ind_file)
  saveRDS(data, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
