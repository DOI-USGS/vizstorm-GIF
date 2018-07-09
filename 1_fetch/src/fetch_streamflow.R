fetch_streamflow <- function(ind_file, sites_yml, dates_yml, stream_params_yml, gd_config) {

  dates <- yaml::yaml.load_file(dates_yml) # these dates are user input right now but should be dynamic based on storm
  pcodes <- yaml::yaml.load_file(stream_params_yml)
  site_ids <- yaml::yaml.load_file(sites_yml)

  message('  starting download of NWIS flow data at ', Sys.time())
  flow <- readNWISuv(
    siteNumbers=site_ids, parameterCd=pcodes$flow,
    startDate=dates$start, endDate=dates$end) %>%
    mutate(parm_cd = '00060') %>%
    rename(result_va=X_00060_00000, remark_cd=X_00060_00000_cd)

  data_file <- as_data_file(ind_file)
  saveRDS(flow, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
