fetch_streamdata <- function(ind_file, sites_ind, dates, stream_params, gd_config) {

  sites <- readRDS(scipiper::sc_retrieve(sites_ind, remake_file = getOption("scipiper.remake_file")))

  message('  starting download of NWIS data at ', Sys.time())
  # pad the date range by 1 day in each direction because our dates parameter
  # specifies dates in UTC but readNWISuv queries dates in local time - if we
  # didn't do this padding, then we'd cut off a few hours at the beginning
  # relative to the precip data we've retrieved. The padding on the end date is
  # probably gratuitous, but it also doesn't hurt.
  dates_with_buffer <- list(
    start = (as.POSIXct(dates$start, tz='UTC') - as.difftime(1, units='days')) %>% format('%Y-%m-%d'),
    end = (as.POSIXct(dates$end, tz='UTC') + as.difftime(1, units='days')) %>% format('%Y-%m-%d')
  )
  data <- readNWISuv(
    siteNumbers=sites$site_no, parameterCd=stream_params$stage,
    startDate=dates_with_buffer$start, endDate=dates_with_buffer$end)
  data_filt <- data %>%
    filter(dateTime >= as.POSIXct(dates$start, tz='UTC'),
           dateTime <= as.POSIXct(dates$end, tz='UTC'))

  data_file <- as_data_file(ind_file)
  saveRDS(data, data_file)
  gd_put(remote_ind = ind_file, local_source = data_file, config_file = gd_config)
}
