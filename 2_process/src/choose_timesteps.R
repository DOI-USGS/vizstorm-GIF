choose_timesteps <- function(ind_file, dates) {
  # temporary quick version of creating timesteps - later we should also look to
  timesteps <- seq(as.POSIXct(dates$start, tz='UTC'), as.POSIXct(dates$end, tz='UTC'), by=as.difftime(1, units='hours'))
  data_file <- as_data_file(ind_file)
  saveRDS(timesteps, data_file)
  gd_put(ind_file, data_file)
}
