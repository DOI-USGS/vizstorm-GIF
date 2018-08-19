choose_timesteps <- function(ind_file, dates) {
  timesteps <- seq(as.POSIXct(dates$start, tz = "UTC"), as.POSIXct(dates$end, tz = "UTC"), by = 'hours')
  data_file <- as_data_file(ind_file)
  saveRDS(timesteps, data_file)
  gd_put(ind_file, data_file)
}

# choose_timesteps <- function(ind_file, precip_rasters_ind) {
#
#   precip_rasters <- readRDS(sc_retrieve(precip_rasters_ind))
#
#   timesteps <- as.POSIXct(names(precip_rasters), tz='UTC')
#
#   data_file <- as_data_file(ind_file)
#   saveRDS(timesteps, data_file)
#   gd_put(ind_file, data_file)
# }
