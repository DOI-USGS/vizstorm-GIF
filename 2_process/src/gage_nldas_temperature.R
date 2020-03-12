#' Download air temperature time series from NLDAS for each gage
#'
#' @param dates dates field from viz config
#' @param filtered_sites_ind gages filtered down to custom list
#' @param ind_file output indicator file
#'

get_gage_nldas_temperature <- function(dates, filtered_sites_ind, ind_file) {
  sites_df <- readRDS(sc_retrieve(filtered_sites_ind)) %>%
    st_transform(4326)
  sites_coordinates <- st_coordinates(sites_df) %>% as_tibble()
  all_sites_data <- tibble()
  for(i in seq_along(sites_df$site_no)) {
    #note that the space between the lon/lat values is required by the web service
    url <- sprintf("https://hydro1.gesdisc.eosdis.nasa.gov/daac-bin/access/timeseries.cgi?variable=NLDAS:NLDAS_FORA0125_H.002:TMP2m&location=GEOM:POINT(%f, %f)&startDate=%sT%02d&endDate=%sT%02d&type=netcdf",
                    sites_coordinates$X[i], sites_coordinates$Y[i],
                    as.Date(dates$start), lubridate::hour(dates$start),
                   as.Date(dates$end), lubridate::hour(dates$start))
    nc_file_name <- sprintf("1_fetch/tmp/%s.nc", sites_df$site_no[i])
    download.file(url, destfile = nc_file_name,
                  method = "wget")
    site_data_nc <- ncdf4::nc_open(nc_file_name)
    tmp <- ncdf4::ncvar_get(site_data_nc, varid = "TMP2m")
    dates_numeric <- ncdf4::ncvar_get(site_data_nc, varid = "time")
    site_data_df <- tibble(temp_k = tmp,
                           dateTime = dates_numeric) %>%
      mutate(temp_f = (temp_k - 273.15)*9/5 + 32,
              dateTime = as.POSIXct(dateTime, origin = "1970-01-01 00:00:00",
                                 tz = "UTC"),
             site_no = sites_df$site_no[i])
    all_sites_data <- bind_rows(all_sites_data, site_data_df)
  }
  saveRDS(all_sites_data, file = as_data_file(ind_file))
  gd_put(remote_ind = ind_file)
}
