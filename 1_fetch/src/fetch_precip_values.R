
fetch_precip_values <- function(ind_file, precip_spatial_ind, dates) {

  nc <- ncdf4::nc_open("https://cida.usgs.gov/thredds/dodsC/stageiv_combined")

  precip_spatial <- readRDS(sc_retrieve(precip_spatial_ind))

  t_vals <- get_time_nc(nc$dim$time)

  start_date <- as.Date(dates$start)
  end_date <- as.Date(dates$end)

  x_inds <- seq(min(precip_spatial$x), max(precip_spatial$x), 1)
  y_inds <- seq(min(precip_spatial$y), max(precip_spatial$y), 1)
  t_inds <- which(t_vals > start_date & t_vals < end_date)

  # ensures we make our request in the right axis order.
  dimid_order <- match(nc$var$Total_precipitation_surface_1_Hour_Accumulation$dimids,
                       c(nc$dim$x$id, nc$dim$y$id, nc$dim$time$id))

  precip <- array(dim = c(length(x_inds), length(y_inds), length((t_inds))))
  t_counter <- 1

  for(step in seq(min(t_inds), max(t_inds), 1)) {
    precip[,,t_counter] <-
      ncdf4::ncvar_get(nc, nc$var$Total_precipitation_surface_1_Hour_Accumulation,
                       start <- c(min(x_inds),
                                  min(y_inds),
                                  step)[dimid_order],
                       count <- c(length(x_inds),
                                  length(y_inds),
                                  1)[dimid_order])

    if (t_counter %% 10 == 0){
      message(paste(t_counter, 'of', length(t_inds)))
    }

    t_counter <- t_counter + 1
  }

  precip <- arrayhelpers::array2df(precip)

  x_inds <- data.frame(d1 = seq_len(length(x_inds)), x = x_inds)
  y_inds <- data.frame(d2 = seq_len(length(y_inds)), y = y_inds)
  t_inds <- data.frame(d3 = seq_len(length(t_inds)), t = t_inds)

  precip <- precip %>%
    left_join(x_inds, by = "d1") %>%
    left_join(y_inds, by = "d2") %>%
    left_join(t_inds, by = "d3") %>%
    inner_join(sf::st_set_geometry(precip_spatial, NULL),
               by = c("x", "y")) %>%
    left_join(data.frame(time = t_vals,
                         t = seq_len(length(t_vals))),
              by = "t") %>%
    dplyr::select(precip, time, id)

  data_file <- as_data_file(ind_file)
  saveRDS(precip, data_file)
  gd_put(remote_ind=ind_file, local_source=data_file, mock_get='none')
}

get_time_nc <- function(t_dim) {
  time_units<-strsplit(t_dim$units, " ")[[1]]
  time_step<-time_units[1]
  date_origin<-time_units[3]

  if(grepl("hour", time_step, ignore.case = TRUE)) {
    multiplier <- (60^2)
  } else {
    stop("only hour time steps supported so far")
  }

  origin <- as.POSIXct(strptime(date_origin,
                                format = "%Y-%m-%dT%H:%M:%SZ",
                                tz = "UTC"),
                       tz = "UTC")

  as.POSIXct(t_dim$vals * multiplier,
             origin = origin,
             tz = "UTC")
}

